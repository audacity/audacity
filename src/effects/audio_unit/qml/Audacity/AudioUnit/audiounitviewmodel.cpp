/*
 * Audacity: A Digital Audio Editor
 */
#include "audiounitviewmodel.h"

#include <future>
#include <map>
#include <memory>
#include <optional>
#include <thread>
#include <utility>

#include <AudioToolbox/AudioUnitUtilities.h>
#include <CoreFoundation/CoreFoundation.h>
#include <dispatch/dispatch.h>
#include <pthread.h>

#include "au3-effects/EffectManager.h"
#include "au3-effects/Effect.h"
#include "au3-realtime-effects/RealtimeEffectState.h"
#include "au3-audio-unit/AudioUnitInstance.h"
#include "au3-audio-unit/AudioUnitUtils.h"
#include "au3-audio-unit/AudioUnitEffectBase.h"

#include "log.h"

namespace au::effects {
namespace {
//! Dedicated thread on whose run loop AudioUnit event listeners live.
//! Parameter notifications are delivered and the listeners are disposed
//! there, so the main thread is never part of the listener's lifecycle:
//! disposing a listener away from its run loop leaves its delivery timer
//! scheduled and firing into an empty callback, while disposing on the
//! main thread deadlocks when an in-flight notification holds the listener
//! lock during a blocking request to the plugin's hosting service.
CFRunLoopRef auEventListenerRunLoop()
{
    static const CFRunLoopRef runLoop = [] {
        std::promise<CFRunLoopRef> promise;
        std::thread thread([&promise] {
            pthread_setname_np("AudioUnitEventListener");

            //! NOTE An unsignaled source keeps the run loop from returning
            CFRunLoopSourceContext sourceContext {};
            CFRunLoopSourceRef source = CFRunLoopSourceCreate(kCFAllocatorDefault, 0, &sourceContext);
            CFRunLoopAddSource(CFRunLoopGetCurrent(), source, kCFRunLoopDefaultMode);
            CFRelease(source);

            promise.set_value(CFRunLoopGetCurrent());
            CFRunLoopRun();
        });
        thread.detach();
        return promise.get_future().get();
    }();

    return runLoop;
}
}

AudioUnitViewModel::AudioUnitViewModel(QObject* parent, int instanceId)
    : AbstractEffectViewModel(parent, instanceId)
{}

AudioUnitViewModel::~AudioUnitViewModel()
{
    checkSettingChangesFromUi();
    deinit();
}

void AudioUnitViewModel::doInit()
{
    IF_ASSERT_FAILED(instanceId() >= 0) {
        return;
    }

    m_instance = std::dynamic_pointer_cast<AudioUnitInstance>(instancesRegister()->instanceById(instanceId()));
    IF_ASSERT_FAILED(m_instance) {
        return;
    }

    instancesRegister()->settingsChanged(instanceId()).onNotify(this, [this]() {
        settingsToView();
    });
    instancesRegister()->updateSettingsRequested(instanceId()).onNotify(this, [this]() {
        settingsFromView();
    });
    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]() {
        settingsToView();
    });

    m_eventListenerRef = MakeListener();

    const EffectSettings* settings = instancesRegister()->settingsById(instanceId());
    IF_ASSERT_FAILED(settings) {
        return;
    }

    const EffectId id = instancesRegister()->effectIdByInstanceId(instanceId());
    const AudioUnitEffectBase* const effect = dynamic_cast<AudioUnitEffectBase*>(effectsProvider()->effect(id));

    IF_ASSERT_FAILED(effect) {
        return;
    }

    m_settingsAccess = instancesRegister()->settingsAccessById(instanceId());
    IF_ASSERT_FAILED(m_settingsAccess) {
        return;
    }

    if (!m_instance->IsInitialized()) {
        settingsToView();
    }

    setTitle(effect->GetSymbol().Msgid().translated());
    connect(&m_settingsTimer, &QTimer::timeout, this, [this]() {
        checkSettingChangesFromUi();
    });
    // Transfers ui changes to the effect settings
    // TODO: make this centralized across all effects
    m_settingsTimer.start(100);
}

void au::effects::AudioUnitViewModel::doStartPreview()
{
    IF_ASSERT_FAILED(m_settingsAccess) {
        return;
    }

    settingsFromView();

    m_settingsAccess->ModifySettings([this](EffectSettings& settings) {
        executionScenario()->previewEffect(instanceId(), settings);
        return nullptr;
    });
}

void au::effects::AudioUnitViewModel::doStopPreview()
{
    executionScenario()->stopPreview();
}

void AudioUnitViewModel::DisposeListenerAsync(AUEventListenerRef listener, std::shared_ptr<EventListenerContext> context,
                                              std::shared_ptr<AudioUnitInstance> instance)
{
    if (context) {
        context->viewModel = nullptr;
    }

    if (!listener) {
        return;
    }

    // Dispose on the listener run loop, where the listener and its delivery
    // timer live. If a notification is in flight, only that thread waits for
    // it; the main thread stays available. The context stays alive until the
    // dispose runs; a callback arriving in the meantime sees a null viewModel
    // and does nothing.
    //
    // The instance is held until the dispose completes: AudioComponentInstance
    // Dispose spins until the instance has no live listeners, so releasing the
    // instance only after AUListenerDispose keeps the component teardown from
    // overlapping the listener teardown - overlapping them deadlocks the
    // out-of-process hosting service.
    CFRunLoopRef runLoop = auEventListenerRunLoop();
    CFRunLoopPerformBlock(runLoop, kCFRunLoopDefaultMode, ^ {
            AUListenerDispose(listener);

            // Hand the strong references back to the main thread. Dropping the
            // last one here would destroy the instance on this thread, and its
            // destruction reaches state that only the main thread may touch.
            dispatch_async(dispatch_get_main_queue(), ^ {
                (void)context;
                (void)instance;
            });
        });
    CFRunLoopWakeUp(runLoop);
}

void AudioUnitViewModel::deinit()
{
    assert(CFRunLoopGetCurrent() == CFRunLoopGetMain());

    DisposeListenerAsync(m_eventListenerRef.release(), std::move(m_listenerContext), m_instance);
}

void au::effects::AudioUnitViewModel::settingsToView()
{
    IF_ASSERT_FAILED(m_instance && m_settingsAccess) {
        return;
    }

    if (!m_instance->IsInitialized()) {
        m_instance->Initialize();
    }

    m_instance->StoreSettings(m_instance->mProcessor, AudioUnitInstance::GetSettings(m_settingsAccess->Get()));

    AudioUnitParameter aup = {};
    aup.mAudioUnit = m_instance->GetAudioUnit();
    aup.mParameterID = kAUParameterListener_AnyParameter;
    aup.mScope = kAudioUnitScope_Global;
    aup.mElement = 0;
    AUParameterListenerNotify(m_eventListenerRef.get(), nullptr, &aup);
}

void au::effects::AudioUnitViewModel::settingsFromView()
{
    IF_ASSERT_FAILED(m_instance && m_settingsAccess) {
        return;
    }

    m_settingsAccess->ModifySettings([this](EffectSettings& settings) {
        m_instance->FetchSettings(AudioUnitInstance::GetSettings(settings), true, true);
        return nullptr;
    });
}

void au::effects::AudioUnitViewModel::checkSettingChangesFromUi()
{
    if (m_toUpdate.size()) {
        m_settingsAccess->ModifySettings([&](EffectSettings& settings){
            auto& mySettings = AudioUnitInstance::GetSettings(settings);
            for (auto [ID, value] : m_toUpdate) {
                auto& pair = mySettings.values[ID];
                if (pair.has_value()) {
                    pair->second = value;
                } else {
                    pair.emplace(mySettings.Intern(wxString {}), value);
                }
            }

            m_instance->StoreSettings(m_instance->mProcessor, mySettings);

            //! NOTE A message marks the settings as changed for the realtime
            //! state; without it the change is not carried over to the stored
            //! settings when processing is not running, and the effect falls
            //! back to its previous values the next time it is initialized
            return m_instance->MakeMessage();
        });
        m_toUpdate.clear();
        m_settingsAccess->Flush();
    }
}

void AudioUnitViewModel::EventListenerCallback(void* inCallbackRefCon, void* inObject, const AudioUnitEvent* inEvent,
                                               UInt64 inEventHostTime, AudioUnitParameterValue inParameterValue)
{
    UNUSED(inObject);
    UNUSED(inEventHostTime);

    // Called on the listener run loop; the view model lives on the main
    // thread, so hand the event over
    auto* rawContext = static_cast<EventListenerContext*>(inCallbackRefCon);
    std::shared_ptr<EventListenerContext> context = rawContext->self.lock();
    if (!context) {
        return;
    }

    const AudioUnitEvent event = *inEvent;
    dispatch_async(dispatch_get_main_queue(), ^ {
            if (context->viewModel) {
                context->viewModel->EventListener(&event, inParameterValue);
            }
        });
}

void au::effects::AudioUnitViewModel::EventListener(const AudioUnitEvent* inEvent, AudioUnitParameterValue inParameterValue)
{
    if (!globalContext()->currentProject()) {
        return;
    }

    if (inEvent->mEventType == kAudioUnitEvent_ParameterValueChange) {
        constexpr AudioUnitParameterValue epsilon = 1e-6;
        const auto ID = inEvent->mArgument.mParameter.mParameterID;

        auto it = m_parameterValues.find(ID);
        // When the UI is opened - EventListener is called for each parameter
        // with the current value.
        if (it == m_parameterValues.end()) {
            m_parameterValues.insert(std::make_pair(ID, inParameterValue));
            return;
        }

        if (std::abs(it->second - inParameterValue) <= epsilon) {
            return;
        }
        it->second = inParameterValue;

        m_instance->EventListener(inEvent, inParameterValue);
        m_toUpdate.emplace_back(ID, inParameterValue);
        projectHistory()->modifyState();
        projectHistory()->markUnsaved();
    } else if (inEvent->mEventType == kAudioUnitEvent_PropertyChange
               && inEvent->mArgument.mProperty.mPropertyID == kAudioUnitProperty_PresentPreset) {
        fetchSettingsAsync();
    }
}

void au::effects::AudioUnitViewModel::fetchSettingsAsync()
{
    IF_ASSERT_FAILED(m_instance && m_settingsAccess && m_listenerContext) {
        return;
    }

    if (m_settingsFetchPending) {
        m_settingsFetchQueued = true;
        return;
    }
    m_settingsFetchPending = true;

    // For an out-of-process plugin, reading the preset and every parameter
    // value is a series of blocking requests to the hosting service, which
    // may itself be waiting for this thread. Do the reads on a background
    // queue and only write the result into the settings back on the main
    // thread, where the shared name set may be safely modified.
    std::shared_ptr<EventListenerContext> context = m_listenerContext;
    std::shared_ptr<AudioUnitInstance> instance = m_instance;

    dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^ {
            AUPreset preset {};
            std::optional<SInt32> presetNumber;
            if (!instance->GetFixedSizeProperty(kAudioUnitProperty_PresentPreset, preset)
                && preset.presetNumber >= 0) {
                presetNumber = preset.presetNumber;
            }

            using FetchedValues = std::map<AudioUnitParameterID, std::optional<std::pair<wxString, AudioUnitParameterValue> > >;
            auto values = std::make_shared<FetchedValues>();
            instance->ForEachParameter(
                [&](const AudioUnitWrapper::ParameterInfo& pi, AudioUnitParameterID ID) {
            auto& slot = (*values)[ID];
            AudioUnitParameterValue value;
            if (pi.mName
                && !AudioUnitGetParameter(instance->GetAudioUnit(), ID, kAudioUnitScope_Global, 0, &value)) {
                slot.emplace(*pi.mName, value);
            }
            return true;
        });

            dispatch_async(dispatch_get_main_queue(), ^ {
                // Capture the instance so its last reference is released on the
                // main thread rather than on the queue this fetch ran on.
                (void)instance;

                AudioUnitViewModel* viewModel = context->viewModel;
                if (!viewModel) {
                    return;
                }

                viewModel->m_settingsFetchPending = false;

                // Another preset change arrived while this fetch was running:
                // the values read above may be stale, so discard them and
                // fetch again.
                if (viewModel->m_settingsFetchQueued) {
                    viewModel->m_settingsFetchQueued = false;
                    viewModel->fetchSettingsAsync();
                    return;
                }

                viewModel->m_settingsAccess->ModifySettings([&](EffectSettings& settings) {
                auto& mySettings = AudioUnitInstance::GetSettings(settings);
                if (presetNumber) {
                    mySettings.mPresetNumber = presetNumber;
                }
                for (const auto& [ID, fetched] : *values) {
                    auto& slot = mySettings.values[ID];
                    slot.reset();
                    if (fetched) {
                        slot.emplace(mySettings.Intern(fetched->first), fetched->second);
                    }
                }
                return nullptr;
            });
            });
        });
}

au::effects::AudioUnitViewModel::EventListenerPtr au::effects::AudioUnitViewModel::MakeListener()
{
    const auto unit = m_instance->GetAudioUnit();
    EventListenerPtr result;

    // Register a callback with the audio unit
    m_listenerContext = std::make_shared<EventListenerContext>();
    m_listenerContext->viewModel = this;
    m_listenerContext->self = m_listenerContext;
    AUEventListenerRef eventListenerRef{};
    if (AUEventListenerCreate(AudioUnitViewModel::EventListenerCallback, m_listenerContext.get(), auEventListenerRunLoop(),
                              kCFRunLoopDefaultMode, 0.0, 0.0, &eventListenerRef)) {
        DisposeListenerAsync(nullptr, std::move(m_listenerContext), m_instance);
        return nullptr;
    }

    result.reset(eventListenerRef);

    // AudioUnitEvent is a struct with a discriminator field and a union
    AudioUnitEvent event;
    event.mEventType = kAudioUnitEvent_ParameterValueChange;
    // Initialize union member -- the ID (second field) reassigned later
    auto& parameter = event.mArgument.mParameter;
    parameter = AudioUnitUtils::Parameter{ unit, kAudioUnitScope_Global };

    // Register each parameter as something we're interested in, seeding the
    // cache with its current value so a genuine first change is propagated
    // instead of being mistaken for the AU's open-time notification and dropped.
    if (auto& parameters = m_instance->GetParameters()) {
        for (const auto& ID : parameters) {
            parameter.mParameterID = ID;
            if (AUEventListenerAddEventType(result.get(), this, &event)) {
                DisposeListenerAsync(result.release(), std::move(m_listenerContext), m_instance);
                return nullptr;
            }
            AudioUnitParameterValue value;
            if (!AudioUnitGetParameter(unit, ID, kAudioUnitScope_Global, 0, &value)) {
                m_parameterValues.insert_or_assign(ID, value);
            }
        }
    }

    // Now set up the other union member
    event.mEventType = kAudioUnitEvent_PropertyChange;
    // And bind the listener function to certain property changes
    for (auto type : {
            kAudioUnitProperty_Latency,
            kAudioUnitProperty_PresentPreset,
        }) {
        event.mArgument.mProperty = AudioUnitUtils::Property{
            unit, type, kAudioUnitScope_Global };
        if (AUEventListenerAddEventType(result.get(), this, &event)) {
            DisposeListenerAsync(result.release(), std::move(m_listenerContext), m_instance);
            return nullptr;
        }
    }

    return result;
}

QString AudioUnitViewModel::title() const
{
    return m_title;
}

void AudioUnitViewModel::setTitle(const QString& newTitle)
{
    if (m_title == newTitle) {
        return;
    }
    m_title = newTitle;
    emit titleChanged();
}
}
