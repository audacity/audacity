/*
 * Audacity: A Digital Audio Editor
 */
#include "audiounitviewmodel.h"

#include <AudioToolbox/AudioUnitUtilities.h>
#include <CoreFoundation/CoreFoundation.h>

#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-audio-unit/AudioUnitInstance.h"
#include "libraries/lib-audio-unit/AudioUnitUtils.h"
#include "libraries/lib-audio-unit/AudioUnitEffectBase.h"

#include "log.h"

namespace au::effects {
AudioUnitViewModel::AudioUnitViewModel(QObject* parent)
    : QObject(parent)
{}

AudioUnitViewModel::~AudioUnitViewModel()
{
    checkSettingChangesFromUi();
}

void AudioUnitViewModel::init()
{
    IF_ASSERT_FAILED(m_instanceId >= 0) {
        return;
    }

    m_instance = std::dynamic_pointer_cast<AudioUnitInstance>(instancesRegister()->instanceById(m_instanceId));
    IF_ASSERT_FAILED(m_instance) {
        return;
    }

    instancesRegister()->settingsChanged(m_instanceId).onNotify(this, [this]() {
        settingsToView();
    });
    instancesRegister()->updateSettingsRequested(m_instanceId).onNotify(this, [this]() {
        settingsFromView();
    });
    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]() {
        settingsToView();
    });

    m_eventListenerRef = MakeListener();

    const EffectSettings* settings = instancesRegister()->settingsById(m_instanceId);
    IF_ASSERT_FAILED(settings) {
        return;
    }

    const EffectId id = instancesRegister()->effectIdByInstanceId(m_instanceId);
    const AudioUnitEffectBase* const effect = dynamic_cast<AudioUnitEffectBase*>(EffectManager::Get().GetEffect(id.toStdString()));

    IF_ASSERT_FAILED(effect) {
        return;
    }

    m_settingsAccess = instancesRegister()->settingsAccessById(m_instanceId);
    IF_ASSERT_FAILED(m_settingsAccess) {
        return;
    }

    settingsToView();

    setTitle(QString::fromStdString(effect->GetSymbol().Translation().ToStdString()));
    connect(&m_settingsTimer, &QTimer::timeout, this, [this]() {
        checkSettingChangesFromUi();
    });
    // Transfers ui changes to the effect settings
    // TODO: make this centralized across all effects
    m_settingsTimer.start(100);
}

void au::effects::AudioUnitViewModel::preview()
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

void AudioUnitViewModel::deinit()
{
    if (m_eventListenerRef) {
        AUListenerDispose(m_eventListenerRef.get());
        m_eventListenerRef.reset();
    }
}

void au::effects::AudioUnitViewModel::settingsToView()
{
    IF_ASSERT_FAILED(m_instance && m_settingsAccess) {
        return;
    }

    if (m_instance->StoreSettings(m_instance->mProcessor, AudioUnitInstance::GetSettings(m_settingsAccess->Get()))) {
        AudioUnitParameter aup = {};
        aup.mAudioUnit = m_instance->GetAudioUnit();
        aup.mParameterID = kAUParameterListener_AnyParameter;
        aup.mScope = kAudioUnitScope_Global;
        aup.mElement = 0;
        AUParameterListenerNotify(NULL, NULL, &aup);
    }
    return;
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
                if (auto& pair = mySettings.values[ID]; pair.has_value()) {
                    pair->second = value;
                }
            }

            m_instance->StoreSettings(m_instance->mProcessor, mySettings);

            return nullptr;
        });
        m_toUpdate.clear();
        m_settingsAccess->Flush();
    }
}

void AudioUnitViewModel::EventListenerCallback(void* inCallbackRefCon, void* inObject, const AudioUnitEvent* inEvent,
                                               UInt64 inEventHostTime, AudioUnitParameterValue inParameterValue)
{
    static_cast<AudioUnitViewModel*>(inCallbackRefCon)->EventListener(inEvent, inParameterValue);
}

void au::effects::AudioUnitViewModel::EventListener(const AudioUnitEvent* inEvent, AudioUnitParameterValue inParameterValue)
{
    if (!globalContext()->currentProject()) {
        return;
    }
    // Modify the instance and its workers
    m_instance->EventListener(inEvent, inParameterValue);
    const auto unit = m_instance->GetAudioUnit();

    if (inEvent->mEventType == kAudioUnitEvent_ParameterValueChange) {
        constexpr AudioUnitParameterValue epsilon = 1e-6;

        auto it = m_parameterValues.find(inEvent->mArgument.mParameter.mParameterID);

        // When the UI is opened - EventListener is called for each parameter
        // with the current value.
        if (it == m_parameterValues.end()) {
            m_parameterValues.insert(std::make_pair(inEvent->mArgument.mParameter.mParameterID, inParameterValue));
        } else if (std::abs(it->second - inParameterValue) > epsilon) {
            it->second = inParameterValue;
            AudioUnitSetParameter(unit, inEvent->mArgument.mParameter.mParameterID, kAudioUnitScope_Global, 0, inParameterValue, 0);
            projectHistory()->modifyState();
            projectHistory()->markUnsaved();
        }

        const auto ID = inEvent->mArgument.mParameter.mParameterID;
        m_toUpdate.emplace_back(ID, inParameterValue);
    } else if (inEvent->mEventType == kAudioUnitEvent_PropertyChange
               && inEvent->mArgument.mProperty.mPropertyID == kAudioUnitProperty_PresentPreset) {
        m_settingsAccess->ModifySettings([this](EffectSettings& settings) {
            m_instance->FetchSettings(AudioUnitInstance::GetSettings(settings), true, true);
            return nullptr;
        });
    }
}

au::effects::AudioUnitViewModel::EventListenerPtr au::effects::AudioUnitViewModel::MakeListener()
{
    const auto unit = m_instance->GetAudioUnit();
    EventListenerPtr result;

    // Register a callback with the audio unit
    AUEventListenerRef eventListenerRef{};
    if (AUEventListenerCreate(AudioUnitViewModel::EventListenerCallback, this, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode, 0.0, 0.0,
                              &eventListenerRef)) {
        return nullptr;
    }

    result.reset(eventListenerRef);

    // AudioUnitEvent is a struct with a discriminator field and a union
    AudioUnitEvent event{ kAudioUnitEvent_ParameterValueChange };
    // Initialize union member -- the ID (second field) reassigned later
    auto& parameter = event.mArgument.mParameter;
    parameter = AudioUnitUtils::Parameter{ unit, kAudioUnitScope_Global };

    // Register each parameter as something we're interested in
    if (auto& parameters = m_instance->GetParameters()) {
        for (const auto& ID : parameters) {
            parameter.mParameterID = ID;
            if (AUEventListenerAddEventType(result.get(), this, &event)) {
                return nullptr;
            }
        }
    }

    // Now set up the other union member
    event = { kAudioUnitEvent_PropertyChange };
    // And bind the listener function to certain property changes
    for (auto type : {
            kAudioUnitProperty_Latency,
            kAudioUnitProperty_PresentPreset,
        }) {
        event.mArgument.mProperty = AudioUnitUtils::Property{
            unit, type, kAudioUnitScope_Global };
        if (AUEventListenerAddEventType(result.get(), this, &event)) {
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

int AudioUnitViewModel::instanceId() const
{
    return m_instanceId;
}

void AudioUnitViewModel::setInstanceId(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}
}
