/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <AudioToolbox/AudioUnitUtilities.h>

#include <QObject>
#include <QTimer>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "trackedit/iprojecthistory.h"

#include "effects/effects_base/view/abstracteffectviewmodel.h"
#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effects/effects_base/irealtimeeffectservice.h"

#include "au3-audio-unit/AudioUnitInstance.h"

namespace au::effects {
class AudioUnitViewModel : public AbstractEffectViewModel
{
    Q_OBJECT
    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)

    muse::GlobalInject<IEffectInstancesRegister> instancesRegister;
    muse::GlobalInject<IEffectsProvider> effectsProvider;

    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<IEffectExecutionScenario> executionScenario{ this };
    muse::ContextInject<IRealtimeEffectService> realtimeEffectService{ this };
    muse::ContextInject<au::playback::IPlayback> playback{ this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory{ this };

public:
    AudioUnitViewModel(QObject* parent, int instanceId);
    ~AudioUnitViewModel() override;

    Q_INVOKABLE void deinit();

    QString title() const;
    void setTitle(const QString& newTitle);

signals:
    void titleChanged();

private:
    void doInit() override;
    void doStartPreview() override;
    void doStopPreview() override;

    using EventListenerPtr = AudioUnitCleanup<AUEventListenerRef, AUListenerDispose>;

    struct EventListenerContext {
        AudioUnitViewModel* viewModel = nullptr;
        std::weak_ptr<EventListenerContext> self;
    };

    static void EventListenerCallback(void* inCallbackRefCon, void* inObject, const AudioUnitEvent* inEvent, UInt64 inEventHostTime,
                                      AudioUnitParameterValue inParameterValue);
    static void DisposeListenerAsync(AUEventListenerRef listener, std::shared_ptr<EventListenerContext> context,
                                     std::shared_ptr<AudioUnitInstance> instance);
    void EventListener(const AudioUnitEvent* inEvent, AudioUnitParameterValue inParameterValue);
    EventListenerPtr MakeListener();
    void fetchSettingsAsync();

    void settingsToView();
    void settingsFromView();
    void checkSettingChangesFromUi();

    std::unordered_map<AudioUnitParameterID, AudioUnitParameterValue> m_parameterValues;
    std::vector<std::pair<AudioUnitParameterID, AudioUnitParameterValue> > m_toUpdate;

    std::shared_ptr<AudioUnitInstance> m_instance;
    EffectSettingsAccessPtr m_settingsAccess;
    EventListenerPtr m_eventListenerRef;
    std::shared_ptr<EventListenerContext> m_listenerContext;
    bool m_settingsFetchPending = false;
    bool m_settingsFetchQueued = false;

    QString m_title;
    QTimer m_settingsTimer;
};

class AudioUnitViewModelFactory : public EffectViewModelFactory<AudioUnitViewModel>
{
};
}
