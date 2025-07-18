/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <AudioToolbox/AudioUnitUtilities.h>

#include <QObject>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "trackedit/iprojecthistory.h"
#include "global/async/asyncable.h"

#include "effects/effects_base/ieffectinstancesregister.h"
#include "effects/effects_base/ieffectexecutionscenario.h"
#include "effects/effects_base/irealtimeeffectservice.h"

#include "libraries/lib-audio-unit/AudioUnitInstance.h"

namespace au::effects {
class AudioUnitViewModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IRealtimeEffectService> realtimeEffectService;
    muse::Inject<IEffectExecutionScenario> executionScenario;
    muse::Inject<au::playback::IPlayback> playback;
    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    AudioUnitViewModel(QObject* parent = nullptr);
    ~AudioUnitViewModel() override;

    Q_INVOKABLE void init();
    Q_INVOKABLE void preview();
    Q_INVOKABLE void deinit();

    int instanceId() const;
    void setInstanceId(int newInstanceId);

    QString title() const;
    void setTitle(const QString& newTitle);

signals:
    void instanceIdChanged();
    void titleChanged();

private:
    using EventListenerPtr = AudioUnitCleanup<AUEventListenerRef, AUListenerDispose>;
    static void EventListenerCallback(void* inCallbackRefCon, void* inObject, const AudioUnitEvent* inEvent, UInt64 inEventHostTime,
                                      AudioUnitParameterValue inParameterValue);
    void EventListener(const AudioUnitEvent* inEvent, AudioUnitParameterValue inParameterValue);
    EventListenerPtr MakeListener();

    void settingsToView();
    void settingsFromView();
    void checkSettingChangesFromUi();

    std::unordered_map<AudioUnitParameterID, AudioUnitParameterValue> m_parameterValues;
    std::vector<std::pair<AudioUnitParameterID, AudioUnitParameterValue> > m_toUpdate;

    EffectInstanceId m_instanceId = -1;
    std::shared_ptr<AudioUnitInstance> m_instance;
    EffectSettingsAccessPtr m_settingsAccess;
    EventListenerPtr m_eventListenerRef;

    QString m_title;
    QTimer m_settingsTimer;
};
}
