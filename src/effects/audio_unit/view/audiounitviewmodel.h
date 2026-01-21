/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <AudioToolbox/AudioUnitUtilities.h>

#include <QObject>

#include "effects/effects_base/ieffectsprovider.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "trackedit/iprojecthistory.h"

#include "effects/effects_base/view/abstracteffectviewmodel.h"
#include "effects/effects_base/irealtimeeffectservice.h"

#include "au3-audio-unit/AudioUnitInstance.h"

namespace au::effects {
class AudioUnitViewModel : public AbstractEffectViewModel
{
    Q_OBJECT
    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)

    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<IRealtimeEffectService> realtimeEffectService{ this };
    muse::Inject<au::playback::IPlayback> playback{ this };
    muse::Inject<trackedit::IProjectHistory> projectHistory{ this };
    muse::Inject<IEffectsProvider> effectsProvider{ this };

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
    static void EventListenerCallback(void* inCallbackRefCon, void* inObject, const AudioUnitEvent* inEvent, UInt64 inEventHostTime,
                                      AudioUnitParameterValue inParameterValue);
    void EventListener(const AudioUnitEvent* inEvent, AudioUnitParameterValue inParameterValue);
    EventListenerPtr MakeListener();

    void settingsToView();
    void settingsFromView();
    void checkSettingChangesFromUi();

    std::unordered_map<AudioUnitParameterID, AudioUnitParameterValue> m_parameterValues;
    std::vector<std::pair<AudioUnitParameterID, AudioUnitParameterValue> > m_toUpdate;

    std::shared_ptr<AudioUnitInstance> m_instance;
    EffectSettingsAccessPtr m_settingsAccess;
    EventListenerPtr m_eventListenerRef;

    QString m_title;
    QTimer m_settingsTimer;
};

class AudioUnitViewModelFactory : public EffectViewModelFactory<AudioUnitViewModel>
{
};
}
