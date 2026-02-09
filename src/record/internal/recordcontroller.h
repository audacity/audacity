/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_RECORDCONTROLLER_H
#define AU_RECORD_RECORDCONTROLLER_H

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iinteractive.h"

#include "context/iglobalcontext.h"
#include "playback/iplaybackcontroller.h"
#include "record/irecordconfiguration.h"

#include "record/irecord.h"
#include "record/irecordcontroller.h"

namespace au::record {
class RecordController : public IRecordController, public muse::actions::Actionable, public muse::async::Asyncable, public muse::Injectable
{
    muse::GlobalInject<record::IRecordConfiguration> configuration;

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<muse::IInteractive> interactive{ this };
    muse::Inject<IRecord> record{ this };
    muse::Inject<playback::IPlaybackController> playbackController{ this };

public:
    RecordController(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void init();
    void deinit();

    bool isRecordAllowed() const override;
    muse::async::Notification isRecordAllowedChanged() const override;

    bool isRecording() const override;
    muse::async::Notification isRecordingChanged() const override;

    muse::secs_t recordPosition() const override;
    muse::async::Channel<muse::secs_t> recordPositionChanged() const override;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

    bool isMicMeteringOn() const override;
    muse::async::Notification isMicMeteringOnChanged() const override;

    bool isInputMonitoringOn() const override;
    muse::async::Notification isInputMonitoringOnChanged() const override;

private:
    enum class RecordStatus {
        Stopped = 0,
        Paused,
        Running
    };

    void onProjectChanged();

    void toggleRecord();
    void start();
    void pause();
    void stop();
    void toggleMicMetering();
    void toggleInputMonitoring();

    void setCurrentRecordStatus(RecordStatus status);

    muse::async::Notification m_isRecordingChanged;
    muse::async::Notification m_isRecordAllowedChanged;

    RecordStatus m_currentRecordStatus = RecordStatus::Stopped;

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
};
}

#endif // AU_RECORD_RECORDCONTROLLER_H
