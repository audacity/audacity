/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_RECORDCONTROLLER_H
#define AU_RECORD_RECORDCONTROLLER_H

#include "framework/global/async/asyncable.h"
#include "framework/actions/actionable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/global/iinteractive.h"
#include "playback/iplaybackcontroller.h"
#include "record/irecordconfiguration.h"

#include "record/irecord.h"
#include "record/irecordcontroller.h"

namespace au::record {
class RecordController : public IRecordController, public muse::actions::Actionable, public muse::async::Asyncable, public muse::Injectable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<IRecord> record;
    muse::Inject<playback::IPlaybackController> playbackController;
    muse::Inject<record::IRecordConfiguration> configuration;

public:
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
