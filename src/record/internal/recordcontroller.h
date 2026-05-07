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
#include "trackedit/iselectioncontroller.h"
#include "record/irecordconfiguration.h"

#include "record/irecord.h"
#include "record/irecordcontroller.h"

namespace au::record {
class RecordController : public IRecordController, public muse::actions::Actionable, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<record::IRecordConfiguration> configuration;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<IRecord> record{ this };
    muse::ContextInject<playback::IPlaybackController> playbackController{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };

public:
    RecordController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    void deinit();

    bool isRecordAllowed() const override;
    muse::async::Notification isRecordAllowedChanged() const override;

    bool isRecording() const override;
    muse::async::Notification isRecordingChanged() const override;

    const std::vector<trackedit::ClipKey>& recordingClipKeys() const override;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

    bool isMicMeteringOn() const override;
    muse::async::Notification isMicMeteringOnChanged() const override;

    bool isInputMonitoringOn() const override;
    muse::async::Notification isInputMonitoringOnChanged() const override;

    bool isLeadInRecording() const override;
    muse::async::Notification isLeadInRecordingChanged() const override;
    muse::secs_t leadInRecordingStartTime() const override;
    std::vector<trackedit::TrackId> leadInRecordingTrackIds() const override;

private:
    enum class RecordStatus {
        Stopped = 0,
        Paused,
        LeadIn,
        Running
    };

    void onProjectChanged();

    void toggleRecord();
    void start();
    void pause();
    void stop();
    void leadInRecording();
    void toggleMicMetering();
    void toggleInputMonitoring();

    void setCurrentRecordStatus(RecordStatus status);

    muse::async::Notification m_isRecordingChanged;
    muse::async::Notification m_isRecordAllowedChanged;

    RecordStatus m_currentRecordStatus = RecordStatus::Stopped;

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;

    muse::secs_t m_leadInRecordingStartTime = 0.0;
    std::vector<trackedit::TrackId> m_leadInRecordingTrackIds;
};
}

#endif // AU_RECORD_RECORDCONTROLLER_H
