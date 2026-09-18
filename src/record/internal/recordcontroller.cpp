/*
* Audacity: A Digital Audio Editor
*/
#include "recordcontroller.h"

#include "framework/global/translation.h"
#include "framework/rcommand/actiontocommand.h"

#include "../recordcommands.h"

using namespace muse;
using namespace au::record;
using namespace muse::async;
using namespace muse::actions;
using namespace muse::rcommand;

namespace {
const ActionQuery RECORD_START_QUERY("action://record/start");
const ActionQuery RECORD_PAUSE_QUERY("action://record/pause");
const ActionQuery RECORD_STOP_QUERY("action://record/stop");
const ActionQuery RECORD_TOGGLE_MIC_METERING("action://record/toggle-mic-metering");
const ActionQuery RECORD_TOGGLE_INPUT_MONITORING("action://record/toggle-input-monitoring");
const ActionQuery RECORD_LEAD_IN_RECORDING_QUERY("action://record/lead-in-recording");

const ActionCode RECORD_ON_CURRENT_TRACK_CODE("record-on-current-track");
const ActionCode RECORD_ON_NEW_TRACK_CODE("record-on-new-track");
}

void RecordController::init()
{
    auto cd = commandDispatcher();
    cd->onRequest(this, RECORD_START_COMMAND, [this]() { return toggleRecord(); });
    cd->onRequest(this, RECORD_ON_CURRENT_TRACK_COMMAND, [this]() { return toggleRecord(); });
    cd->onRequest(this, RECORD_ON_NEW_TRACK_COMMAND, [this]() { return recordOnNewTrack(); });
    cd->onRequest(this, RECORD_PAUSE_COMMAND, [this]() { return pause(); });
    cd->onRequest(this, RECORD_STOP_COMMAND, [this]() { return stop(); });
    cd->onRequest(this, RECORD_TOGGLE_MIC_METERING_COMMAND, [this]() { return toggleMicMetering(); });
    cd->onRequest(this, RECORD_TOGGLE_INPUT_MONITORING_COMMAND, [this]() { return toggleInputMonitoring(); });
    cd->onRequest(this, RECORD_LEAD_IN_RECORDING_COMMAND, [this]() { return leadInRecording(); });

    static const std::vector<ActionToCommand> actionToCommand = {
        { RECORD_START_QUERY.toString(), RECORD_START_COMMAND, {} },
        { RECORD_ON_CURRENT_TRACK_CODE, RECORD_ON_CURRENT_TRACK_COMMAND, {} },
        { RECORD_ON_NEW_TRACK_CODE, RECORD_ON_NEW_TRACK_COMMAND, {} },
        { RECORD_PAUSE_QUERY.toString(), RECORD_PAUSE_COMMAND, {} },
        { RECORD_STOP_QUERY.toString(), RECORD_STOP_COMMAND, {} },
        { RECORD_TOGGLE_MIC_METERING.toString(), RECORD_TOGGLE_MIC_METERING_COMMAND, {} },
        { RECORD_TOGGLE_INPUT_MONITORING.toString(), RECORD_TOGGLE_INPUT_MONITORING_COMMAND, {} },
        { RECORD_LEAD_IN_RECORDING_QUERY.toString(), RECORD_LEAD_IN_RECORDING_COMMAND, {} },
    };
    registerActionToCommand(this, actionToCommand, commandDispatcher(), dispatcher());

    playbackController()->isPlayingChanged().onNotify(this, [this]() {
        m_isRecordAllowedChanged.notify();
    });

    record()->recordPositionChanged().onReceive(this, [this](const muse::secs_t&) {
        if (m_currentRecordStatus == RecordStatus::LeadIn) {
            setCurrentRecordStatus(RecordStatus::Running);
        }
    });

    record()->recordingFinished().onNotify(this, [this]() {
        if (isRecording()) {
            setCurrentRecordStatus(RecordStatus::Stopped);
        }
    });

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });
}

void RecordController::deinit()
{
}

bool RecordController::isRecordAllowed() const
{
    return !playbackController()->isPlaying();
}

Notification RecordController::isRecordAllowedChanged() const
{
    return m_isRecordAllowedChanged;
}

bool RecordController::isRecording() const
{
    return m_currentRecordStatus == RecordStatus::Running
           || m_currentRecordStatus == RecordStatus::Paused
           || m_currentRecordStatus == RecordStatus::LeadIn;
}

const std::vector<au::trackedit::ClipKey>& RecordController::recordingClipKeys() const
{
    return record()->recordingClipKeys();
}

Notification RecordController::isRecordingChanged() const
{
    return m_isRecordingChanged;
}

Ret RecordController::toggleRecord()
{
    if (m_currentRecordStatus == RecordStatus::Paused) {
        return resume();
    }

    if (isRecording()) {
        return stop();
    }

    return start();
}

Ret RecordController::recordOnNewTrack()
{
    if (isRecording()) {
        return stop();
    }

    return startWithNewTrack();
}

Ret RecordController::start()
{
    IF_ASSERT_FAILED(record()) {
        return make_ret(Ret::Code::InternalError);
    }

    stopPlaybackIfPaused();

    Ret ret = record()->start();
    if (!ret) {
        //: Title of an error dialog
        interactive()->error(muse::trc("record", "Recording error"), ret.text());
        return ret;
    }

    setCurrentRecordStatus(RecordStatus::Running);
    return make_ok();
}

Ret RecordController::startWithNewTrack()
{
    IF_ASSERT_FAILED(record()) {
        return make_ret(Ret::Code::InternalError);
    }

    stopPlaybackIfPaused();

    const int recordingChannels = std::max(1, audioDriverController()->configuration().inputChannels);

    au::trackedit::TrackIdList newTracks;
    if (recordingChannels == 2) {
        newTracks.push_back(tracksInteraction()->addWaveTrack(2));
    } else {
        for (int i = 0; i < recordingChannels; ++i) {
            newTracks.push_back(tracksInteraction()->addWaveTrack(1));
        }
    }

    selectionController()->setSelectedTracks(newTracks);
    trackNavigationController()->setFocusedTrack(newTracks.front());

    Ret ret = record()->start();
    if (!ret) {
        trackeditInteraction()->deleteTracks(newTracks);
        interactive()->error(muse::trc("record", "Recording error"), ret.text());
        return ret;
    }

    setCurrentRecordStatus(RecordStatus::Running);
    return make_ok();
}

Ret RecordController::pause()
{
    IF_ASSERT_FAILED(record()) {
        return make_ret(Ret::Code::InternalError);
    }

    if (m_currentRecordStatus == RecordStatus::Paused) {
        return resume();
    }

    Ret ret = record()->pause();
    if (!ret) {
        interactive()->error(muse::trc("record", "Recording error"), ret.text());
        return ret;
    }

    setCurrentRecordStatus(RecordStatus::Paused);
    return make_ok();
}

Ret RecordController::resume()
{
    IF_ASSERT_FAILED(record()) {
        return make_ret(Ret::Code::InternalError);
    }

    Ret ret = record()->resume();
    if (!ret) {
        interactive()->error(muse::trc("record", "Recording error"), ret.text());
        return ret;
    }

    setCurrentRecordStatus(RecordStatus::Running);
    return make_ok();
}

Ret RecordController::stop()
{
    IF_ASSERT_FAILED(record()) {
        return make_ret(Ret::Code::InternalError);
    }

    Ret ret = record()->stop();
    if (!ret) {
        interactive()->error(muse::trc("record", "Recording error"), ret.text());
        return ret;
    }

    setCurrentRecordStatus(RecordStatus::Stopped);
    return make_ok();
}

Ret RecordController::leadInRecording()
{
    IF_ASSERT_FAILED(record()) {
        return make_ret(Ret::Code::InternalError);
    }

    stopPlaybackIfPaused();

    // Store the recording start position and selected tracks before starting;
    // recording always starts from the current playhead position
    m_leadInRecordingStartTime = globalContext()->playbackState()->playbackPosition();
    m_leadInRecordingTrackIds = selectionController()->selectedTracks();

    Ret ret = record()->leadInRecording();
    if (!ret) {
        m_leadInRecordingTrackIds.clear();
        interactive()->error(muse::trc("record", "Lead-in Recording error"), ret.text());
        return ret;
    }

    setCurrentRecordStatus(RecordStatus::LeadIn);
    return make_ok();
}

void RecordController::stopPlaybackIfPaused()
{
    //! NOTE: recording from a paused playback state must start immediately from
    //! the playhead; stop playback first so that the paused stream is released
    //! and the engine pause flag is cleared
    if (playbackController()->isPaused()) {
        playbackController()->stop();
    }
}

Ret RecordController::toggleMicMetering()
{
    configuration()->setIsMicMeteringOn(!configuration()->isMicMeteringOn());
    return make_ok();
}

muse::async::Notification RecordController::isMicMeteringOnChanged() const
{
    return configuration()->isMicMeteringOnChanged();
}

bool RecordController::isMicMeteringOn() const
{
    return configuration()->isMicMeteringOn();
}

Ret RecordController::toggleInputMonitoring()
{
    configuration()->setIsInputMonitoringOn(!configuration()->isInputMonitoringOn());
    return make_ok();
}

muse::async::Notification RecordController::isInputMonitoringOnChanged() const
{
    return configuration()->isInputMonitoringOnChanged();
}

bool RecordController::isInputMonitoringOn() const
{
    return configuration()->isInputMonitoringOn();
}

bool RecordController::isLeadInRecording() const
{
    return m_currentRecordStatus == RecordStatus::LeadIn;
}

muse::async::Notification RecordController::isLeadInRecordingChanged() const
{
    return m_isRecordingChanged;
}

muse::secs_t RecordController::leadInRecordingStartTime() const
{
    return m_leadInRecordingStartTime;
}

std::vector<au::trackedit::TrackId> RecordController::leadInRecordingTrackIds() const
{
    return m_leadInRecordingTrackIds;
}

void RecordController::setCurrentRecordStatus(RecordStatus status)
{
    if (m_currentRecordStatus == status) {
        return;
    }

    // Clear lead-in data when leaving LeadIn state
    if (m_currentRecordStatus == RecordStatus::LeadIn && status != RecordStatus::LeadIn) {
        m_leadInRecordingTrackIds.clear();
    }

    m_currentRecordStatus = status;
    m_isRecordingChanged.notify();
}

bool RecordController::canReceiveAction(const ActionCode& code) const
{
    if (globalContext()->currentProject() == nullptr) {
        return false;
    }

    if (code == RECORD_START_QUERY.toString()
        || code == RECORD_ON_CURRENT_TRACK_CODE
        || code == RECORD_ON_NEW_TRACK_CODE) {
        return !playbackController()->isPlaying() && m_currentRecordStatus != RecordStatus::LeadIn;
    }

    if (code == RECORD_LEAD_IN_RECORDING_QUERY.toString()) {
        return !playbackController()->isPlaying() && !isRecording();
    }

    if (code == RECORD_STOP_QUERY.toString()) {
        return isRecording();
    }

    return true;
}

void RecordController::onProjectChanged()
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (prj) {
        prj->aboutCloseBegin().onNotify(this, [this]() {
            stop();
        });
    }
}
