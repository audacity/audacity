/*
* Audacity: A Digital Audio Editor
*/
#include "recordcontroller.h"

#include "framework/global/translation.h"

using namespace muse;
using namespace au::record;
using namespace muse::async;
using namespace muse::actions;

static const ActionQuery RECORD_START_QUERY("action://record/start");
static const ActionQuery RECORD_PAUSE_QUERY("action://record/pause");
static const ActionQuery RECORD_STOP_QUERY("action://record/stop");
static const ActionQuery RECORD_LEVEL_QUERY("action://record/level"); // doesn't have callback here
static const ActionQuery RECORD_TOGGLE_MIC_METERING("action://record/toggle-mic-metering");
static const ActionQuery RECORD_TOGGLE_INPUT_MONITORING("action://record/toggle-input-monitoring");
static const ActionQuery RECORD_LEAD_IN_RECORDING_QUERY("action://record/lead-in-recording");

void RecordController::init()
{
    dispatcher()->reg(this, RECORD_START_QUERY, this, &RecordController::toggleRecord);
    dispatcher()->reg(this, RECORD_PAUSE_QUERY, this, &RecordController::pause);
    dispatcher()->reg(this, RECORD_STOP_QUERY, this, &RecordController::stop);
    dispatcher()->reg(this, RECORD_TOGGLE_MIC_METERING, this, &RecordController::toggleMicMetering);
    dispatcher()->reg(this, RECORD_TOGGLE_INPUT_MONITORING, this, &RecordController::toggleInputMonitoring);
    dispatcher()->reg(this, RECORD_LEAD_IN_RECORDING_QUERY, this, &RecordController::leadInRecording);

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

void RecordController::toggleRecord()
{
    if (isRecording()) {
        stop();
    } else {
        start();
    }
}

void RecordController::start()
{
    IF_ASSERT_FAILED(record()) {
        return;
    }

    Ret ret = record()->start();
    if (!ret) {
        interactive()->error(muse::trc("record", "Recording error"), ret.text());
        return;
    }

    setCurrentRecordStatus(RecordStatus::Running);
}

void RecordController::pause()
{
    IF_ASSERT_FAILED(record()) {
        return;
    }

    Ret ret = record()->pause();
    if (!ret) {
        interactive()->error(muse::trc("record", "Recording error"), ret.text());
        return;
    }

    setCurrentRecordStatus(RecordStatus::Paused);
}

void RecordController::stop()
{
    IF_ASSERT_FAILED(record()) {
        return;
    }

    Ret ret = record()->stop();
    if (!ret) {
        interactive()->error(muse::trc("record", "Recording error"), ret.text());
        return;
    }

    setCurrentRecordStatus(RecordStatus::Stopped);
}

void RecordController::leadInRecording()
{
    IF_ASSERT_FAILED(record()) {
        return;
    }

    // Store the recording start position and selected tracks before starting
    m_leadInRecordingStartTime = selectionController()->selectionStartTime();
    m_leadInRecordingTrackIds = selectionController()->selectedTracks();

    Ret ret = record()->leadInRecording();
    if (!ret) {
        m_leadInRecordingTrackIds.clear();
        interactive()->error(muse::trc("record", "Lead-in Recording error"), ret.text());
        return;
    }

    setCurrentRecordStatus(RecordStatus::LeadIn);
}

void RecordController::toggleMicMetering()
{
    configuration()->setIsMicMeteringOn(!configuration()->isMicMeteringOn());
}

muse::async::Notification RecordController::isMicMeteringOnChanged() const
{
    return configuration()->isMicMeteringOnChanged();
}

bool RecordController::isMicMeteringOn() const
{
    return configuration()->isMicMeteringOn();
}

void RecordController::toggleInputMonitoring()
{
    configuration()->setIsInputMonitoringOn(!configuration()->isInputMonitoringOn());
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

    if (code == RECORD_START_QUERY.toString()) {
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
