/*
* Audacity: A Digital Audio Editor
*/
#include "recordcontroller.h"

#include "translation.h"

using namespace muse;
using namespace au::record;
using namespace muse::async;
using namespace muse::actions;

static const ActionQuery RECORD_START_QUERY("action://record/start");
static const ActionQuery RECORD_PAUSE_QUERY("action://record/pause");
static const ActionQuery RECORD_STOP_QUERY("action://record/stop");
static const ActionQuery RECORD_LEVEL_QUERY("action://record/level"); // doesn't have callback here

void RecordController::init()
{
    dispatcher()->reg(this, RECORD_START_QUERY, this, &RecordController::toggleRecord);
    dispatcher()->reg(this, RECORD_PAUSE_QUERY, this, &RecordController::pause);
    dispatcher()->reg(this, RECORD_STOP_QUERY, this, &RecordController::stop);

    playbackController()->isPlayingChanged().onNotify(this, [this]() {
        m_isRecordAllowedChanged.notify();
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
    return m_currentRecordStatus == RecordStatus::Running || m_currentRecordStatus == RecordStatus::Paused;
}

Channel<muse::secs_t> RecordController::recordPositionChanged() const
{
    return record()->recordPositionChanged();
}

secs_t RecordController::recordPosition() const
{
    return record()->recordPosition();
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

void RecordController::setCurrentRecordStatus(RecordStatus status)
{
    if (m_currentRecordStatus == status) {
        return;
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
        return !playbackController()->isPlaying();
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
