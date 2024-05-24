/*
* Audacity: A Digital Audio Editor
*/
#include "recordcontroller.h"

using namespace muse;
using namespace au::audio;
using namespace au::record;
using namespace muse::async;
using namespace muse::actions;

static const ActionCode RECORD_CODE("record");
static const ActionCode RECORD_LEVEL_CODE("record-level");

void RecordController::init()
{
    dispatcher()->reg(this, RECORD_CODE, this, &RecordController::toggleRecord);
}

void RecordController::deinit()
{
    stop();
}

bool RecordController::isRecording() const
{
    return m_currentPlaybackStatus == RecordStatus::Running;
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
    IF_ASSERT_FAILED(au3Record()) {
        return;
    }

    au3Record()->start();
    setCurrentRecordStatus(RecordStatus::Running);
}

void RecordController::stop()
{
    IF_ASSERT_FAILED(au3Record()) {
        return;
    }

    au3Record()->stop();
    setCurrentRecordStatus(RecordStatus::Stopped);
}

void RecordController::setCurrentRecordStatus(RecordStatus status)
{
    if (m_currentPlaybackStatus == status) {
        return;
    }

    m_currentPlaybackStatus = status;
    m_isRecordingChanged.notify();
}

bool RecordController::canReceiveAction(const ActionCode&) const
{
    return globalContext()->currentProject() != nullptr;
}
