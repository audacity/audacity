/*
* Audacity: A Digital Audio Editor
*/

#include "recordmetercontroller.h"

using namespace au::record;

bool RecordMeterController::isRecordMeterVisible() const
{
    return m_recordMeterVisible;
}

void RecordMeterController::setRecordMeterVisible(bool visible)
{
    if (m_recordMeterVisible == visible) {
        return;
    }

    m_recordMeterVisible = visible;
    m_isRecordMeterVisibleChanged.notify();
}

muse::async::Notification RecordMeterController::isRecordMeterVisibleChanged() const
{
    return m_isRecordMeterVisibleChanged;
}

bool RecordMeterController::isSoundActivationMeterVisible() const
{
    return m_soundActivationMeterVisible;
}

void RecordMeterController::setSoundActivationMeterVisible(bool visible)
{
    if (m_soundActivationMeterVisible == visible) {
        return;
    }

    m_soundActivationMeterVisible = visible;
    m_isSoundActivationMeterVisibleChanged.notify();
}

muse::async::Notification RecordMeterController::isSoundActivationMeterVisibleChanged() const
{
    return m_isSoundActivationMeterVisibleChanged;
}
