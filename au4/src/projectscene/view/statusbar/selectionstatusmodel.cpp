/*
* Audacity: A Digital Audio Editor
*/
#include "selectionstatusmodel.h"

#include "playback/iaudiooutput.h"

using namespace au::projectscene;

void SelectionStatusModel::init()
{
    m_startTime = selectionController()->dataSelectedStartTime();
    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](processing::secs_t time) {
        m_startTime = !time.is_negative() ? time : processing::secs_t(0.0);
        emit startTimeChanged();
    });

    m_endTime = selectionController()->dataSelectedEndTime();
    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](processing::secs_t time) {
        m_endTime = !time.is_negative() ? time : processing::secs_t(0.0);
        emit endTimeChanged();
    });

    playback()->audioOutput()->sampleRateChanged().onReceive(this, [this](audio::sample_rate_t) {
        emit sampleRateChanged();
    });
}

double SelectionStatusModel::startTime() const
{
    return m_startTime;
}

void SelectionStatusModel::setStartTime(double time)
{
    if (qFuzzyCompare(m_startTime, time)) {
        return;
    }

    selectionController()->setDataSelectedStartTime(time, true);
}

double SelectionStatusModel::endTime() const
{
    return m_endTime;
}

void SelectionStatusModel::setEndTime(double time)
{
    if (qFuzzyCompare(m_endTime, time)) {
        return;
    }

    selectionController()->setDataSelectedEndTime(time, true);
}

int SelectionStatusModel::currentFormat() const
{
    return m_currentFormat; // from settings
}

void SelectionStatusModel::setCurrentFormat(int format)
{
    if (m_currentFormat == format) {
        return;
    }

    m_currentFormat = format;
    emit currentFormatChanged();
}

double SelectionStatusModel::sampleRate() const
{
    return playback()->audioOutput()->sampleRate();
}
