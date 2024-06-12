/*
* Audacity: A Digital Audio Editor
*/
#include "selectionstatusmodel.h"

using namespace au::projectscene;

SelectionStatusModel::SelectionStatusModel()
{
}

void SelectionStatusModel::init()
{
    m_startTime = selectionController()->dataSelectedStartTime();
    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](processing::secs_t time) {
        m_startTime = time;
        emit startTimeChanged();
    });

    m_endTime = selectionController()->dataSelectedEndTime();
    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](processing::secs_t time) {
        m_endTime = time;
        emit endTimeChanged();
    });
}

double SelectionStatusModel::startTime() const
{
    return m_startTime;
}

double SelectionStatusModel::endTime() const
{
    return m_endTime;
}
