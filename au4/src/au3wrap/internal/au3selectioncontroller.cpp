#include "au3selectioncontroller.h"

using namespace au::au3;

void Au3SelectionController::resetSelection()
{
    m_selectedTrackIds.set(std::vector<processing::TrackId>());
    m_selectedStartTime.set(-1.0);
    m_selectedEndTime.set(-1.0);
}

muse::ValCh<std::vector<au::processing::TrackId> > Au3SelectionController::selectedTrackIds() const
{
    return m_selectedTrackIds;
}

void Au3SelectionController::setSelectedTrackIds(const std::vector<processing::TrackId>& trackIds)
{
    m_selectedTrackIds.set(trackIds);
}

muse::ValCh<au::processing::secs_t> Au3SelectionController::selectedStartTime() const
{
    return m_selectedStartTime;
}

void Au3SelectionController::setSelectedStartTime(const processing::secs_t time)
{
    m_selectedStartTime.set(time);
}

muse::ValCh<au::processing::secs_t> Au3SelectionController::selectedEndTime() const
{
    return m_selectedEndTime;
}

void Au3SelectionController::setSelectedEndTime(const processing::secs_t time)
{
    m_selectedEndTime.set(time);
}
