#include "au3selectioncontroller.h"

using namespace au::au3;

void Au3SelectionController::resetDataSelection()
{
    m_selectedTrackIds.set(std::vector<processing::TrackId>());
    m_selectedStartTime.set(-1.0);
    m_selectedEndTime.set(-1.0);
}

muse::ValCh<std::vector<au::processing::TrackId> > Au3SelectionController::dataSelectedOnTracks() const
{
    return m_selectedTrackIds;
}

void Au3SelectionController::setDataSelectedOnTracks(const std::vector<processing::TrackId>& trackIds)
{
    m_selectedTrackIds.set(trackIds);
}

muse::ValCh<au::processing::secs_t> Au3SelectionController::dataSelectedStartTime() const
{
    return m_selectedStartTime;
}

void Au3SelectionController::setDataSelectedStartTime(const processing::secs_t time)
{
    m_selectedStartTime.set(time);
}

muse::ValCh<au::processing::secs_t> Au3SelectionController::dataSelectedEndTime() const
{
    return m_selectedEndTime;
}

void Au3SelectionController::setDataSelectedEndTime(const processing::secs_t time)
{
    m_selectedEndTime.set(time);
}
