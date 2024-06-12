/*
* Audacity: A Digital Audio Editor
*/
#include "au3selectioncontroller.h"

#include "log.h"

//#define DEBUG_SELECTION
#ifdef DEBUG_SELECTION
#define MYLOG LOGD
#else
#define MYLOG LOGN
#endif

using namespace au::au3;

// clip selection

void Au3SelectionController::resetSelectedClip()
{
    MYLOG() << "resetSelectedClip";
    m_selectedClip.set(au::processing::ClipKey(), true);
}

au::processing::ClipKey Au3SelectionController::selectedClip() const
{
    return m_selectedClip.val;
}

void Au3SelectionController::setSelectedClip(const au::processing::ClipKey& clipKey)
{
    MYLOG() << "track: " << clipKey.trackId << ", clip: " << clipKey.index;
    m_selectedClip.set(clipKey, true);
}

muse::async::Channel<au::processing::ClipKey> Au3SelectionController::clipSelected() const
{
    return m_selectedClip.selected;
}

// data selection

void Au3SelectionController::resetDataSelection()
{
    MYLOG() << "resetDataSelection";
    m_selectedTrackIds.set(std::vector<au::processing::TrackId>(), true);
    m_selectedStartTime.set(-1.0, true);
    m_selectedEndTime.set(-1.0, true);
}

std::vector<au::processing::TrackId> Au3SelectionController::dataSelectedOnTracks() const
{
    return m_selectedTrackIds.val;
}

void Au3SelectionController::setDataSelectedOnTracks(
    const std::vector<au::processing::TrackId>& trackIds,
    bool complete)
{
    MYLOG() << "trackIds: " << trackIds << ", complete: " << complete;
    m_selectedTrackIds.set(trackIds, complete);
}

muse::async::Channel<std::vector<au::processing::TrackId> >
Au3SelectionController::dataSelectedOnTracksChanged() const
{
    return m_selectedTrackIds.changed;
}

muse::async::Channel<std::vector<au::processing::TrackId> >
Au3SelectionController::dataSelectedOnTracksSelected() const
{
    return m_selectedTrackIds.selected;
}

au::processing::secs_t Au3SelectionController::dataSelectedStartTime() const
{
    return m_selectedStartTime.val;
}

void Au3SelectionController::setDataSelectedStartTime(au::processing::secs_t time, bool complete)
{
    MYLOG() << "start time: " << time << ", complete: " << complete;
    m_selectedStartTime.set(time, complete);
}

muse::async::Channel<au::processing::secs_t> Au3SelectionController::dataSelectedStartTimeChanged() const
{
    return m_selectedStartTime.changed;
}

muse::async::Channel<au::processing::secs_t> Au3SelectionController::dataSelectedStartTimeSelected() const
{
    return m_selectedStartTime.selected;
}

au::processing::secs_t Au3SelectionController::dataSelectedEndTime() const
{
    return m_selectedEndTime.val;
}

void Au3SelectionController::setDataSelectedEndTime(au::processing::secs_t time, bool complete)
{
    MYLOG() << "end time: " << time << ", complete: " << complete;
    m_selectedEndTime.set(time, complete);
}

muse::async::Channel<au::processing::secs_t> Au3SelectionController::dataSelectedEndTimeChanged() const
{
    return m_selectedEndTime.changed;
}

muse::async::Channel<au::processing::secs_t> Au3SelectionController::dataSelectedEndTimeSelected() const
{
    return m_selectedEndTime.selected;
}
