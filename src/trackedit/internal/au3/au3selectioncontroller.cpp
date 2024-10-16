/*
* Audacity: A Digital Audio Editor
*/
#include "au3selectioncontroller.h"

#include "global/containers.h"
#include "global/realfn.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"

#include "au3wrap/internal/domconverter.h"
#include "au3wrap/au3types.h"
#include "au3wrap/internal/domaccessor.h"

#include "log.h"

//#define DEBUG_SELECTION
#ifdef DEBUG_SELECTION
#define MYLOG LOGD
#else
#define MYLOG LOGN
#endif

using namespace au::trackedit;
using namespace au::au3;

// clip selection

void Au3SelectionController::resetSelectedTrack()
{
    MYLOG() << "resetSelectedTrack";

    auto& tracks = Au3TrackList::Get(projectRef());
    for (Au3Track* au3Track : tracks) {
        au3Track->SetSelected(false);
    }

    m_selectedTrack.set(au::trackedit::TrackId(-1), true);
}

au::trackedit::TrackId Au3SelectionController::selectedTrack() const
{
    return m_selectedTrack.val;
}

void Au3SelectionController::setSelectedTrack(trackedit::TrackId trackId)
{
    MYLOG() << "track: " << trackId;

    auto& tracks = Au3TrackList::Get(projectRef());
    for (Au3Track* au3Track : tracks) {
        au3Track->SetSelected(au3Track->GetId() == Au3TrackId(trackId));
    }

    m_selectedTrack.set(trackId, true);
}

muse::async::Channel<au::trackedit::TrackId> Au3SelectionController::trackSelected() const
{
    return m_selectedTrack.selected;
}

void Au3SelectionController::resetSelectedClip()
{
    MYLOG() << "resetSelectedClip";
    m_selectedClip.set(au::trackedit::ClipKey(), true);
}

au::trackedit::ClipKey Au3SelectionController::selectedClip() const
{
    return m_selectedClip.val;
}

void Au3SelectionController::setSelectedClip(const au::trackedit::ClipKey& clipKey)
{
    MYLOG() << clipKey;
    m_selectedClip.set(clipKey, true);
}

muse::async::Channel<au::trackedit::ClipKey> Au3SelectionController::clipSelected() const
{
    return m_selectedClip.selected;
}

double Au3SelectionController::selectedClipStartTime() const
{
    auto clipKey = selectedClip();
    if (!clipKey.isValid()) {
        return -1.0;
    }

    WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    return clip->Start();
}

double Au3SelectionController::selectedClipEndTime() const
{
    auto clipKey = selectedClip();
    if (!clipKey.isValid()) {
        return -1.0;
    }

    WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return false;
    }

    std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    IF_ASSERT_FAILED(clip) {
        return false;
    }

    return clip->End();
}

void Au3SelectionController::setSelectedTrackAudioData(TrackId trackId)
{
    auto& tracks = ::TrackList::Get(projectRef());
    ::Track* au3Track = tracks.FindById(::TrackId(trackId));

    secs_t audioDataStartTime = au3Track->GetStartTime();
    secs_t audioDataEndTime = au3Track->GetEndTime();

    setDataSelectedStartTime(audioDataStartTime, true);
    setDataSelectedEndTime(audioDataEndTime, true);
}

void Au3SelectionController::setSelectedClipAudioData(trackedit::TrackId trackId, secs_t time)
{
    const auto& clip = au3::DomAccessor::findWaveClip(projectRef(), trackId, time);

    secs_t audioDataStartTime = clip->Start();
    secs_t audioDataEndTime = clip->End();

    setDataSelectedStartTime(audioDataStartTime, true);
    setDataSelectedEndTime(audioDataEndTime, true);
}

// data selection

void Au3SelectionController::resetDataSelection()
{
    MYLOG() << "resetDataSelection";

    auto& tracks = Au3TrackList::Get(projectRef());
    for (trackedit::TrackId trackId : m_selectedTrackIds.val) {
        Au3Track* au3Track = tracks.FindById(Au3TrackId(trackId));
        if (au3Track) {
            au3Track->SetSelected(false);
        }
    }

    m_selectedTrackIds.set(std::vector<au::trackedit::TrackId>(), true);
    m_selectedStartTime.set(-1.0, true);
    m_selectedEndTime.set(-1.0, true);
}

bool Au3SelectionController::isDataSelected() const
{
    return muse::RealIsEqualOrMore(m_selectedStartTime.val, 0.0) && m_selectedEndTime.val > 0.0;
}

bool Au3SelectionController::isDataSelectedOnTrack(TrackId trackId) const
{
    return std::find(m_selectedTrackIds.val.begin(), m_selectedTrackIds.val.end(), trackId) != m_selectedTrackIds.val.end();
}

std::vector<au::trackedit::TrackId> Au3SelectionController::dataSelectedOnTracks() const
{
    return m_selectedTrackIds.val;
}

void Au3SelectionController::setDataSelectedOnTracks(
    const std::vector<au::trackedit::TrackId>& trackIds,
    bool complete)
{
    MYLOG() << "trackIds: " << trackIds << ", complete: " << complete;

    auto& tracks = Au3TrackList::Get(projectRef());
    for (Au3Track* au3Track : tracks) {
        if (muse::contains(trackIds, au3::DomConverter::trackId(au3Track->GetId()))) {
            au3Track->SetSelected(true);
        } else {
            au3Track->SetSelected(false);
        }
    }

    m_selectedTrackIds.set(trackIds, complete);
}

muse::async::Channel<std::vector<au::trackedit::TrackId> >
Au3SelectionController::dataSelectedOnTracksChanged() const
{
    return m_selectedTrackIds.changed;
}

muse::async::Channel<std::vector<au::trackedit::TrackId> >
Au3SelectionController::dataSelectedOnTracksSelected() const
{
    return m_selectedTrackIds.selected;
}

au::trackedit::secs_t Au3SelectionController::dataSelectedStartTime() const
{
    return m_selectedStartTime.val;
}

void Au3SelectionController::setDataSelectedStartTime(au::trackedit::secs_t time, bool complete)
{
    MYLOG() << "start time: " << time << ", complete: " << complete;

    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setT0(time);

    m_selectedStartTime.set(time, complete);
}

muse::async::Channel<au::trackedit::secs_t> Au3SelectionController::dataSelectedStartTimeChanged() const
{
    return m_selectedStartTime.changed;
}

muse::async::Channel<au::trackedit::secs_t> Au3SelectionController::dataSelectedStartTimeSelected() const
{
    return m_selectedStartTime.selected;
}

au::trackedit::secs_t Au3SelectionController::dataSelectedEndTime() const
{
    return m_selectedEndTime.val;
}

void Au3SelectionController::setDataSelectedEndTime(au::trackedit::secs_t time, bool complete)
{
    MYLOG() << "end time: " << time << ", complete: " << complete;

    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setT1(time);

    m_selectedEndTime.set(time, complete);
}

muse::async::Channel<au::trackedit::secs_t> Au3SelectionController::dataSelectedEndTimeChanged() const
{
    return m_selectedEndTime.changed;
}

muse::async::Channel<au::trackedit::secs_t> Au3SelectionController::dataSelectedEndTimeSelected() const
{
    return m_selectedEndTime.selected;
}

Au3Project& Au3SelectionController::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
