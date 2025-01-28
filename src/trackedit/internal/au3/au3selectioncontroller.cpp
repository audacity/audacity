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

void Au3SelectionController::init()
{
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

        resetDataSelection();

        if (prj) {
            //! NOTE: sync selectionControler with internal project state if trackList changed
            auto trackList = &Au3TrackList::Get(projectRef());
            m_tracksSubc = trackList->Subscribe([this](const TrackListEvent&) {
                updateSelectionController();
            });

            //! NOTE: load project's last saved selection state
            auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
            m_selectedStartTime.set(selectedRegion.t0(), true);
            m_selectedEndTime.set(selectedRegion.t1(), true);
        } else {
            m_tracksSubc.Reset();
        }
    });
}

void Au3SelectionController::resetSelectedTracks()
{
    MYLOG() << "resetSelectedTrack";

    setSelectedTracks({}, true);
}

TrackIdList Au3SelectionController::selectedTracks() const
{
    return m_selectedTracks.val;
}

void Au3SelectionController::setSelectedTracks(const TrackIdList& tracksIds, bool complete)
{
    MYLOG() << "tracks: " << tracksIds;

    auto& tracks = Au3TrackList::Get(projectRef());
    for (Au3Track* au3Track : tracks) {
        if (muse::contains(tracksIds, TrackId(au3Track->GetId()))) {
            au3Track->SetSelected(true);
        } else {
            au3Track->SetSelected(false);
        }
    }

    m_selectedTracks.set(tracksIds, complete);
}

void Au3SelectionController::addSelectedTrack(const TrackId& trackId)
{
    auto selectedTracks = m_selectedTracks.val;

    if (!muse::contains(selectedTracks, trackId)) {
        selectedTracks.push_back(trackId);

        auto& tracks = Au3TrackList::Get(projectRef());
        for (Au3Track* au3Track : tracks) {
            if (muse::contains(selectedTracks, TrackId(au3Track->GetId()))) {
                au3Track->SetSelected(true);
            } else {
                au3Track->SetSelected(false);
            }
        }

        m_selectedTracks.set(selectedTracks, true);
    }
}

TrackIdList Au3SelectionController::determinateTracks(double y1, double y2) const
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return {};
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return {};
    }

    if (y1 < 0 && y2 < 0) {
        return {};
    }

    if (y1 > y2) {
        std::swap(y1, y2);
    }

    if (y1 < 1) {
        y1 = 1;
    }

    trackedit::TrackIdList tracks = prj->trackIdList();
    trackedit::TrackIdList result;

    int tracksVericalY = vs->tracksVericalY().val;
    int trackTop = -tracksVericalY;
    int trackBottom = trackTop;

    for (trackedit::TrackId trackId : tracks) {
        trackTop = trackBottom;
        trackBottom = trackTop + vs->trackHeight(trackId).val;

        if (muse::RealIsEqualOrMore(y1, trackTop) && !muse::RealIsEqualOrMore(y1, trackBottom)) {
            result.push_back(trackId);
        }

        if (muse::RealIsEqualOrMore(y2, trackTop) && !muse::RealIsEqualOrMore(y2, trackBottom)) {
            if (!result.empty() && result.back() != trackId) {
                result.push_back(trackId);
            }
            break;
        }

        if (!result.empty() && result.back() != trackId) {
            result.push_back(trackId);
            continue;
        }
    }

    return result;
}

std::optional<au::trackedit::TrackId> Au3SelectionController::determinePointedTrack(double y) const
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return {};
    }

    auto vs = globalContext()->currentProject()->viewState();
    if (!vs) {
        return {};
    }

    if (y < 1) {
        y = 1;
    }

    trackedit::TrackIdList tracks = prj->trackIdList();

    int tracksVericalY = vs->tracksVericalY().val;
    int trackTop = -tracksVericalY;
    int trackBottom = trackTop;

    for (trackedit::TrackId trackId : tracks) {
        trackTop = trackBottom;
        trackBottom = trackTop + vs->trackHeight(trackId).val;

        if (muse::RealIsEqualOrMore(y, trackTop) && muse::RealIsEqualOrLess(y, trackBottom)) {
            return trackId;
        }
    }

    if (muse::RealIsEqualOrLess(y, trackBottom)) {
        return {};
    }

    return INVALID_TRACK;
}

muse::async::Channel<TrackIdList> Au3SelectionController::tracksSelected() const
{
    return m_selectedTracks.selected;
}

void Au3SelectionController::resetSelectedClips()
{
    MYLOG() << "resetSelectedClip";
    m_selectedClips.set(au::trackedit::ClipKeyList(), true);
}

bool Au3SelectionController::hasSelectedClips() const
{
    return !m_selectedClips.val.empty();
}

ClipKeyList Au3SelectionController::selectedClips() const
{
    return m_selectedClips.val;
}

ClipKeyList Au3SelectionController::selectedClipsInTrackOrder() const
{
    ClipKeyList sortedSelectedClips;
    auto& tracks = ::TrackList::Get(projectRef());
    for (const auto& track : tracks) {
        for (const auto& selectedClip : m_selectedClips.val) {
            if (TrackId(track->GetId()) == selectedClip.trackId) {
                sortedSelectedClips.push_back(selectedClip);
            }
        }
    }

    return sortedSelectedClips;
}

void Au3SelectionController::setSelectedClips(const ClipKeyList& clipKeys, bool complete)
{
    m_selectedClips.set(clipKeys, complete);

    //! NOTE: when selecting a clip, we also need to select
    //! the track on which the clip is located
    TrackIdList selectedTracks;
    for (const ClipKey& key : clipKeys) {
        if (muse::contains(selectedTracks, key.trackId)) {
            continue;
        }
        selectedTracks.push_back(key.trackId);
    }
    setSelectedTracks(selectedTracks, complete);
}

std::optional<au::trackedit::ClipId> Au3SelectionController::setSelectedClip(trackedit::TrackId trackId, secs_t time)
{
    const auto& clip = au3::DomAccessor::findWaveClip(projectRef(), trackId, time);
    if (!clip) {
        return std::nullopt;
    }

    setSelectedClips({ { trackId, clip->GetId() } }, true);
    return clip->GetId();
}

void Au3SelectionController::addSelectedClip(const ClipKey& clipKey)
{
    auto selectedClips = m_selectedClips.val;

    if (!muse::contains(selectedClips, clipKey)) {
        selectedClips.push_back(clipKey);

        m_selectedClips.set(selectedClips, true);

        //! NOTE: when selecting a clip, we also need to select
        //! the track on which the clip is located
        addSelectedTrack(clipKey.trackId);
    }
}

void Au3SelectionController::removeClipSelection(const ClipKey& clipKey)
{
    auto selectedClips = m_selectedClips.val;

    if (!muse::contains(selectedClips, clipKey)) {
        return;
    }

    selectedClips.erase(
        std::remove(selectedClips.begin(), selectedClips.end(), clipKey),
        selectedClips.end()
        );

    m_selectedClips.set(selectedClips, true);

    //! NOTE: update selected tracks
    TrackIdList selectedTracks;
    for (const ClipKey& key : selectedClips) {
        if (muse::contains(selectedTracks, key.trackId)) {
            continue;
        }
        selectedTracks.push_back(key.trackId);
    }
    setSelectedTracks(selectedTracks, true);
}

muse::async::Channel<ClipKeyList> Au3SelectionController::clipsSelected() const
{
    return m_selectedClips.selected;
}

double Au3SelectionController::selectedClipStartTime() const
{
    auto clipKeyList = selectedClips();
    if (clipKeyList.empty()) {
        return -1.0;
    }

    auto clipKey = clipKeyList.at(0);

    WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    if (!clip) {
        return -1.0;
    }

    return clip->Start();
}

double Au3SelectionController::selectedClipEndTime() const
{
    auto clipKeyList = selectedClips();
    if (clipKeyList.empty()) {
        return -1.0;
    }

    auto clipKey = clipKeyList.at(0);

    WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return -1.0;
    }

    std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
    if (!clip) {
        return -1.0;
    }

    return clip->End();
}

void Au3SelectionController::setSelectedTrackAudioData(TrackId trackId)
{
    auto& tracks = ::TrackList::Get(projectRef());
    ::Track* au3Track = tracks.FindById(::TrackId(trackId));
    if (!au3Track) {
        return;
    }

    secs_t audioDataStartTime = au3Track->GetStartTime();
    secs_t audioDataEndTime = au3Track->GetEndTime();

    setDataSelectedStartTime(audioDataStartTime, true);
    setDataSelectedEndTime(audioDataEndTime, true);
}

void Au3SelectionController::setSelectedClipAudioData(trackedit::TrackId trackId, secs_t time)
{
    const auto& clip = au3::DomAccessor::findWaveClip(projectRef(), trackId, time);
    if (!clip) {
        return;
    }

    secs_t audioDataStartTime = clip->Start();
    secs_t audioDataEndTime = clip->End();

    setDataSelectedStartTime(audioDataStartTime, true);
    setDataSelectedEndTime(audioDataEndTime, true);
}

// data selection

void Au3SelectionController::resetDataSelection()
{
    MYLOG() << "resetDataSelection";

    const auto playbackPosition = playback()->player()->playbackPosition();
    m_selectedStartTime.set(playbackPosition, true);
    m_selectedEndTime.set(playbackPosition, true);
}

bool Au3SelectionController::timeSelectionIsNotEmpty() const
{
    return muse::RealIsEqualOrMore(m_selectedEndTime.val, 0.0) && !muse::RealIsEqualOrLess(
        m_selectedEndTime.val, m_selectedStartTime.val);
}

bool Au3SelectionController::isDataSelectedOnTrack(TrackId trackId) const
{
    return muse::contains(m_selectedTracks.val, trackId) && timeSelectionIsNotEmpty();
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

bool Au3SelectionController::selectionContainsGroup() const
{
    const auto clipKeyList = selectedClips();
    if (clipKeyList.empty()) {
        return false;
    }

    for (const auto& clipKey : clipKeyList) {
        WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        if (clip->GetGroupId() != -1) {
            return true;
        }
    }

    return false;
}

bool Au3SelectionController::isSelectionGrouped() const
{
    const auto clipKeyList = selectedClips();
    if (clipKeyList.empty()) {
        return false;
    }

    std::optional<int64_t> selectionGroupId;

    for (const auto& clipKey : clipKeyList) {
        WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
        IF_ASSERT_FAILED(clip) {
            return false;
        }

        if (!selectionGroupId.has_value()) {
            selectionGroupId = clip->GetGroupId();
            continue;
        }

        if (clip->GetGroupId() != selectionGroupId.value()) {
            return false;
        }
    }

    //! NOTE: none of the clips are grouped
    if (selectionGroupId.value() == -1) {
        return false;
    }

    return true;
}

void Au3SelectionController::resetTimeSelection()
{
    resetDataSelection();
    resetSelectedClips();
}

muse::async::Channel<au::trackedit::secs_t> Au3SelectionController::dataSelectedEndTimeChanged() const
{
    return m_selectedEndTime.changed;
}

muse::async::Channel<au::trackedit::secs_t> Au3SelectionController::dataSelectedEndTimeSelected() const
{
    return m_selectedEndTime.selected;
}

secs_t Au3SelectionController::selectionStartTime() const
{
    return m_selectionStartTime.val;
}

void Au3SelectionController::setSelectionStartTime(secs_t time)
{
    m_selectionStartTime.set(time, true);
}

void Au3SelectionController::updateSelectionController()
{
    auto& tracks = Au3TrackList::Get(projectRef());
    TrackIdList selectedTracks;
    for (const auto& selectedTrack : tracks.Selected()) {
        selectedTracks.push_back(selectedTrack->GetId());
    }

    m_selectedTracks.set(selectedTracks, true);
}

Au3Project& Au3SelectionController::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
