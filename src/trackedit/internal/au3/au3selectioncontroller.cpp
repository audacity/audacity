/*
* Audacity: A Digital Audio Editor
*/
#include "au3selectioncontroller.h"
#include "selectionrestorer.h"

#include "global/containers.h"
#include "global/realfn.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-track-selection/TrackFocus.h"

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

        if (prj) {
            resetSelectedClips();
            resetSelectedTracks();

            //! NOTE: load project's last saved selection state
            auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
            m_selectedStartTime.set(selectedRegion.t0(), true);
            m_selectedEndTime.set(selectedRegion.t1(), true);

            auto& restorer = SelectionRestorer::Get(projectRef());
            restorer.selectionGetter = [this] {
                return ClipAndTimeSelection {
                    m_selectedClips.val,
                    m_selectedStartTime.val,
                    m_selectedEndTime.val
                };
            };

            restorer.selectionSetter = [this](const ClipAndTimeSelection& selection) {
                restoreSelection(selection);
            };

            auto vs = globalContext()->currentProject()->viewState();
            if (vs) {
                vs->updateClipsBoundaries(false);
            }
        } else {
            m_tracksSubc.Reset();
        }
    });
}

void Au3SelectionController::restoreSelection(const ClipAndTimeSelection& selection)
{
    MYLOG() << "restoreSelection";

    m_selectedClips.set(selection.selectedClips, true);
    m_selectedStartTime.set(selection.dataSelectedStartTime, true);
    m_selectedEndTime.set(selection.dataSelectedEndTime, true);
    updateSelectionController();
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

double Au3SelectionController::leftMostSelectedClipStartTime() const
{
    std::optional<double> startTime;
    for (const auto& selectedClip : selectedClips()) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        if (!startTime.has_value()) {
            startTime = clip->GetPlayStartTime();
            continue;
        }

        if (!muse::RealIsEqualOrMore(clip->GetPlayStartTime(), startTime.value())) {
            startTime = clip->GetPlayStartTime();
        }
    }

    if (startTime.has_value()) {
        return startTime.value();
    }

    return -1.0;
}

double Au3SelectionController::rightMostSelectedClipEndTime() const
{
    std::optional<double> endTime;
    for (const auto& selectedClip : selectedClips()) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.clipId);
        IF_ASSERT_FAILED(clip) {
            continue;
        }

        if (!endTime.has_value()) {
            endTime = clip->GetPlayEndTime();
            continue;
        }

        if (!muse::RealIsEqualOrLess(clip->GetPlayEndTime(), endTime.value())) {
            endTime = clip->GetPlayEndTime();
        }
    }

    if (endTime.has_value()) {
        return endTime.value();
    }

    return -1.0;
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

void Au3SelectionController::setSelectedAllAudioData()
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    trackedit::TrackIdList tracks = prj->trackIdList();

    setSelectedTracks(tracks, true);
    setDataSelectedStartTime(0.0, true);
    setDataSelectedEndTime(prj->totalTime(), true);
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

au::trackedit::TrackId Au3SelectionController::focusedTrack() const
{
    return m_focusedTrack.val;
}

void Au3SelectionController::setFocusedTrack(TrackId trackId)
{
    Au3Track* track = au3::DomAccessor::findTrack(projectRef(), ::TrackId(trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }
    auto& trackFocus = TrackFocus::Get(projectRef());
    trackFocus.SetFocus(track->shared_from_this());

    m_focusedTrack.set(trackId, true);
}

muse::async::Channel<au::trackedit::TrackId> Au3SelectionController::focusedTrackChanged() const
{
    return m_focusedTrack.changed;
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

    if (!selectedTracks.empty()) {
        setFocusedTrack(selectedTracks.front());
    }
}

Au3Project& Au3SelectionController::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
