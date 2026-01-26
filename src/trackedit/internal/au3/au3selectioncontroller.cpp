/*
* Audacity: A Digital Audio Editor
*/
#include "au3selectioncontroller.h"

#include "global/containers.h"
#include "global/realfn.h"

#include "au3-track/Track.h"
#include "au3-time-frequency-selection/ViewInfo.h"

#include "au3wrap/internal/domconverter.h"
#include "au3wrap/au3types.h"
#include "au3wrap/internal/domaccessor.h"

#include "spectrogram/spectrogramtypes.h"

#include "framework/global/log.h"
#include "framework/global/defer.h"

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
            //! NOTE: load project's last saved selection state
            auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
            auto savedSelectedClips = au3::DomAccessor::findSelectedClips(projectRef());
            auto savedSelectedLabels = au3::DomAccessor::findSelectedLabels(projectRef());
            auto savedSelectedTracks = au3::DomAccessor::findSelectedTracks(projectRef());

            // Clip selection takes priority
            if (!savedSelectedClips.empty()) {
                selectedRegion.setTimes(0, 0);
            }

            m_selectedStartTime.set(selectedRegion.t0(), true);
            m_selectedEndTime.set(selectedRegion.t1(), true);

            TrackId focusedTrack = au3::DomAccessor::findFocusedTrack(projectRef());
            if (focusedTrack != INVALID_TRACK) {
                m_focusedTrack.set(focusedTrack, true);
            }

            setSelectedLabels(savedSelectedLabels, true);
            setSelectedClips(savedSelectedClips, true);

            if (savedSelectedClips.empty()) {
                setSelectedTracks(savedSelectedTracks, true);
            }

            setClipsIntersectingRangeSelection(findClipsIntersectingRangeSelection());
            setLabelsIntersectingRangeSelection(findLabelsIntersectingRangeSelection());

            projectHistory()->historyChanged().onNotify(this, [this]() {
                onUndoRedo();
            }, Asyncable::Mode::SetReplace);
        } else {
            m_tracksSubc.Reset();
        }
    });
}

void Au3SelectionController::onUndoRedo()
{
    // Resync controller state with the project state after undo/redo
    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    auto restoredSelectedClips = au3::DomAccessor::findSelectedClips(projectRef());
    auto restoredSelectedLabels = au3::DomAccessor::findSelectedLabels(projectRef());
    auto restoredSelectedTracks = au3::DomAccessor::findSelectedTracks(projectRef());
    auto restoredFocusedTrack = au3::DomAccessor::findFocusedTrack(projectRef());

    MYLOG() << "[SELECTION] onUndoRedo: time=" << selectedRegion.t0() << ":" << selectedRegion.t1()
            << " clips=" << restoredSelectedClips.size()
            << " labels=" << restoredSelectedLabels.size()
            << " tracks=" << restoredSelectedTracks.size()
            << " focusedTrack=" << restoredFocusedTrack;

    m_selectedStartTime.set(selectedRegion.t0(), true);
    m_selectedEndTime.set(selectedRegion.t1(), true);

    m_selectedClips.set(restoredSelectedClips, true);
    setSelectedLabels(restoredSelectedLabels, true);
    setSelectedTracks(restoredSelectedTracks, true);

    if (restoredFocusedTrack != INVALID_TRACK) {
        m_focusedTrack.set(restoredFocusedTrack, true);
    }

    updateSelectionController();

    setClipsIntersectingRangeSelection(findClipsIntersectingRangeSelection());
    setLabelsIntersectingRangeSelection(findLabelsIntersectingRangeSelection());
}

ClipKeyList Au3SelectionController::findClipsIntersectingRangeSelection() const
{
    if (!timeSelectionIsNotEmpty() || selectedTracks().empty()) {
        return {};
    }

    ClipKeyList clipsIntersectingRangeSelection = {};
    for (const auto& trackId : m_selectedTracks.val) {
        WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
        if (!waveTrack) {
            continue;
        }

        for (const auto& clip : waveTrack->GetSortedClipsIntersecting(m_selectedStartTime.val, m_selectedEndTime.val)) {
            clipsIntersectingRangeSelection.push_back(DomConverter::clip(waveTrack, clip.get()).key);
        }
    }

    return clipsIntersectingRangeSelection;
}

void Au3SelectionController::resetSelectedTracks()
{
    MYLOG() << "[SELECTION] resetSelectedTrack";

    setSelectedTracks({}, true);
}

TrackIdList Au3SelectionController::selectedTracks() const
{
    return m_selectedTracks.val;
}

void Au3SelectionController::setSelectedTracks(const TrackIdList& tracksIds, bool complete)
{
    MYLOG() << "[SELECTION] setSelectedTracks: " << tracksIds;

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

muse::async::Channel<TrackIdList> Au3SelectionController::selectedTracksChanged() const
{
    return m_selectedTracks.changed;
}

muse::async::Channel<TrackIdList> Au3SelectionController::tracksSelected() const
{
    return m_selectedTracks.selected;
}

void Au3SelectionController::resetSelectedClips()
{
    MYLOG() << "[SELECTION] resetSelectedClip";
    //! NOTE: sync clip deselection with au3 persistence
    au3::DomAccessor::clearAllClipSelection(projectRef());
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
    //! NOTE: sync clip selection with au3 persistence
    au3::DomAccessor::clearAllClipSelection(projectRef());
    for (const ClipKey& key : clipKeys) {
        au3::DomAccessor::setClipSelected(projectRef(), key, true);
    }

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
        au3::DomAccessor::setClipSelected(projectRef(), clipKey, true);

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

    au3::DomAccessor::setClipSelected(projectRef(), clipKey, false);

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

std::optional<secs_t> Au3SelectionController::selectedClipStartTime() const
{
    auto clipKeyList = selectedClips();
    if (clipKeyList.empty()) {
        return std::nullopt;
    }

    auto clipKey = clipKeyList.at(0);

    WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return std::nullopt;
    }

    std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    if (!clip) {
        return std::nullopt;
    }

    return clip->Start();
}

std::optional<secs_t> Au3SelectionController::selectedClipEndTime() const
{
    auto clipKeyList = selectedClips();
    if (clipKeyList.empty()) {
        return std::nullopt;
    }

    auto clipKey = clipKeyList.at(0);

    WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return std::nullopt;
    }

    std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
    if (!clip) {
        return std::nullopt;
    }

    return clip->End();
}

std::optional<secs_t> Au3SelectionController::leftMostSelectedClipStartTime() const
{
    std::optional<secs_t> startTime;
    for (const auto& selectedClip : selectedClips()) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
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

    return startTime;
}

std::optional<secs_t> Au3SelectionController::rightMostSelectedClipEndTime() const
{
    std::optional<secs_t> endTime;
    for (const auto& selectedClip : selectedClips()) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(selectedClip.trackId));
        IF_ASSERT_FAILED(waveTrack) {
            continue;
        }

        std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, selectedClip.itemId);
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

    return endTime;
}

// label selection

void Au3SelectionController::resetSelectedLabels()
{
    MYLOG() << "[SELECTION] resetSelectedLabels";
    au3::DomAccessor::clearAllLabelSelection(projectRef());
    m_selectedLabels.set(au::trackedit::LabelKeyList(), true);
}

bool Au3SelectionController::hasSelectedLabels() const
{
    return !m_selectedLabels.val.empty();
}

LabelKeyList Au3SelectionController::selectedLabels() const
{
    return m_selectedLabels.val;
}

LabelKeyList Au3SelectionController::selectedLabelsInTrackOrder() const
{
    LabelKeyList sortedSelectedLabels;
    auto& tracks = ::TrackList::Get(projectRef());
    for (const auto& track : tracks) {
        for (const auto& selectedLabel : m_selectedLabels.val) {
            if (TrackId(track->GetId()) == selectedLabel.trackId) {
                sortedSelectedLabels.push_back(selectedLabel);
            }
        }
    }

    return sortedSelectedLabels;
}

void Au3SelectionController::setSelectedLabels(const LabelKeyList& labelKeys, bool complete)
{
    au3::DomAccessor::clearAllLabelSelection(projectRef());
    for (const LabelKey& key : labelKeys) {
        au3::DomAccessor::setLabelSelected(projectRef(), key, true);
    }

    m_selectedLabels.set(labelKeys, complete);

    //! NOTE: when selecting a label, we also need to select
    //! the track on which the label is located
    TrackIdList selectedTracks;
    for (const LabelKey& key : labelKeys) {
        if (muse::contains(selectedTracks, key.trackId)) {
            continue;
        }
        selectedTracks.push_back(key.trackId);
    }
    setSelectedTracks(selectedTracks, complete);
}

void Au3SelectionController::addSelectedLabel(const LabelKey& labelKey)
{
    auto selectedLabels = m_selectedLabels.val;

    if (!muse::contains(selectedLabels, labelKey)) {
        selectedLabels.push_back(labelKey);
        au3::DomAccessor::setLabelSelected(projectRef(), labelKey, true);

        m_selectedLabels.set(selectedLabels, true);

        //! NOTE: when selecting a label, we also need to select
        //! the track on which the label is located
        addSelectedTrack(labelKey.trackId);
    }
}

void Au3SelectionController::removeLabelSelection(const LabelKey& labelKey)
{
    auto selectedLabels = m_selectedLabels.val;

    if (!muse::contains(selectedLabels, labelKey)) {
        return;
    }

    au3::DomAccessor::setLabelSelected(projectRef(), labelKey, false);

    selectedLabels.erase(
        std::remove(selectedLabels.begin(), selectedLabels.end(), labelKey),
        selectedLabels.end()
        );

    m_selectedLabels.set(selectedLabels, true);

    //! NOTE: update selected tracks
    TrackIdList selectedTracks;
    for (const LabelKey& key : selectedLabels) {
        if (muse::contains(selectedTracks, key.trackId)) {
            continue;
        }
        selectedTracks.push_back(key.trackId);
    }
    setSelectedTracks(selectedTracks, true);
}

muse::async::Channel<LabelKeyList> Au3SelectionController::labelsSelected() const
{
    return m_selectedLabels.selected;
}

std::optional<secs_t> Au3SelectionController::selectedLabelStartTime() const
{
    auto labelKeyList = selectedLabels();
    if (labelKeyList.empty()) {
        return std::nullopt;
    }

    auto labelKey = labelKeyList.at(0);

    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), ::TrackId(labelKey.trackId));
    if (!labelTrack) {
        return std::nullopt;
    }

    const Au3Label* label = DomAccessor::findLabel(labelTrack, labelKey.itemId);
    if (!label) {
        return std::nullopt;
    }

    return label->getT0();
}

std::optional<secs_t> Au3SelectionController::selectedLabelEndTime() const
{
    auto labelKeyList = selectedLabels();
    if (labelKeyList.empty()) {
        return std::nullopt;
    }

    auto labelKey = labelKeyList.at(0);

    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), ::TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return std::nullopt;
    }

    const Au3Label* label = DomAccessor::findLabel(labelTrack, labelKey.itemId);
    if (!label) {
        return std::nullopt;
    }

    return label->getT1();
}

std::optional<secs_t> Au3SelectionController::leftMostSelectedLabelStartTime() const
{
    std::optional<secs_t> startTime;
    for (const auto& selectedLabel : selectedLabels()) {
        Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(selectedLabel.trackId));
        IF_ASSERT_FAILED(labelTrack) {
            continue;
        }

        const Au3Label* label = DomAccessor::findLabel(labelTrack, selectedLabel.itemId);
        IF_ASSERT_FAILED(label) {
            continue;
        }

        if (!startTime.has_value()) {
            startTime = label->getT0();
            continue;
        }

        if (!muse::RealIsEqualOrMore(label->getT0(), startTime.value())) {
            startTime = label->getT0();
        }
    }

    return startTime;
}

std::optional<secs_t> Au3SelectionController::rightMostSelectedLabelEndTime() const
{
    std::optional<secs_t> endTime;
    for (const auto& selectedLabel : selectedLabels()) {
        Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(selectedLabel.trackId));
        IF_ASSERT_FAILED(labelTrack) {
            continue;
        }

        const Au3Label* label = DomAccessor::findLabel(labelTrack, selectedLabel.itemId);
        IF_ASSERT_FAILED(label) {
            continue;
        }

        if (!endTime.has_value()) {
            endTime = label->getT1();
            continue;
        }

        if (!muse::RealIsEqualOrLess(label->getT1(), endTime.value())) {
            endTime = label->getT1();
        }
    }

    return endTime;
}

std::optional<secs_t> Au3SelectionController::leftMostSelectedItemStartTime() const
{
    std::optional<secs_t> result = leftMostSelectedClipStartTime();

    std::optional<secs_t> labelStartTime = leftMostSelectedLabelStartTime();
    if (labelStartTime.has_value()) {
        if (!result.has_value() || labelStartTime.value() < result.value()) {
            result = labelStartTime;
        }
    }

    return result;
}

std::optional<secs_t> Au3SelectionController::rightMostSelectedItemEndTime() const
{
    std::optional<secs_t> result = rightMostSelectedClipEndTime();

    std::optional<secs_t> labelEndTime = rightMostSelectedLabelEndTime();
    if (labelEndTime.has_value()) {
        if (!result.has_value() || labelEndTime.value() > result.value()) {
            result = labelEndTime;
        }
    }

    return result;
}

std::optional<secs_t> Au3SelectionController::selectedTracksStartTime() const
{
    std::optional<secs_t> result;
    auto& tracks = ::TrackList::Get(projectRef());

    for (const auto& trackId : selectedTracks()) {
        ::Track* au3Track = tracks.FindById(::TrackId(trackId));
        if (!au3Track) {
            continue;
        }

        double trackStart = au3Track->GetStartTime();
        if (!result.has_value() || trackStart < result.value()) {
            result = trackStart;
        }
    }

    return result;
}

std::optional<secs_t> Au3SelectionController::selectedTracksEndTime() const
{
    std::optional<secs_t> result;
    auto& tracks = ::TrackList::Get(projectRef());

    for (const auto& trackId : selectedTracks()) {
        ::Track* au3Track = tracks.FindById(::TrackId(trackId));
        if (!au3Track) {
            continue;
        }

        double trackEnd = au3Track->GetEndTime();
        if (!result.has_value() || trackEnd > result.value()) {
            result = trackEnd;
        }
    }

    return result;
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
    MYLOG() << "[SELECTION] resetDataSelection";

    const auto initialPlaybackPosition = globalContext()->playbackState()->playbackPosition();
    m_selectedStartTime.set(initialPlaybackPosition, true);
    m_selectedEndTime.set(initialPlaybackPosition, true);

    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setTimes(0, 0);

    setClipsIntersectingRangeSelection({});
    setLabelsIntersectingRangeSelection({});
}

bool Au3SelectionController::timeSelectionIsNotEmpty() const
{
    return muse::RealIsEqualOrMore(m_selectedEndTime.val, 0.0) && !muse::RealIsEqualOrLess(
        m_selectedEndTime.val, m_selectedStartTime.val);
}

bool au::trackedit::Au3SelectionController::timeSelectionHasAudioData() const
{
    if (!timeSelectionIsNotEmpty() || m_selectedTracks.val.empty()) {
        return false;
    }

    for (const auto& trackId : m_selectedTracks.val) {
        WaveTrack* waveTrack = au3::DomAccessor::findWaveTrack(projectRef(), ::TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return false;
        }

        if (!waveTrack->IsEmpty(m_selectedStartTime.val, m_selectedEndTime.val)) {
            return true;
        }
    }

    return false;
}

bool Au3SelectionController::isDataSelectedOnTrack(TrackId trackId) const
{
    return muse::contains(m_selectedTracks.val, trackId) && timeSelectionIsNotEmpty();
}

void Au3SelectionController::setSelectedAllAudioData(const std::optional<secs_t>& fromTime, const std::optional<secs_t>& toTime)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    trackedit::TrackIdList tracks = prj->trackIdList();

    secs_t startTime = fromTime.has_value() ? fromTime.value() : secs_t(0.0);
    secs_t endTime = toTime.has_value() ? toTime.value() : prj->totalTime();

    // Clear item selection in favor of data selection
    resetSelectedClips();
    resetSelectedLabels();

    setSelectedTracks(tracks, true);
    setDataSelectedStartTime(startTime, true);
    setDataSelectedEndTime(endTime, true);
}

ClipKeyList Au3SelectionController::clipsIntersectingRangeSelection() const
{
    return m_clipsIntersectingRangeSelection.val;
}

void Au3SelectionController::setClipsIntersectingRangeSelection(const ClipKeyList& clipKeys)
{
    m_clipsIntersectingRangeSelection.set(clipKeys, true);
}

muse::async::Channel<ClipKeyList> Au3SelectionController::clipsIntersectingRangeSelectionChanged() const
{
    return m_clipsIntersectingRangeSelection.changed;
}

LabelKeyList Au3SelectionController::labelsIntersectingRangeSelection() const
{
    return m_labelsIntersectingRangeSelection.val;
}

void Au3SelectionController::setLabelsIntersectingRangeSelection(const LabelKeyList& labelKeys)
{
    m_labelsIntersectingRangeSelection.set(labelKeys, true);
}

LabelKeyList Au3SelectionController::findLabelsIntersectingRangeSelection() const
{
    if (!timeSelectionIsNotEmpty() || selectedTracks().empty()) {
        return {};
    }

    double startTime = m_selectedStartTime.val;
    double endTime = m_selectedEndTime.val;

    LabelKeyList result;
    for (const auto& trackId : m_selectedTracks.val) {
        Au3LabelTrack* labelTrack = au3::DomAccessor::findLabelTrack(projectRef(), ::TrackId(trackId));
        if (!labelTrack) {
            continue;
        }

        for (int i = 0; i < labelTrack->GetNumLabels(); ++i) {
            const Au3Label* label = labelTrack->GetLabel(i);
            if (!label) {
                continue;
            }

            double t0 = label->getT0();
            double t1 = label->getT1();

            bool intersects = false;
            if (muse::RealIsEqual(t0, t1)) {
                // point label
                intersects = muse::RealIsEqualOrMore(t0, startTime) && muse::RealIsEqualOrLess(t0, endTime);
            } else {
                // range label
                intersects = t0 < endTime && t1 > startTime;
            }

            if (intersects) {
                result.push_back(LabelKey(trackId, label->GetId()));
            }
        }
    }

    return result;
}

au::trackedit::secs_t Au3SelectionController::dataSelectedStartTime() const
{
    return m_selectedStartTime.val;
}

void Au3SelectionController::setDataSelectedStartTime(au::trackedit::secs_t time, bool complete)
{
    MYLOG() << "[SELECTION] setDataSelectedStartTime: " << time << ", complete: " << complete;

    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setT0(time, false);

    m_selectedStartTime.set(time, complete);

    if (complete) {
        setClipsIntersectingRangeSelection(findClipsIntersectingRangeSelection());
        setLabelsIntersectingRangeSelection(findLabelsIntersectingRangeSelection());
        projectHistory()->modifyState(false);
    }
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
    MYLOG() << "[SELECTION] setDataSelectedEndTime: " << time << ", complete: " << complete;

    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setT1(time, false);

    m_selectedEndTime.set(time, complete);

    if (complete) {
        setClipsIntersectingRangeSelection(findClipsIntersectingRangeSelection());
        setLabelsIntersectingRangeSelection(findLabelsIntersectingRangeSelection());
        projectHistory()->modifyState(false);
    }
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

        std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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

        std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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
    resetSelectedLabels();
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

std::pair<double, double> Au3SelectionController::frequencySelection(trackedit::TrackId trackId) const
{
    if (m_frequencySelection.has_value() && m_frequencySelection->trackId == static_cast<int>(trackId)) {
        return { m_frequencySelection->startFrequency, m_frequencySelection->endFrequency };
    }
    return { spectrogram::SelectionInfo::UndefinedFrequency, spectrogram::SelectionInfo::UndefinedFrequency };
}

void Au3SelectionController::setFrequencySelection(trackedit::TrackId trackId, const std::pair<double, double>& selection)
{
    TrackFrequencySelection trackFrequencySelection{ static_cast<int>(trackId), selection.first, selection.second };
    if (m_frequencySelection == trackFrequencySelection) {
        return;
    }

    const std::optional<trackedit::TrackId> previousTrackId
        = m_frequencySelection ? std::make_optional(m_frequencySelection->trackId) : std::nullopt;

    muse::Defer notifyPreviousTrack([this, trackId, previousTrackId]() {
        if (previousTrackId && previousTrackId != trackId) {
            m_frequencySelectionChanged.send(*previousTrackId);
        }
    });

    if (selection.first == spectrogram::SelectionInfo::UndefinedFrequency
        && selection.second == spectrogram::SelectionInfo::UndefinedFrequency) {
        m_frequencySelection.reset();
        return;
    }
    m_frequencySelection.emplace(trackFrequencySelection);
    m_frequencySelectionChanged.send(trackId);
}

void Au3SelectionController::resetFrequencySelection()
{
    if (!m_frequencySelection.has_value()) {
        return;
    }

    const trackedit::TrackId previousTrackId = m_frequencySelection->trackId;

    m_frequencySelection.reset();
    m_frequencySelectionChanged.send(previousTrackId);
}

muse::async::Channel<au::trackedit::TrackId> Au3SelectionController::frequencySelectionChanged() const
{
    return m_frequencySelectionChanged;
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

int Au3SelectionController::trackDistance(const TrackId previous, const TrackId next) const
{
    if (previous == next) {
        return 0;
    }

    const auto& tracks = Au3TrackList::Get(projectRef());
    auto prevIter = tracks.Find(au3::DomAccessor::findTrack(projectRef(), ::TrackId(previous)));
    auto nextIter = tracks.Find(au3::DomAccessor::findTrack(projectRef(), ::TrackId(next)));

    if (prevIter == tracks.end() || nextIter == tracks.end()) {
        return 0;
    }

    auto tempIter = prevIter;
    int forwardDistance = 0;
    while (tempIter != tracks.end() && tempIter != nextIter) {
        ++tempIter;
        ++forwardDistance;
    }

    if (tempIter == nextIter) {
        return forwardDistance;
    } else {
        return -std::distance(nextIter, prevIter);
    }
}

TrackIdList Au3SelectionController::orderedTrackList() const
{
    TrackIdList trackIds;
    auto& tracks = Au3TrackList::Get(projectRef());
    for (const auto& track : tracks) {
        trackIds.push_back(track->GetId());
    }
    return trackIds;
}

Au3Project& Au3SelectionController::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}
