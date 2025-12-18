/*
* Audacity: A Digital Audio Editor
*/
#include "au3selectioncontroller.h"
#include "selectionrestorer.h"

#include "global/containers.h"
#include "global/realfn.h"

#include "au3-track/Track.h"
#include "au3-time-frequency-selection/ViewInfo.h"
#include "au3-track-selection/TrackFocus.h"

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
            resetSelectedLabels();
            resetSelectedTracks();

            //! NOTE: load project's last saved selection state
            auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
            m_selectedStartTime.set(selectedRegion.t0(), true);
            m_selectedEndTime.set(selectedRegion.t1(), true);
            m_spectralSelectionStartFrequency.set(selectedRegion.f0(), true);
            m_spectralSelectionEndFrequency.set(selectedRegion.f1(), true);

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

            auto& tracks = ::TrackList::Get(projectRef());
            if (!tracks.empty()) {
                const auto it = tracks.begin();
                setFocusedTrack(TrackId((*it)->GetId()));
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

    std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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

    std::shared_ptr<WaveClip> clip = au3::DomAccessor::findWaveClip(waveTrack, clipKey.itemId);
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

    if (endTime.has_value()) {
        return endTime.value();
    }

    return -1.0;
}

// label selection

void Au3SelectionController::resetSelectedLabels()
{
    MYLOG() << "resetSelectedLabels";
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

double Au3SelectionController::selectedLabelStartTime() const
{
    auto labelKeyList = selectedLabels();
    if (labelKeyList.empty()) {
        return -1.0;
    }

    auto labelKey = labelKeyList.at(0);

    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), ::TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return -1.0;
    }

    const Au3Label* label = DomAccessor::findLabel(labelTrack, labelKey.itemId);
    if (!label) {
        return -1.0;
    }

    return label->getT0();
}

double Au3SelectionController::selectedLabelEndTime() const
{
    auto labelKeyList = selectedLabels();
    if (labelKeyList.empty()) {
        return -1.0;
    }

    auto labelKey = labelKeyList.at(0);

    Au3LabelTrack* labelTrack = DomAccessor::findLabelTrack(projectRef(), ::TrackId(labelKey.trackId));
    IF_ASSERT_FAILED(labelTrack) {
        return -1.0;
    }

    const Au3Label* label = DomAccessor::findLabel(labelTrack, labelKey.itemId);
    if (!label) {
        return -1.0;
    }

    return label->getT1();
}

double Au3SelectionController::leftMostSelectedLabelStartTime() const
{
    std::optional<double> startTime;
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

    if (startTime.has_value()) {
        return startTime.value();
    }

    return -1.0;
}

double Au3SelectionController::rightMostSelectedLabelEndTime() const
{
    std::optional<double> endTime;
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

    const auto initialPlaybackPosition = globalContext()->playbackState()->playbackPosition();
    m_selectedStartTime.set(initialPlaybackPosition, true);
    m_selectedEndTime.set(initialPlaybackPosition, true);

    resetSpectralSelection();
    setClipsIntersectingRangeSelection({});
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

    if (complete) {
        setClipsIntersectingRangeSelection(findClipsIntersectingRangeSelection());
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
    MYLOG() << "end time: " << time << ", complete: " << complete;

    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setT1(time);

    m_selectedEndTime.set(time, complete);

    if (complete) {
        setClipsIntersectingRangeSelection(findClipsIntersectingRangeSelection());
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

au::trackedit::TrackId Au3SelectionController::focusedTrack() const
{
    return m_focusedTrack.val;
}

void Au3SelectionController::setFocusedTrack(TrackId trackId)
{
    m_focusedTrack.set(trackId, true);
}

muse::async::Channel<au::trackedit::TrackId> Au3SelectionController::focusedTrackChanged() const
{
    return m_focusedTrack.changed;
}

void Au3SelectionController::focusPreviousTrack()
{
    const au::trackedit::TrackId currentFocusedTrack = focusedTrack();

    Au3Track* au3FocusedTrack = au3::DomAccessor::findTrack(projectRef(), ::TrackId(currentFocusedTrack));
    if (!au3FocusedTrack) {
        return;
    }

    auto& tracks = Au3TrackList::Get(projectRef());
    auto currentIter = tracks.Find(au3FocusedTrack);
    if (currentIter != tracks.begin()) {
        --currentIter;
        if (*currentIter) {
            const TrackId trackId = TrackId((*currentIter)->GetId());
            setFocusedTrack(trackId);
        }
    }
}

void Au3SelectionController::focusNextTrack()
{
    const au::trackedit::TrackId currentFocusedTrack = focusedTrack();

    Au3Track* au3FocusedTrack = au3::DomAccessor::findTrack(projectRef(), ::TrackId(currentFocusedTrack));
    if (!au3FocusedTrack) {
        return;
    }

    auto& tracks = Au3TrackList::Get(projectRef());
    auto currentIter = tracks.Find(au3FocusedTrack);
    if (currentIter != tracks.end()) {
        ++currentIter;
        if (currentIter != tracks.end() && *currentIter) {
            const TrackId trackId = TrackId((*currentIter)->GetId());
            setFocusedTrack(trackId);
        }
    }
}

void Au3SelectionController::focusTrackByIndex(int index)
{
    if (index < 0) {
        return;
    }

    auto& tracks = Au3TrackList::Get(projectRef());
    auto iter = tracks.begin();
    std::advance(iter, index);
    if (iter != tracks.end() && *iter) {
        const TrackId trackId = TrackId((*iter)->GetId());
        setFocusedTrack(trackId);
    }
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

// spectral selection

double Au3SelectionController::spectralSelectionStartFrequency() const
{
    return m_spectralSelectionStartFrequency.val;
}

void Au3SelectionController::setSpectralSelectionStartFrequency(double frequency, bool complete)
{
    MYLOG() << "spectral start frequency: " << frequency << ", complete: " << complete;

    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setF0(frequency);

    m_spectralSelectionStartFrequency.set(frequency, complete);
}

muse::async::Channel<double> Au3SelectionController::spectralSelectionStartFrequencyChanged() const
{
    return m_spectralSelectionStartFrequency.changed;
}

double Au3SelectionController::spectralSelectionEndFrequency() const
{
    return m_spectralSelectionEndFrequency.val;
}

void Au3SelectionController::setSpectralSelectionEndFrequency(double frequency, bool complete)
{
    MYLOG() << "spectral end frequency: " << frequency << ", complete: " << complete;

    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setF1(frequency);

    m_spectralSelectionEndFrequency.set(frequency, complete);
}

muse::async::Channel<double> Au3SelectionController::spectralSelectionEndFrequencyChanged() const
{
    return m_spectralSelectionEndFrequency.changed;
}

bool Au3SelectionController::hasSpectralSelection() const
{
    constexpr double UndefinedFrequency = -1.0;
    return m_spectralSelectionStartFrequency.val != UndefinedFrequency
           && m_spectralSelectionEndFrequency.val != UndefinedFrequency
           && m_spectralSelectionStartFrequency.val != m_spectralSelectionEndFrequency.val;
}

void Au3SelectionController::resetSpectralSelection()
{
    MYLOG() << "resetSpectralSelection";

    constexpr double UndefinedFrequency = -1.0;
    
    auto& selectedRegion = ViewInfo::Get(projectRef()).selectedRegion;
    selectedRegion.setF0(UndefinedFrequency);
    selectedRegion.setF1(UndefinedFrequency);

    m_spectralSelectionStartFrequency.set(UndefinedFrequency, true);
    m_spectralSelectionEndFrequency.set(UndefinedFrequency, true);
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
