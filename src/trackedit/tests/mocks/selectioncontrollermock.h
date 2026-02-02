/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "trackedit/iselectioncontroller.h"

namespace au::trackedit {
class SelectionControllerMock : public ISelectionController
{
public:
    MOCK_METHOD(void, resetSelectedTracks, (), (override));
    MOCK_METHOD(TrackIdList, selectedTracks, (), (const, override));
    MOCK_METHOD(void, setSelectedTracks, (const TrackIdList&, bool), (override));
    MOCK_METHOD(muse::async::Channel<TrackIdList>, selectedTracksChanged, (), (const, override));
    MOCK_METHOD(void, removeClipSelection, (const trackedit::ClipKey&), (override));
    MOCK_METHOD(muse::async::Channel<TrackIdList>, tracksSelected, (), (const, override));

    MOCK_METHOD(void, resetSelectedClips, (), (override));
    MOCK_METHOD(bool, hasSelectedClips, (), (const, override));
    MOCK_METHOD(ClipKeyList, selectedClips, (), (const, override));
    MOCK_METHOD(ClipKeyList, selectedClipsInTrackOrder, (), (const, override));
    MOCK_METHOD(void, setSelectedClips, (const ClipKeyList&, bool), (override));
    MOCK_METHOD(void, addSelectedClip, (const ClipKey& clipKey), (override));
    MOCK_METHOD(muse::async::Channel<ClipKeyList>, clipsSelected, (), (const, override));
    MOCK_METHOD(std::optional<secs_t>, selectedClipStartTime, (), (const, override));
    MOCK_METHOD(std::optional<secs_t>, selectedClipEndTime, (), (const, override));
    MOCK_METHOD(std::optional<secs_t>, leftMostSelectedClipStartTime, (), (const, override));
    MOCK_METHOD(std::optional<secs_t>, rightMostSelectedClipEndTime, (), (const, override));

    MOCK_METHOD(void, resetSelectedLabels, (), (override));
    MOCK_METHOD(bool, hasSelectedLabels, (), (const, override));
    MOCK_METHOD(LabelKeyList, selectedLabels, (), (const, override));
    MOCK_METHOD(LabelKeyList, selectedLabelsInTrackOrder, (), (const, override));
    MOCK_METHOD(void, setSelectedLabels, (const LabelKeyList&, bool), (override));
    MOCK_METHOD(void, addSelectedLabel, (const LabelKey& LabelKey), (override));
    MOCK_METHOD(void, removeLabelSelection, (const LabelKey& labelKey), (override));
    MOCK_METHOD(muse::async::Channel<LabelKeyList>, labelsSelected, (), (const, override));

    MOCK_METHOD(std::optional<secs_t>, selectedLabelStartTime, (), (const, override));
    MOCK_METHOD(std::optional<secs_t>, selectedLabelEndTime, (), (const, override));

    MOCK_METHOD(std::optional<secs_t>, leftMostSelectedLabelStartTime, (), (const, override));
    MOCK_METHOD(std::optional<secs_t>, rightMostSelectedLabelEndTime, (), (const, override));

    MOCK_METHOD(std::optional<secs_t>, leftMostSelectedItemStartTime, (), (const, override));
    MOCK_METHOD(std::optional<secs_t>, rightMostSelectedItemEndTime, (), (const, override));

    MOCK_METHOD(std::optional<secs_t>, selectedTracksStartTime, (), (const, override));
    MOCK_METHOD(std::optional<secs_t>, selectedTracksEndTime, (), (const, override));

    MOCK_METHOD(void, setSelectedTrackAudioData, (TrackId), (override));
    MOCK_METHOD(void, resetDataSelection, (), (override));
    MOCK_METHOD(bool, timeSelectionIsNotEmpty, (), (const, override));
    MOCK_METHOD(bool, timeSelectionHasAudioData, (), (const, override));
    MOCK_METHOD(bool, isDataSelectedOnTrack, (TrackId), (const, override));
    MOCK_METHOD(void, setSelectedAllAudioData, (const std::optional<secs_t>&, const std::optional<secs_t>&), (override));
    MOCK_METHOD(ClipKeyList, clipsIntersectingRangeSelection, (), (const, override));
    MOCK_METHOD(void, setClipsIntersectingRangeSelection, (const ClipKeyList& clipKeys), (override));
    MOCK_METHOD(muse::async::Channel<ClipKeyList>, clipsIntersectingRangeSelectionChanged, (), (const, override));
    MOCK_METHOD(LabelKeyList, labelsIntersectingRangeSelection, (), (const, override));
    MOCK_METHOD(void, setLabelsIntersectingRangeSelection, (const LabelKeyList& labelKeys), (override));

    MOCK_METHOD(secs_t, dataSelectedStartTime, (), (const, override));
    MOCK_METHOD(void, setDataSelectedStartTime, (secs_t, bool), (override));
    MOCK_METHOD(muse::async::Channel<secs_t>, dataSelectedStartTimeChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<secs_t>, dataSelectedStartTimeSelected, (), (const, override));

    MOCK_METHOD(secs_t, dataSelectedEndTime, (), (const, override));
    MOCK_METHOD(void, setDataSelectedEndTime, (secs_t, bool), (override));
    MOCK_METHOD(muse::async::Channel<secs_t>, dataSelectedEndTimeChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<secs_t>, dataSelectedEndTimeSelected, (), (const, override));

    MOCK_METHOD(trackedit::secs_t, selectionStartTime, (), (const override));
    MOCK_METHOD(void, setSelectionStartTime, (trackedit::secs_t), (override));

    using FrequencySelection = std::pair<double, double>;
    MOCK_METHOD(FrequencySelection, frequencySelection, (trackedit::TrackId trackId), (const, override));
    MOCK_METHOD(void, setFrequencySelection, (trackedit::TrackId, const FrequencySelection&), (override));
    MOCK_METHOD(void, resetFrequencySelection, (), (override));
    MOCK_METHOD(muse::async::Channel<trackedit::TrackId>, frequencySelectionChanged, (), (const, override));

    MOCK_METHOD(bool, selectionContainsGroup, (), (const, override));
    MOCK_METHOD(bool, isSelectionGrouped, (), (const, override));

    MOCK_METHOD(void, resetTimeSelection, (), (override));

    MOCK_METHOD(TrackId, focusedTrack, (), (const, override));
    MOCK_METHOD(void, setFocusedTrack, (TrackId trackId), (override));
    MOCK_METHOD(muse::async::Channel<trackedit::TrackId>, focusedTrackChanged, (), (const, override));

    MOCK_METHOD(void, focusPreviousTrack, (), (override));
    MOCK_METHOD(void, focusNextTrack, (), (override));
    MOCK_METHOD(void, focusTrackByIndex, (int index), (override));

    MOCK_METHOD(int, trackDistance, (TrackId previous, TrackId next), (const, override));
    MOCK_METHOD(TrackIdList, orderedTrackList, (), (const, override));
};
}
