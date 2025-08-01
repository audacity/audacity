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
    MOCK_METHOD(void, removeClipSelection, (const trackedit::ClipKey&), (override));
    MOCK_METHOD(muse::async::Channel<TrackIdList>, tracksSelected, (), (const, override));

    MOCK_METHOD(void, resetSelectedClips, (), (override));
    MOCK_METHOD(bool, hasSelectedClips, (), (const, override));
    MOCK_METHOD(ClipKeyList, selectedClips, (), (const, override));
    MOCK_METHOD(ClipKeyList, selectedClipsInTrackOrder, (), (const, override));
    MOCK_METHOD(void, setSelectedClips, (const ClipKeyList&, bool), (override));
    MOCK_METHOD(void, addSelectedClip, (const ClipKey& clipKey), (override));
    MOCK_METHOD(muse::async::Channel<ClipKeyList>, clipsSelected, (), (const, override));
    MOCK_METHOD(double, selectedClipStartTime, (), (const, override));
    MOCK_METHOD(double, selectedClipEndTime, (), (const, override));
    MOCK_METHOD(double, leftMostSelectedClipStartTime, (), (const, override));
    MOCK_METHOD(double, rightMostSelectedClipEndTime, (), (const, override));

    MOCK_METHOD(void, setSelectedTrackAudioData, (TrackId), (override));
    MOCK_METHOD(void, resetDataSelection, (), (override));
    MOCK_METHOD(bool, timeSelectionIsNotEmpty, (), (const, override));
    MOCK_METHOD(bool, isDataSelectedOnTrack, (TrackId), (const, override));
    MOCK_METHOD(void, setSelectedAllAudioData, (), (override));

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

    MOCK_METHOD(bool, selectionContainsGroup, (), (const, override));
    MOCK_METHOD(bool, isSelectionGrouped, (), (const, override));

    MOCK_METHOD(void, resetTimeSelection, (), (override));

    MOCK_METHOD(TrackId, focusedTrack, (), (const, override));
    MOCK_METHOD(void, setFocusedTrack, (TrackId trackId), (override));
    MOCK_METHOD(muse::async::Channel<trackedit::TrackId>, focusedTrackChanged, (), (const, override));
};
}
