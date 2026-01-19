/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "trackedit/internal/itracknavigationcontroller.h"

namespace au::trackedit {
class TrackNavigationControllerMock : public ITrackNavigationController
{
public:
    MOCK_METHOD(TrackId, focusedTrack, (), (const, override));
    MOCK_METHOD(void, setFocusedTrack, (TrackId trackId), (override));
    MOCK_METHOD(muse::async::Channel<TrackId>, focusedTrackChanged, (), (const, override));

    MOCK_METHOD(void, focusTrackByIndex, (const muse::actions::ActionData& args), (override));
    MOCK_METHOD(void, focusPrevTrack, (), (override));
    MOCK_METHOD(void, focusNextTrack, (), (override));

    MOCK_METHOD(void, setFocusedItem, (const TrackItemKey& key), (override));
    MOCK_METHOD(muse::async::Channel<TrackItemKey>, focusedItemChanged, (), (const, override));

    MOCK_METHOD(void, navigateUp, (const muse::actions::ActionData& args), (override));
    MOCK_METHOD(void, navigateDown, (const muse::actions::ActionData& args), (override));
    MOCK_METHOD(void, trackRangeSelection, (), (override));
    MOCK_METHOD(void, toggleSelectionOnFocusedTrack, (), (override));
    MOCK_METHOD(void, multiSelectionUp, (), (override));
    MOCK_METHOD(void, multiSelectionDown, (), (override));

    MOCK_METHOD(muse::async::Channel<TrackItemKey>, openContextMenuRequested, (), (const, override));
};
}
