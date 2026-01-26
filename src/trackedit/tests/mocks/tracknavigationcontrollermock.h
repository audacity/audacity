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
    MOCK_METHOD(void, setFocusedTrack, (const TrackId& trackId, bool), (override));
    MOCK_METHOD((muse::async::Channel<TrackId, bool>), focusedTrackChanged, (), (const, override));

    MOCK_METHOD(TrackItemKey, focusedItem, (), (const, override));
    MOCK_METHOD(void, setFocusedItem, (const TrackItemKey& key, bool), (override));
    MOCK_METHOD((muse::async::Channel<TrackItemKey, bool>), focusedItemChanged, (), (const, override));

    MOCK_METHOD(void, trackRangeSelection, (), (override));
    MOCK_METHOD(void, toggleSelectionOnFocusedTrack, (), (override));
    MOCK_METHOD(void, multiSelectionUp, (), (override));
    MOCK_METHOD(void, multiSelectionDown, (), (override));

    MOCK_METHOD(muse::async::Channel<TrackItemKey>, openContextMenuRequested, (), (const, override));
};
}
