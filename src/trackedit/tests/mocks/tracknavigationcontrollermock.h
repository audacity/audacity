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
    MOCK_METHOD(bool, isNavigationEnabled, (), (const, override));
    MOCK_METHOD(void, setIsNavigationActive, (bool), (override));
    MOCK_METHOD(muse::async::Notification, isNavigationActiveChanged, (), (const, override));

    MOCK_METHOD(TrackId, focusedTrack, (), (const, override));
    MOCK_METHOD((muse::async::Channel<TrackId, bool>), focusedTrackChanged, (), (const, override));

    MOCK_METHOD(TrackFocus, focus, (), (const, override));
    MOCK_METHOD(void, setFocus, (const TrackFocus& focus, bool), (override));
    MOCK_METHOD((muse::async::Channel<TrackFocus, bool>), focusChanged, (), (const, override));

    MOCK_METHOD(TrackItemKeyList, itemKeysInRange, (const TrackItemKey& anchor, const TrackItemKey& target), (const, override));

    MOCK_METHOD(void, resetNavigation, (), (override));

    MOCK_METHOD(muse::async::Channel<TrackItemKey>, openContextMenuRequested, (), (const, override));
    MOCK_METHOD(muse::async::Channel<TrackId>, openRulerContextMenuRequested, (), (const, override));
};
}
