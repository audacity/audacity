/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "playback/iplayer.h"

namespace au::playback {
class PlayerMock : public IPlayer
{
public:
    MOCK_METHOD(bool, isBusy, (), (const, override));
    MOCK_METHOD(void, play, (), (override));
    MOCK_METHOD(void, seek, (const muse::secs_t, bool), (override));
    MOCK_METHOD(void, rewind, (), (override));
    MOCK_METHOD(void, stop, (), (override));
    MOCK_METHOD(void, pause, (), (override));
    MOCK_METHOD(void, resume, (), (override));

    MOCK_METHOD(bool, isRunning, (), (const, override));
    MOCK_METHOD(PlaybackStatus, playbackStatus, (), (const, override));
    MOCK_METHOD(muse::async::Channel<PlaybackStatus>, playbackStatusChanged, (), (const, override));
    MOCK_METHOD(muse::ValNt<bool>, reachedEnd, (), (const, override));

    MOCK_METHOD(PlaybackRegion, playbackRegion, (), (const, override));
    MOCK_METHOD(void, setPlaybackRegion, (const PlaybackRegion&), (override));

    MOCK_METHOD(PlaybackRegion, loopRegion, (), (const, override));
    MOCK_METHOD(void, loopEditingBegin, (), (override));
    MOCK_METHOD(void, loopEditingEnd, (), (override));
    MOCK_METHOD(void, setLoopRegion, (const PlaybackRegion&), (override));
    MOCK_METHOD(void, setLoopRegionStart, (const muse::secs_t time), (override));
    MOCK_METHOD(void, setLoopRegionEnd, (const muse::secs_t time), (override));

    MOCK_METHOD(void, clearLoopRegion, (), (override));
    MOCK_METHOD(muse::async::Notification, loopRegionChanged, (), (const, override));

    MOCK_METHOD(bool, isLoopRegionClear, (), (const, override));

    MOCK_METHOD(void, setLoopRegionActive, (const bool), (override));
    MOCK_METHOD(bool, isLoopRegionActive, (), (const, override));

    MOCK_METHOD(muse::secs_t, playbackPosition, (), (const, override));
    MOCK_METHOD(void, updatePlaybackPosition, (), (override));
    MOCK_METHOD(muse::async::Channel<muse::secs_t>, playbackPositionChanged, (), (const, override));

    MOCK_METHOD(muse::Ret, playTracks, (TrackList&, double, double, const PlayTracksOptions&), (override));
};
}
