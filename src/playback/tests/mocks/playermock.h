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

    MOCK_METHOD(muse::async::Promise<bool>, setLoop, (const muse::secs_t from, const muse::secs_t to), (override));
    MOCK_METHOD(void, resetLoop, (), (override));

    MOCK_METHOD(muse::secs_t, playbackPosition, (), (const, override));
    MOCK_METHOD(muse::async::Channel<muse::secs_t>, playbackPositionChanged, (), (const, override));

    MOCK_METHOD(muse::Ret, playTracks, (TrackList&, double, double, const PlayTracksOptions&), (override));
};
}
