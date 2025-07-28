/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "context/iplaybackstate.h"

namespace au::context {
class PlaybackStateMock : public IPlaybackState
{
public:
    MOCK_METHOD(playback::PlaybackStatus, playbackStatus, (), (const, override));
    MOCK_METHOD(bool, isPlaying, (), (const, override));
    MOCK_METHOD(muse::async::Channel<playback::PlaybackStatus>, playbackStatusChanged, (), (const, override));

    MOCK_METHOD(muse::secs_t, playbackPosition, (), (const, override));
    MOCK_METHOD(muse::async::Channel<muse::secs_t>, playbackPositionChanged, (), (const, override));
};
}
