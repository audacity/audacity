/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "playback/iplaybackcontroller.h"

namespace au::playback {
class PlaybackControllerMock : public IPlaybackController
{
public:
    MOCK_METHOD(bool, isPlayAllowed, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isPlayAllowedChanged, (), (const, override));

    MOCK_METHOD(bool, isPlaying, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isPlayingChanged, (), (const, override));
    MOCK_METHOD(PlaybackStatus, playbackStatus, (), (const, override));

    MOCK_METHOD(PlaybackRegion, loopRegion, (), (const, override));
    MOCK_METHOD(void, loopEditingBegin, (), (override));
    MOCK_METHOD(void, loopEditingEnd, (), (override));
    MOCK_METHOD(void, setLoopRegion, (const PlaybackRegion&), (override));
    MOCK_METHOD(void, setLoopRegionStart, (const muse::secs_t), (override));
    MOCK_METHOD(void, setLoopRegionEnd, (const muse::secs_t), (override));
    MOCK_METHOD(void, clearLoopRegion, (), (override));
    MOCK_METHOD(bool, isLoopRegionClear, (), (const, override));
    MOCK_METHOD(muse::async::Notification, loopRegionChanged, (), (const, override));

    MOCK_METHOD(bool, isLoopRegionActive, (), (const, override));
    MOCK_METHOD(void, setLoopRegionActive, (const bool), (override));
    MOCK_METHOD(void, toggleLoopPlayback, (), (override));

    MOCK_METHOD(bool, isPaused, (), (const, override));
    MOCK_METHOD(bool, isStopped, (), (const, override));

    MOCK_METHOD(void, stop, (), (override));
    MOCK_METHOD(void, stopSeekAndUpdatePlaybackRegion, (), (override));

    MOCK_METHOD(muse::async::Channel<uint32_t>, midiTickPlayed, (), (const, override));

    MOCK_METHOD(muse::async::Channel<playback::TrackId>, trackAdded, (), (const, override));
    MOCK_METHOD(muse::async::Channel<playback::TrackId>, trackRemoved, (), (const, override));

    MOCK_METHOD(bool, actionChecked, (const muse::actions::ActionCode&), (const, override));
    MOCK_METHOD(muse::async::Channel<muse::actions::ActionCode>, actionCheckedChanged, (), (const, override));

    MOCK_METHOD(muse::secs_t, totalPlayTime, (), (const, override));
    MOCK_METHOD(muse::async::Notification, totalPlayTimeChanged, (), (const, override));

    MOCK_METHOD(muse::secs_t, lastPlaybackSeekTime, (), (const, override));
    MOCK_METHOD(void, setLastPlaybackSeekTime, (muse::secs_t), (override));
    MOCK_METHOD(muse::async::Notification, lastPlaybackSeekTimeChanged, (), (const, override));

    MOCK_METHOD(muse::Progress, loadingProgress, (), (const, override));
};
}
