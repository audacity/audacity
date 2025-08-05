/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "global/async/channel.h"
#include "playback/playbacktypes.h"

namespace au::context {
//! NOTE The current player is in the global context.
//! We need to get the current state from it (playing position, status)
//! But direct manage (play, seek..) is bad idea, need send actions (to playback controller)
//! So, this interface limits the player interface to the context so that it cannot be misused.
class IPlaybackContext
{
public:
    virtual ~IPlaybackContext() = default;

    virtual playback::PlaybackStatus playbackStatus() const = 0;
    virtual bool isPlaying() const = 0;
    virtual muse::async::Channel<playback::PlaybackStatus> playbackStatusChanged() const = 0;

    virtual muse::secs_t playbackPosition() const = 0;
    virtual muse::async::Channel<muse::secs_t> playbackPositionChanged() const = 0;
};

using IPlaybackContextPtr = std::shared_ptr<IPlaybackContext>;
}
