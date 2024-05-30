#pragma once

#include <memory>

#include "global/async/channel.h"
#include "global/async/promise.h"

#include "audiotypes.h"

//! NOTE Implemented in Au3Wrap
namespace au::playback {
class IPlayer
{
public:
    virtual ~IPlayer() = default;

    virtual void play() = 0;
    virtual void seek(const audio::secs_t newPosition) = 0;
    virtual void stop() = 0;
    virtual void pause() = 0;
    virtual void resume() = 0;
    virtual muse::async::Channel<audio::PlaybackStatus> playbackStatusChanged() const = 0;

    virtual muse::async::Promise<bool> setLoop(const audio::secs_t from, const audio::secs_t to) = 0;
    virtual void resetLoop() = 0;

    virtual muse::async::Promise<audio::secs_t> playbackPosition() const = 0;
    virtual muse::async::Channel<audio::secs_t> playbackPositionChanged() const = 0;
};

using IPlayerPtr = std::shared_ptr<IPlayer>;
}
