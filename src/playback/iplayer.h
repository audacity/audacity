#pragma once

#include <memory>

#include "global/types/ret.h"
#include "global/types/retval.h"
#include "global/async/channel.h"
#include "global/async/promise.h"
#include "global/async/notification.h"

#include "playbacktypes.h"

class TrackList;
namespace au::playback {
class IPlayer
{
public:
    virtual ~IPlayer() = default;

    virtual bool isBusy() const = 0;

    virtual void play() = 0;
    virtual void seek(const muse::secs_t newPosition, bool applyIfPlaying = false) = 0;
    virtual void rewind() = 0;
    virtual void stop() = 0;
    virtual void pause() = 0;
    virtual void resume() = 0;

    virtual bool isRunning() const = 0;
    virtual PlaybackStatus playbackStatus() const = 0;
    virtual muse::async::Channel<PlaybackStatus> playbackStatusChanged() const = 0;
    virtual muse::ValNt<bool> reachedEnd() const = 0;

    virtual PlaybackRegion playbackRegion() const = 0;
    virtual void setPlaybackRegion(const PlaybackRegion& region) = 0;

    virtual muse::async::Promise<bool> setLoop(const muse::secs_t from, const muse::secs_t to) = 0;
    virtual void resetLoop() = 0;

    virtual muse::secs_t playbackPosition() const = 0;
    virtual muse::async::Channel<muse::secs_t> playbackPositionChanged() const = 0;

    // tracks
    virtual muse::Ret playTracks(TrackList& trackList, double startTime, double endTime, const PlayTracksOptions& options = {}) = 0;
};

using IPlayerPtr = std::shared_ptr<IPlayer>;
}
