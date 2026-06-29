#pragma once

#include <memory>
#include <string>

#include "framework/global/types/ret.h"
#include "framework/global/types/retval.h"
#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"

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

    virtual muse::async::Notification loopRegionChanged() const = 0;
    virtual PlaybackRegion loopRegion() const = 0;
    virtual void loopEditingBegin() = 0;
    virtual void loopEditingEnd() = 0;
    virtual void setLoopRegion(const PlaybackRegion& region) = 0;
    virtual void setLoopRegionStart(const muse::secs_t time) = 0;
    virtual void setLoopRegionEnd(const muse::secs_t time) = 0;

    virtual void clearLoopRegion() = 0;
    virtual bool isLoopRegionClear() const = 0;

    virtual void setLoopRegionActive(const bool active) = 0;
    virtual bool isLoopRegionActive() const = 0;

    /*!
     * \brief Consumer side of a single-producer/single-consumer lock-free queue.
     * In other words, implementation assumes a single caller to this method on a single thread, whatever thread this might be.
     * Beside consuming from a lock-free queue, the implementation also ensures that it does not involve locks elsewhere, making
     * it suitable for time-critical contexts.
     */
    virtual void updatePlaybackPosition() = 0;

    virtual muse::secs_t playbackPosition() const = 0;
    virtual muse::async::Channel<muse::secs_t> playbackPositionChanged() const = 0;

    // tracks
    virtual muse::Ret playTracks(TrackList& trackList, double startTime, double endTime, const PlayTracksOptions& options = {}) = 0;

    // session status
    virtual bool isPlaying() const = 0;
    virtual bool isPaused() const = 0;
    virtual bool isStopped() const = 0;
    virtual muse::async::Notification isPlayingChanged() const = 0;
    virtual bool isPlayAllowed() const = 0;
    virtual muse::async::Notification isPlayAllowedChanged() const = 0;

    virtual muse::secs_t lastPlaybackSeekTime() const = 0;
    virtual void setLastPlaybackSeekTime(muse::secs_t secs) = 0;
    virtual muse::async::Notification lastPlaybackSeekTimeChanged() const = 0;
    virtual muse::secs_t totalPlayTime() const = 0;

    // High-level transport intents
    virtual void togglePlay(bool ignoreSelection) = 0;
    virtual void rewindToStart() = 0;
    virtual void rewindToEnd() = 0;
    virtual void seekTo(muse::secs_t secs, bool triggerPlay) = 0;
    virtual void changePlaybackRegion(muse::secs_t start, muse::secs_t end) = 0;
    virtual void stopSeekAndUpdatePlaybackRegion() = 0;
    virtual void toggleLoopPlayback() = 0;
    virtual void setLoopRegionToSelection() = 0;
    virtual void setSelectionToLoop() = 0;
    virtual void setLoopRegionInOut() = 0;
    virtual void setSelectionFollowsLoopRegion() = 0;

    // Audio device/configuration changes. Each can't be applied on an open
    // stream, so playback is stopped first and, if the user was actively
    // playing, resumed from the same position afterwards.
    virtual void setAudioApi(const std::string& api) = 0;
    virtual void setAudioOutputDevice(const std::string& device) = 0;
    virtual void setAudioInputDevice(const std::string& device) = 0;
    virtual void setInputChannels(int channels) = 0;
    virtual void rescanAudioDevices() = 0;
};

using IPlayerPtr = std::shared_ptr<IPlayer>;
}
