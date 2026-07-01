/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/notification.h"
#include "framework/global/types/secs.h"

#include "playbacktypes.h"

namespace au::playback {
//! Coordinates the player (IPlayer) and the recorder (IRecordController): owns the
//! playback session state (cursor, play region, pause/resume bookmarks) and the
//! transport intents/policies that depend on BOTH subsystems — e.g. "playback is
//! allowed only when not recording" and "resume after a device change only if we
//! were actively playing". The au3 stream primitives stay on IPlayer; consumers
//! that only need raw player state (loop region, status) keep using
//! IPlayback::player() directly.
class ITransport : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITransport)

public:
    virtual ~ITransport() = default;

    // playback / recording coordination policy
    virtual bool isPlayAllowed() const = 0;
    virtual muse::async::Notification isPlayAllowedChanged() const = 0;

    // transport intents
    virtual void togglePlay(bool ignoreSelection) = 0;
    virtual void pause() = 0;
    virtual void stop() = 0;
    virtual void seekTo(muse::secs_t secs, bool triggerPlay) = 0;
    virtual void rewindToStart() = 0;
    virtual void rewindToEnd() = 0;
    virtual void changePlaybackRegion(muse::secs_t start, muse::secs_t end) = 0;
    virtual void stopSeekAndUpdatePlaybackRegion() = 0;

    // session cursor (the "anchor" a stop returns to)
    virtual muse::secs_t lastPlaybackSeekTime() const = 0;
    virtual void setLastPlaybackSeekTime(muse::secs_t secs) = 0;
    virtual muse::async::Notification lastPlaybackSeekTimeChanged() const = 0;

    virtual muse::secs_t totalPlayTime() const = 0;

    // loop-region intents (the raw loop region lives on IPlayer)
    virtual void toggleLoopPlayback() = 0;
    virtual void setLoopRegionToSelection() = 0;
    virtual void setSelectionToLoop() = 0;
    virtual void setLoopRegionInOut() = 0;
    virtual void setSelectionFollowsLoopRegion() = 0;

    // audio device/configuration changes (need both player and recorder state to
    // decide whether/where to resume — applied via a stream restart)
    virtual void setAudioApi(const std::string& api) = 0;
    virtual void setAudioOutputDevice(const std::string& device) = 0;
    virtual void setAudioInputDevice(const std::string& device) = 0;
    virtual void setInputChannels(int channels) = 0;
    virtual void rescanAudioDevices() = 0;
};

using ITransportPtr = std::shared_ptr<ITransport>;
}
