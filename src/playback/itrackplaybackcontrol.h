/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "auaudio/auaudiotypes.h"
#include "async/channel.h"
#include "modularity/imoduleinterface.h"

namespace au::playback {
class ITrackPlaybackControl : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackPlaybackControl)

public:
    virtual ~ITrackPlaybackControl() = default;

    virtual void setVolume(long trackId, au::auaudio::volume_dbfs_t volume, bool completed) = 0;
    virtual au::auaudio::volume_dbfs_t volume(long trackId) const = 0;

    virtual void setPan(long trackId, au::auaudio::pan_t pan, bool completed) = 0;
    virtual au::auaudio::pan_t pan(long trackId) const = 0;

    virtual void setSolo(long trackId, bool solo) = 0;
    virtual bool solo(long trackId) const = 0;

    virtual void setMuted(long trackId, bool mute) = 0;
    virtual bool muted(long trackId) const = 0;

    virtual muse::async::Channel<long> muteOrSoloChanged() const = 0;
};
}
