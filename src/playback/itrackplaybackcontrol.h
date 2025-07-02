/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3audio/audiotypes.h"
#include "async/channel.h"
#include "modularity/imoduleinterface.h"

namespace au::playback {
class ITrackPlaybackControl : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackPlaybackControl)

public:
    virtual ~ITrackPlaybackControl() = default;

    virtual void setVolume(long trackId, au::audio::volume_dbfs_t volume, bool completed) = 0;
    virtual au::audio::volume_dbfs_t volume(long trackId) const = 0;

    virtual void setPan(long trackId, au::audio::pan_t pan, bool completed) = 0;
    virtual au::audio::pan_t pan(long trackId) const = 0;

    virtual void setSolo(long trackId, bool solo) = 0;
    virtual bool solo(long trackId) const = 0;

    virtual void setMuted(long trackId, bool mute) = 0;
    virtual bool muted(long trackId) const = 0;

    virtual muse::async::Channel<long> muteOrSoloChanged() const = 0;
};
}
