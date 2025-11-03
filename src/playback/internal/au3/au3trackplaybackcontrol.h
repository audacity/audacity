/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"

#include "context/iglobalcontext.h"

#include "trackedit/iprojecthistory.h"
#include "au3wrap/au3types.h"

#include "iplaybackconfiguration.h"

#include "itrackplaybackcontrol.h"

namespace au::playback {
using au::audio::volume_dbfs_t;
using au::audio::pan_t;

class Au3TrackPlaybackControl : public ITrackPlaybackControl
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;
    muse::Inject<au::playback::IPlaybackConfiguration> playbackConfiguration;

public:
    Au3TrackPlaybackControl() = default;
    volume_dbfs_t volume(long trackId) const override;
    void setVolume(long trackId, volume_dbfs_t vol, bool completed) override;

    pan_t pan(long trackId) const override;
    void setPan(long trackId, au::audio::pan_t pan, bool completed) override;

    bool solo(long trackId) const override;
    void setSolo(long trackId, bool solo) override;

    bool muted(long trackId) const override;
    void setMuted(long trackId, bool mute) override;

    muse::async::Channel<long> muteOrSoloChanged() const override;

private:
    au3::Au3Project& projectRef() const;

    muse::async::Channel<long> m_muteOrSoloChanged;
};
}
