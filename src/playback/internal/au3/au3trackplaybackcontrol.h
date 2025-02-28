/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "itrackplaybackcontrol.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"

#include "au3wrap/au3types.h"

namespace au::playback {
using au::audio::volume_dbfs_t;
using au::audio::balance_t;

class Au3TrackPlaybackControl : public ITrackPlaybackControl
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;

public:
    Au3TrackPlaybackControl() = default;
    volume_dbfs_t volume(long trackId) const override;
    void setVolume(long trackId, volume_dbfs_t vol, bool completed) override;

    balance_t balance(long trackId) const override;
    void setBalance(long trackId, au::audio::balance_t balance, bool completed) override;

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
