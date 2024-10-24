/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "itrackplaybackcontrol.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"

namespace au::playback {
using au::audio::volume_dbfs_t;
using au::audio::balance_t;

class Au3TrackPlaybackControl : public ITrackPlaybackControl
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3TrackPlaybackControl() = default;
    volume_dbfs_t volume(long trackId) override;
    void setVolume(long trackId, volume_dbfs_t vol) override;

    balance_t balance(long trackId) override;
    void setBalance(long trackId, balance_t balance) override;

private:
    au3::Au3Project& projectRef() const;
};
}
