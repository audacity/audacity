/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/async/asyncable.h"

#include "context/iplaybackcontext.h"
#include "playback/iplayer.h"

namespace au::context {
class PlaybackContext : public IPlaybackContext, public muse::async::Asyncable
{
public:
    PlaybackContext() = default;

    void setPlayer(playback::IPlayerPtr player);

    playback::PlaybackStatus playbackStatus() const override;
    bool isPlaying() const override;
    muse::async::Channel<playback::PlaybackStatus> playbackStatusChanged() const override;

    muse::secs_t playbackPosition() const override;
    muse::async::Channel<muse::secs_t> playbackPositionChanged() const override;

private:
    playback::IPlayerPtr m_player;

    muse::async::Channel<playback::PlaybackStatus> m_playbackStatusChanged;
    muse::async::Channel<muse::secs_t> m_playbackPositionChanged;
};
}
