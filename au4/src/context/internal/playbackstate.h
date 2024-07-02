/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/async/asyncable.h"

#include "../iplaybackstate.h"
#include "playback/iplayer.h"

namespace au::context {
class PlaybackState : public IPlaybackState, public muse::async::Asyncable
{
public:
    PlaybackState() = default;

    void setPlayer(playback::IPlayerPtr player);

    audio::PlaybackStatus playbackStatus() const override;
    muse::async::Channel<audio::PlaybackStatus> playbackStatusChanged() const override;

    audio::secs_t playbackPosition() const override;
    muse::async::Channel<audio::secs_t> playbackPositionChanged() const override;

private:
    playback::IPlayerPtr m_player;

    muse::async::Channel<audio::PlaybackStatus> m_playbackStatusChanged;
    muse::async::Channel<audio::secs_t> m_playbackPositionChanged;
};
}
