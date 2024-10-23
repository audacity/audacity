#include "playbackstate.h"

using namespace au::context;
using namespace au::playback;

void PlaybackState::setPlayer(playback::IPlayerPtr player)
{
    if (m_player) {
        m_player->playbackStatusChanged().resetOnReceive(this);
        m_player->playbackPositionChanged().resetOnReceive(this);
    }

    m_player = player;

    //! The redirect is needed so that consumers do not have to worry about resubscribing if the player changes
    m_player->playbackStatusChanged().onReceive(this, [this](PlaybackStatus st) {
        m_playbackStatusChanged.send(st);
    });

    m_player->playbackPositionChanged().onReceive(this, [this](muse::secs_t pos) {
        m_playbackPositionChanged.send(pos);
    });
}

PlaybackStatus PlaybackState::playbackStatus() const
{
    return m_player ? m_player->playbackStatus() : PlaybackStatus::Stopped;
}

muse::async::Channel<PlaybackStatus> PlaybackState::playbackStatusChanged() const
{
    return m_playbackStatusChanged;
}

muse::secs_t PlaybackState::playbackPosition() const
{
    return m_player ? m_player->playbackPosition() : muse::secs_t(0.0);
}

muse::async::Channel<muse::secs_t> PlaybackState::playbackPositionChanged() const
{
    return m_playbackPositionChanged;
}
