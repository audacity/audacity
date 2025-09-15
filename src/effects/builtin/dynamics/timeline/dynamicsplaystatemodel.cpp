/*
 * Audacity: A Digital Audio Editor
 */
#include "dynamicsplaystatemodel.h"

#include "playback/iplayer.h"

namespace au::effects {
void DynamicsPlayStateModel::init()
{
    playback()->player()->playbackStatusChanged().onReceive(this, [this](playback::PlaybackStatus) {
        emit playStateChanged();
    });
}

Stopwatch::PlayState DynamicsPlayStateModel::playState() const
{
    switch (playback()->player()->playbackStatus()) {
    case playback::PlaybackStatus::Running:
        return Stopwatch::PlayState::Playing;
    case playback::PlaybackStatus::Paused:
        return Stopwatch::PlayState::Paused;
    case playback::PlaybackStatus::Stopped:
    default:
        return Stopwatch::PlayState::Stopped;
    }
}
}
