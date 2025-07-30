/*
* Audacity: A Digital Audio Editor
*/

#include "playbackmodel.h"

#include "playback/iplayer.h"

using namespace au::playback;

PlaybackModel::PlaybackModel(QObject* parent)
    : QObject(parent)
{
    player()->playbackStatusChanged().onReceive(this, [this](PlaybackStatus) {
        emit isPlayingChanged();
    });
}

bool PlaybackModel::isPlaying() const
{
    return player()->playbackStatus() == PlaybackStatus::Running;
}

bool PlaybackModel::isPaused() const
{
    return player()->playbackStatus() == PlaybackStatus::Paused;
}

bool PlaybackModel::isStopped() const
{
    return player()->playbackStatus() == PlaybackStatus::Stopped;
}

IPlayerPtr PlaybackModel::player() const
{
    return playback()->player();
}
