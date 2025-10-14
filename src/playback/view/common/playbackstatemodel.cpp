/*
* Audacity: A Digital Audio Editor
*/

#include "playbackstatemodel.h"

using namespace au::playback;

PlaybackStateModel::PlaybackStateModel(QObject* parent)
    : QObject(parent)
{
    playbackController()->isPlayingChanged().onNotify(this, [this]() {
        emit isPlayingChanged();
    });
}

bool PlaybackStateModel::isPlaying() const
{
    return playbackController()->isPlaying();
}

bool PlaybackStateModel::isPaused() const
{
    return playbackController()->isPaused();
}

bool PlaybackStateModel::isStopped() const
{
    return playbackController()->isStopped();
}
