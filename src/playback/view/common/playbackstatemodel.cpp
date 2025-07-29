/*
* Audacity: A Digital Audio Editor
*/

#include "playbackstatemodel.h"

using namespace au::playback;

PlaybackStateModel::PlaybackStateModel(QObject* parent)
    : QObject(parent)
{
    controller()->isPlayingChanged().onNotify(this, [this]() {
        emit isPlayingChanged();
    });
}

bool PlaybackStateModel::isPlaying() const
{
    return controller()->isPlaying();
}

bool PlaybackStateModel::isPaused() const
{
    return controller()->isPaused();
}

bool PlaybackStateModel::isStopped() const
{
    return controller()->isStopped();
}
