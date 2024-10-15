/*
* Audacity: A Digital Audio Editor
*/

#include "playbackstatemodel.h"

using namespace au::playback;

PlaybackStateModel::PlaybackStateModel(QObject* parent)
    : QObject(parent)
{
}

bool PlaybackStateModel::isPlaying()
{
    return controller()->isPlaying();
}

bool PlaybackStateModel::isPaused()
{
    return controller()->isPaused();
}
