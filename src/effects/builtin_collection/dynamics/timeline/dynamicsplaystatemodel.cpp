/*
 * Audacity: A Digital Audio Editor
 */
#include "dynamicsplaystatemodel.h"

#include "playback/iplayer.h"

namespace au::effects {
DynamicsPlayStateModel::DynamicsPlayStateModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void DynamicsPlayStateModel::init()
{
    player()->isPlayingChanged().onNotify(this, [this]() {
        emit playStateChanged();
    });
}

Stopwatch::PlayState DynamicsPlayStateModel::playState() const
{
    switch (player()->playbackStatus()) {
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
