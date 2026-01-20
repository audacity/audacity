/*
 * Audacity: A Digital Audio Editor
 */
#include "dynamicsplaystatemodel.h"

#include "playback/iplayer.h"

namespace au::effects {
DynamicsPlayStateModel::DynamicsPlayStateModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void DynamicsPlayStateModel::init()
{
    playbackController()->isPlayingChanged().onNotify(this, [this]() {
        emit playStateChanged();
    });
}

Stopwatch::PlayState DynamicsPlayStateModel::playState() const
{
    switch (playbackController()->playbackStatus()) {
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
