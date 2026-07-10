/*
* Audacity: A Digital Audio Editor
*/

#include "playbackstatemodel.h"

using namespace au::playback;

PlaybackStateModel::PlaybackStateModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void PlaybackStateModel::init()
{
    player()->isPlayingChanged().onNotify(this, [this]() {
        emit isPlayingChanged();
    });

    transport()->lastPlaybackSeekTimeChanged().onNotify(this, [this]() {
        emit lastPlaybackSeekTimeChanged();
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        emit isRecordingChanged();
    });
}

void PlaybackStateModel::setLastPlaybackSeekTime(double time)
{
    transport()->setLastPlaybackSeekTime(std::max(0.0, time));
}

bool PlaybackStateModel::isPlaying() const
{
    return player()->isPlaying();
}

bool PlaybackStateModel::isPaused() const
{
    return player()->isPaused();
}

bool PlaybackStateModel::isStopped() const
{
    return player()->isStopped();
}

bool PlaybackStateModel::isRecording() const
{
    return recordController()->isRecording();
}

double PlaybackStateModel::lastPlaybackSeekTime() const
{
    return transport()->lastPlaybackSeekTime();
}
