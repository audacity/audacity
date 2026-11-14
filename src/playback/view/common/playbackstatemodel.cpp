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
    playbackController()->isPlayingChanged().onNotify(this, [this]() {
        emit isPlayingChanged();
    });

    playbackController()->lastPlaybackSeekTimeChanged().onNotify(this, [this]() {
        emit lastPlaybackSeekTimeChanged();
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        emit isRecordingChanged();
    });
}

void PlaybackStateModel::setLastPlaybackSeekTime(double time)
{
    playbackController()->setLastPlaybackSeekTime(std::max(0.0, time));
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

bool PlaybackStateModel::isRecording() const
{
    return recordController()->isRecording();
}

double PlaybackStateModel::lastPlaybackSeekTime() const
{
    return playbackController()->lastPlaybackSeekTime();
}
