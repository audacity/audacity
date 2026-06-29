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
    playback()->player()->isPlayingChanged().onNotify(this, [this]() {
        emit isPlayingChanged();
    });

    playback()->player()->lastPlaybackSeekTimeChanged().onNotify(this, [this]() {
        emit lastPlaybackSeekTimeChanged();
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        emit isRecordingChanged();
    });
}

void PlaybackStateModel::setLastPlaybackSeekTime(double time)
{
    playback()->player()->setLastPlaybackSeekTime(std::max(0.0, time));
}

bool PlaybackStateModel::isPlaying() const
{
    return playback()->player()->isPlaying();
}

bool PlaybackStateModel::isPaused() const
{
    return playback()->player()->isPaused();
}

bool PlaybackStateModel::isStopped() const
{
    return playback()->player()->isStopped();
}

bool PlaybackStateModel::isRecording() const
{
    return recordController()->isRecording();
}

double PlaybackStateModel::lastPlaybackSeekTime() const
{
    return playback()->player()->lastPlaybackSeekTime();
}
