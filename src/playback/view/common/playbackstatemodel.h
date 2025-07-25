/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "iplaybackcontroller.h"
#include "modularity/ioc.h"
#include "async/asyncable.h"

namespace au::playback {
class PlaybackStateModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged)
    muse::Inject<au::playback::IPlaybackController> controller;

public:
    explicit PlaybackStateModel(QObject* parent = nullptr);

    Q_INVOKABLE bool isPlaying() const;
    Q_INVOKABLE bool isPaused() const;

signals:
    void isPlayingChanged();
};
}
