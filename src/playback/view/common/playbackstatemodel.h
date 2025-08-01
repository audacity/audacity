/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iplaybackcontroller.h"

namespace au::playback {
class PlaybackStateModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged FINAL)
    Q_PROPERTY(bool isPaused READ isPaused NOTIFY isPlayingChanged FINAL)
    Q_PROPERTY(bool isStopped READ isStopped NOTIFY isPlayingChanged FINAL)
    muse::Inject<au::playback::IPlaybackController> controller;

public:
    explicit PlaybackStateModel(QObject* parent = nullptr);

    bool isPlaying() const;
    bool isPaused() const;
    bool isStopped() const;

signals:
    void isPlayingChanged();
};
}
