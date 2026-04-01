/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iplaybackcontroller.h"

namespace au::playback {
class PlaybackStateModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT
    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged FINAL)
    Q_PROPERTY(bool isPaused READ isPaused NOTIFY isPlayingChanged FINAL)
    Q_PROPERTY(bool isStopped READ isStopped NOTIFY isPlayingChanged FINAL)
    Q_PROPERTY(double lastPlaybackSeekTime READ lastPlaybackSeekTime NOTIFY lastPlaybackSeekTimeChanged FINAL)
    muse::ContextInject<au::playback::IPlaybackController> playbackController{ this };

public:
    explicit PlaybackStateModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
    Q_INVOKABLE void setLastPlaybackSeekTime(double time);

    bool isPlaying() const;
    bool isPaused() const;
    bool isStopped() const;
    double lastPlaybackSeekTime() const;

signals:
    void isPlayingChanged();
    void lastPlaybackSeekTimeChanged();
};
}
