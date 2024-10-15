/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "iplaybackcontroller.h"
#include "modularity/ioc.h"

namespace au::playback {
class PlaybackStateModel : public QObject
{
    Q_OBJECT

    muse::Inject<au::playback::IPlaybackController> controller;

public:
    explicit PlaybackStateModel(QObject* parent = nullptr);

    Q_INVOKABLE bool isPlaying();
    Q_INVOKABLE bool isPaused();
};
}
