/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "stopwatch.h"
#include "playback/iplaybackcontroller.h"

#include "global/async/asyncable.h"
#include "global/modularity/ioc.h"

#include <QObject>

namespace au::effects {
class DynamicsPlayStateModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(au::effects::Stopwatch::PlayState playState READ playState NOTIFY playStateChanged)

    muse::Inject<playback::IPlaybackController> playbackController;

public:
    Q_INVOKABLE void init();

    Stopwatch::PlayState playState() const;

signals:
    void playStateChanged();
};
}
