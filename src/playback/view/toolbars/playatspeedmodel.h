/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/actions/iactionsdispatcher.h"

#include "context/iglobalcontext.h"
#include "playback/iplaybackcontroller.h"

#include "au3-utility/Observer.h"

namespace au::playback {
class PlayAtSpeedModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(double speed READ speed WRITE setSpeed NOTIFY speedChanged FINAL)
    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged FINAL)
    Q_PROPERTY(bool isEnabled READ isEnabled NOTIFY isEnabledChanged FINAL)

    muse::ContextInject<context::IGlobalContext> globalContext { this };
    muse::ContextInject<IPlaybackController> playbackController { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };

public:
    static constexpr double MIN_SPEED = 0.01;
    static constexpr double MAX_SPEED = 3.0;
    static constexpr double DEFAULT_SPEED = 1.0;
    static constexpr double SPEED_STEP = 0.1;

    explicit PlayAtSpeedModel(QObject* parent = nullptr);

    double speed() const;
    void setSpeed(double speed);

    bool isPlaying() const;
    bool isEnabled() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void play();
    Q_INVOKABLE void increaseSpeed();
    Q_INVOKABLE void decreaseSpeed();
    Q_INVOKABLE void resetSpeed();

signals:
    void speedChanged();
    void isPlayingChanged();
    void isEnabledChanged();

private:
    void onProjectChanged();
    void updateSpeedFromProject();
    void adjustSpeed(double delta);

    double m_speed = DEFAULT_SPEED;
    Observer::Subscription m_speedSubscription;
};
}
