/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QTimer>

#include <chrono>
#include <functional>
#include <optional>

namespace au::effects {
class Stopwatch : public QObject
{
    Q_OBJECT

public:
    Q_ENUMS(PlayState)
    enum PlayState {
        Stopped, Playing, Paused
    };

    Q_PROPERTY(PlayState playState READ playState WRITE setPlaystate NOTIFY
               playStateChanged)
    Q_PROPERTY(double elapsedTime READ elapsedTime NOTIFY elapsedTimeChanged)

    Stopwatch(QObject* parent = nullptr);

    void setPlaystate(PlayState state);
    PlayState playState() const;

    double elapsedTime() const { return m_elapsedTime; }

signals:
    void playStateChanged();
    void elapsedTimeChanged();
    void tick();

private:
    void doPlay();
    void doPause();
    void onTick();

    const std::function<void(double elapsedSeconds)> m_cb;
    std::optional<std::chrono::steady_clock::time_point> m_startTime;
    std::optional<std::chrono::steady_clock::time_point> m_pauseTime;
    QTimer m_timer;
    PlayState m_playState = Stopped;
    double m_elapsedTime = 0;
};
} // namespace au::effects
