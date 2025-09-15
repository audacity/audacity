/*
 * Audacity: A Digital Audio Editor
 */
#include "stopwatch.h"

namespace au::effects {
Stopwatch::Stopwatch(QObject* parent)
    : QObject{parent}
{
    connect(&m_timer, &QTimer::timeout, this, [this] { onTick(); });
}

Stopwatch::PlayState Stopwatch::playState() const { return m_playState; }

void Stopwatch::setPlaystate(PlayState state)
{
    if (state == m_playState) {
        return;
    }

    switch (state) {
    case Stopped:
        doStop();
        break;
    case Playing:
        doPlay();
        break;
    case Paused:
        doPause();
        break;
    }

    m_playState = state;
    emit playStateChanged();
}

void Stopwatch::doPlay()
{
    const auto now = std::chrono::steady_clock::now();
    if (!m_startTime) {
        m_startTime = now;
    } else if (m_pauseTime) {
        const auto pausedDuration = now - *m_pauseTime;
        m_startTime = *m_startTime + pausedDuration;
        m_pauseTime.reset();
    }
    m_timer.start(std::chrono::milliseconds(10));
}

void Stopwatch::doPause()
{
    m_timer.stop();
    m_pauseTime = std::chrono::steady_clock::now();
}

void Stopwatch::doStop()
{
    m_timer.stop();
    m_startTime.reset();
    m_pauseTime.reset();
}

void Stopwatch::onTick()
{
    assert(m_startTime);
    if (!m_startTime) {
        return;
    }
    using namespace std::chrono;
    const auto now = steady_clock::now();
    const auto elapsed = duration_cast<microseconds>(now - *m_startTime).count();
    m_elapsedTime = elapsed / 1'000'000.0;
    emit tick();
}
} // namespace au::effects
