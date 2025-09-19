/*
 * Audacity: A Digital Audio Editor
 */
#include "stopwatch.h"

#include "global/log.h"

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
    if (m_playState == Stopped || !m_startTime) {
        m_startTime = now;
        m_pauseTime.reset();
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

void Stopwatch::onTick()
{
    IF_ASSERT_FAILED(m_startTime) {
        return;
    }
    if (m_playState == Playing) {
        using namespace std::chrono;
        const auto now = steady_clock::now();
        const auto elapsed = duration_cast<microseconds>(now - *m_startTime).count();
        m_elapsedTime = elapsed / 1'000'000.0;
        emit elapsedTimeChanged();
    }
    emit tick();
}
} // namespace au::effects
