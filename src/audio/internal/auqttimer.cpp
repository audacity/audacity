/*
 * Audacity: A Digital Audio Editor
 */
#include "auqttimer.h"

namespace au {
AuQtTimer::AuQtTimer(std::optional<Qt::TimerType> timerType)
{
    if (timerType.has_value()) {
        m_timer.setTimerType(*timerType);
    }
}

void AuQtTimer::setCallback(std::function<void()> callback)
{
    m_timer.callOnTimeout(std::move(callback));
}

void AuQtTimer::setInterval(const std::chrono::milliseconds& interval)
{
    m_timer.setInterval(interval);
}

void AuQtTimer::setSingleShot(bool singleShot)
{
    m_timer.setSingleShot(singleShot);
}

void AuQtTimer::start()
{
    m_timer.start();
}

void AuQtTimer::stop()
{
    m_timer.stop();
}
}
