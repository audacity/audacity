/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "itimer.h"
#include <QTimer>

namespace au {
class AuQtTimer : public ITimer
{
public:
    AuQtTimer(std::optional<Qt::TimerType> = {});
    ~AuQtTimer() override = default;

    void setCallback(std::function<void()>) override;
    void setInterval(const std::chrono::milliseconds&) override;
    void setSingleShot(bool) override;
    void start() override;
    void stop() override;
private:
    QTimer m_timer;
};
}
