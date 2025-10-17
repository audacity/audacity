/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <chrono>
#include <functional>

namespace au {
class ITimer
{
public:
    virtual ~ITimer() = default;

    virtual void setCallback(std::function<void()>) = 0;
    virtual void setInterval(const std::chrono::milliseconds&) = 0;
    virtual void setSingleShot(bool) = 0;
    virtual void start() = 0;
    virtual void stop() = 0;
};
}
