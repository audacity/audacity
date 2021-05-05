/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file Timer.h
 @brief Declare a toolkit and platform independent timer.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <functional>
#include <chrono>
#include <memory>

#include "TimerApi.h"

class wxTimer;
namespace audacity
{
class TIMER_API Timer final
{
public:
    using Callback = std::function<void ()>;
    using Duration = std::chrono::milliseconds;

    Timer () = default;
    ~Timer ();

    Timer (const Timer&) = delete;
    Timer (Timer&&) = delete;
    Timer& operator = (const Timer&) = delete;
    Timer& operator = (Timer&&) = delete;

    void start ();
    void start (Duration duration);

    void start (Callback callback);
    void start (Duration duration, Callback callback);

    void stop ();

    void setCallback (Callback callback);

    bool isActive () const noexcept;

    void setDuration (Duration duration) noexcept;
    Duration getDuration () const noexcept;

    void setIsSingleShot (bool singleShot) noexcept;
    bool getIsSingleShot () const noexcept;

    static void Singleshot (Duration duration, Callback callback);
private:
    void notify ();

    std::unique_ptr<wxTimer> mTimer;

    Callback mCallback;

    Duration mDuration {};

    bool mIsActive { false };
    bool mSingleShot { true };
};

}