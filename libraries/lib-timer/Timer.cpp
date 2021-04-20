#include "Timer.h"

#include <memory>

namespace audacity
{

namespace
{
class wxTimerHelper : public wxTimer
{
public:
    explicit wxTimerHelper (std::function<void ()> callback)
        : mCallback (std::move (callback))
    {}


    void Notify () override
    {
        mCallback ();
    }

private:
    std::function<void ()> mCallback;
};
}

void Timer::start ()
{
    if (mIsActive)
        stop ();

    if (!mTimer)
        mTimer = std::make_unique<wxTimerHelper> ([this]() { notify (); });

    using namespace std::chrono;

    mTimer->StartOnce (duration_cast<milliseconds>(mDuration).count ());

    mIsActive = true;
}

void Timer::start (Duration duration)
{
    mDuration = duration;

    start ();
}

void Timer::start (Callback callback)
{
    mCallback = std::move (callback);

    start ();
}

void Timer::start (Duration duration, Callback callback)
{
    mDuration = duration;
    mCallback = std::move (callback);

    start ();
}

void Timer::stop ()
{
    if (!mIsActive)
        return;

    mTimer->Stop ();
}

void Timer::setCallback (Callback callback)
{
    mCallback = std::move (callback);
}

bool Timer::isActive () const noexcept
{
    return mIsActive;
}

void Timer::setDuration (Duration duration) noexcept
{
    mDuration = duration;
}

Timer::Duration Timer::getDuration () const noexcept
{
    return mDuration;
}

void Timer::setIsSingleShot (bool singleShot) noexcept
{
    mSingleShot = singleShot;
}

bool Timer::getIsSingleShot () const noexcept
{
    return mSingleShot;
}

void Timer::Singleshot (Duration duration, Callback callback)
{
    thread_local static std::vector<std::unique_ptr<Timer>> Timers;

    Timer* timer = nullptr;

    if (!Timers.empty ())
    {
        auto it = std::find_if (Timers.begin (), Timers.end (), [](const std::unique_ptr<Timer>& timer) {
            return !timer->isActive ();
        });

        if (it != Timers.end ())
            timer = it->get ();
    }

    if (timer == nullptr)
    {
        Timers.emplace_back (new Timer);
        timer = Timers.back ().get ();
    }

    timer->start (duration, std::move (callback));

    Timers.erase (std::remove_if (Timers.begin (), Timers.end (), [](const std::unique_ptr<Timer>& timer) {
        return !timer->isActive ();
    }), Timers.end ());
}

void Timer::notify ()
{
    if (mCallback)
        mCallback ();

    mIsActive = false;

    if (!mSingleShot)
        start ();
}

}