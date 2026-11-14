/**********************************************************************

  Audacity: A Digital Audio Editor

  @file RunLoop.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "RunLoop.h"

using namespace internal::x11;

struct RunLoop::EventHandler
{
    Steinberg::IPtr<Steinberg::Linux::IEventHandler> handler;
    GIOChannel* channel { nullptr };
    guint id{ 0 };

    ~EventHandler()
    {
        if (id != 0) {
            g_source_remove(id);
        }
        if (channel != nullptr) {
            g_io_channel_unref(channel);
        }
    }
};

struct RunLoop::TimerHandler
{
    Steinberg::IPtr<Steinberg::Linux::ITimerHandler> handler;
    guint id { 0 };

    ~TimerHandler()
    {
        if (id != 0) {
            g_source_remove(id);
        }
    }
};

IMPLEMENT_FUNKNOWN_METHODS(RunLoop, Steinberg::Linux::IRunLoop, Steinberg::Linux::IRunLoop::iid);

gboolean RunLoop::OnEvent(GIOChannel* source, GIOCondition condition, gpointer data)
{
    auto handler = static_cast<Steinberg::Linux::IEventHandler*>(data);
    handler->onFDIsSet(g_io_channel_unix_get_fd(source));
    return G_SOURCE_REMOVE;
}

gboolean RunLoop::OnTimer(gpointer data)
{
    auto handler = static_cast<Steinberg::Linux::ITimerHandler*>(data);
    handler->onTimer();
    return G_SOURCE_CONTINUE;
}

RunLoop::RunLoop(::Display* display)
    : mDisplay(display)
{
    FUNKNOWN_CTOR;
}

RunLoop::~RunLoop()
{
    FUNKNOWN_DTOR;
}

Steinberg::tresult RunLoop::registerEventHandler(
    Steinberg::Linux::IEventHandler* handler,
    Steinberg::Linux::FileDescriptor fd)
{
    using namespace Steinberg;

    if (handler == nullptr) {
        return kInvalidArgument;
    }

    if (XConnectionNumber(mDisplay) == fd) {
        return kResultOk;
    }

    auto eventHandler = std::make_unique<EventHandler>();
    eventHandler->handler = handler;

    if (auto channel = g_io_channel_unix_new(fd)) {
        eventHandler->channel = channel;
        eventHandler->id = g_io_add_watch(
            channel,
            (GIOCondition)(G_IO_IN | G_IO_OUT | G_IO_ERR | G_IO_HUP),
            &RunLoop::OnEvent,
            handler
            );
        mEventHandlers.emplace_back(std::move(eventHandler));
        return kResultTrue;
    }
    return kResultFalse;
}

Steinberg::tresult RunLoop::unregisterEventHandler(
    Steinberg::Linux::IEventHandler* handler)
{
    using namespace Steinberg;

    for (auto it = mEventHandlers.begin(), end = mEventHandlers.end(); it != end; ++it) {
        if ((*it)->handler.get() == handler) {
            mEventHandlers.erase(it);
            return kResultTrue;
        }
    }
    return kResultFalse;
}

Steinberg::tresult RunLoop::registerTimer(Steinberg::Linux::ITimerHandler* handler,
                                          Steinberg::Linux::TimerInterval milliseconds)
{
    using namespace Steinberg;

    if (!handler || milliseconds == 0) {
        return kInvalidArgument;
    }

    auto timerHandler = std::make_unique<TimerHandler>();
    timerHandler->handler = handler;
    timerHandler->id = g_timeout_add(milliseconds, &RunLoop::OnTimer, handler);

    mTimerHandlers.emplace_back(std::move(timerHandler));

    return Steinberg::kResultOk;
}

Steinberg::tresult RunLoop::unregisterTimer(Steinberg::Linux::ITimerHandler* handler)
{
    using namespace Steinberg;

    for (auto it = mTimerHandlers.begin(), end = mTimerHandlers.end(); it != end; ++it) {
        if ((*it)->handler.get() == handler) {
            mTimerHandlers.erase(it);
            return kResultTrue;
        }
    }
    return kResultFalse;
}
