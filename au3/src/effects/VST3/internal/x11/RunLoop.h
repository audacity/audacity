/**********************************************************************

  Audacity: A Digital Audio Editor

  @file RunLoop.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <vector>
#include <memory>
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdk.h>

#include <pluginterfaces/gui/iplugview.h>

namespace internal {
namespace x11 {
class RunLoop : public Steinberg::Linux::IRunLoop
{
    struct EventHandler;
    struct TimerHandler;

    std::vector<std::unique_ptr<EventHandler> > mEventHandlers;
    std::vector<std::unique_ptr<TimerHandler> > mTimerHandlers;

    Display* mDisplay { nullptr };

    static gboolean OnEvent(GIOChannel* source, GIOCondition condition, gpointer data);

    static gboolean OnTimer(gpointer data);

public:
    RunLoop(::Display* display);
    virtual ~RunLoop();

    Steinberg::tresult PLUGIN_API registerEventHandler(
        Steinberg::Linux::IEventHandler* handler, Steinberg::Linux::FileDescriptor fd) override;

    Steinberg::tresult PLUGIN_API unregisterEventHandler(
        Steinberg::Linux::IEventHandler* handler) override;

    Steinberg::tresult PLUGIN_API registerTimer(Steinberg::Linux::ITimerHandler* handler,
                                                Steinberg::Linux::TimerInterval milliseconds) override;

    Steinberg::tresult PLUGIN_API unregisterTimer(Steinberg::Linux::ITimerHandler* handler) override;

    DECLARE_FUNKNOWN_METHODS
};
}
}
