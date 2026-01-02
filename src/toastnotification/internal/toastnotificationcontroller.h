/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/actions/actiontypes.h"
#include "framework/global/async/asyncable.h"
#include "framework/actions/actionable.h"

#include "modularity/ioc.h"
#include "framework/actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"

namespace au::toastnotification {
class ToastNotificationController : public muse::actions::Actionable, public muse::async::Asyncable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    void init();

private:
    void addItem(const muse::actions::ActionQuery& q);
    void dismissItem(const muse::actions::ActionQuery& q);
};
}
