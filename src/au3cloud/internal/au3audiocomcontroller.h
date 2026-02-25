/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iinteractive.h"

#include "context/iglobalcontext.h"
#include "au3cloud/iau3audiocomservice.h"
#include "toast/itoastservice.h"

namespace au::au3cloud {
class Au3AudioComController : public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Injectable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::Inject<muse::IInteractive> interactive { this };
    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<IAu3AudioComService> audioComService { this };
    muse::Inject<toast::IToastService> toastService { this };

public:
    Au3AudioComController(muse::modularity::ContextPtr ctx = nullptr)
        : muse::Injectable(ctx) {}

    void init();

private:
    void shareAudio();
};
}
