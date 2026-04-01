/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/actions/actionable.h"
#include "framework/actions/actiontypes.h"

#include "framework/global/modularity/ioc.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iinteractive.h"
#include "framework/interactive/iplatforminteractive.h"
#include "au3cloud/iauthorization.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::au3cloud {
class Au3CloudActionsController : public muse::actions::Actionable, public muse::Contextable
{
    muse::GlobalInject<muse::IPlatformInteractive> platformInteractive;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<IAuthorization> authorization { this };
    muse::ContextInject<IAu3AudioComService> audioComService { this };

public:
    Au3CloudActionsController(muse::modularity::ContextPtr ctx = nullptr);

    void init();

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    void openSignInDialog(const muse::actions::ActionQuery& query);
};
}
