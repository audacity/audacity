/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>
#include <string>

#include "framework/actions/actionable.h"
#include "framework/actions/actiontypes.h"
#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iplatforminteractive.h"
#include "au3cloud/iauthorization.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::au3cloud {
class CloudUrlHandler;

class Au3CloudActionsController : public muse::actions::Actionable, public muse::rcommand::Commandable, public muse::async::Asyncable,
    public muse::Contextable
{
    muse::GlobalInject<muse::IPlatformInteractive> platformInteractive;
    muse::GlobalInject<IAuthorization> authorization;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };
    muse::ContextInject<IAu3AudioComService> audioComService { this };

public:
    Au3CloudActionsController(muse::modularity::ContextPtr ctx = nullptr);
    ~Au3CloudActionsController();

    void init();

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    muse::Ret showTourPage();
    muse::Ret openCloudProjectPage(const muse::rcommand::Params& params);
    muse::Ret openCloudAudioPage(const muse::rcommand::Params& params);
    muse::Ret openCloudProfilePage();
    muse::Ret openUrl(const muse::rcommand::Params& params);

    std::unique_ptr<CloudUrlHandler> m_urlHandler;
    std::vector<std::string> m_pendingUrls;
};
}
