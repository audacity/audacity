/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/ret.h"
#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"

namespace au::record {
class TimedRecordingController : public muse::rcommand::Commandable, public muse::async::Asyncable, public muse::Contextable
{
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };

public:
    TimedRecordingController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

private:
    muse::Ret openSetupDialog();
};
}
