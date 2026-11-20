/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/global/async/asyncable.h"
#include "framework/global/async/promise.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/ret.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"

#include "../imacrorunner.h"
#include "../imacrosrepository.h"

namespace au::macromanager {
class MacrosCommandsController : public muse::rcommand::Commandable, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<IMacrosRepository> repository;
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<IMacroRunner> runner{ this };

public:
    MacrosCommandsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

private:
    void registerHandlers();
    muse::async::Promise<muse::Ret> runMacro(const std::string& name);
};
}
