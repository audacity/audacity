/*
* Audacity: A Digital Audio Editor
*/
#include "macroscommandscontroller.h"

#include "../macromanagercommands.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

void MacrosCommandsController::init()
{
    repository()->macrosChanged().onNotify(this, [this]() {
        registerHandlers();
    });

    registerHandlers();
}

void MacrosCommandsController::registerHandlers()
{
    commandDispatcher()->unreg(this);

    for (const Macro& macro : repository()->macros()) {
        commandDispatcher()->onRequest(this, makeMacroRunCommand(macro.name),
                                       ICommandDispatcher::CallBackParamsPromiseRet([this, name = macro.name](const Params&) {
            return runMacro(name);
        }));
    }
}

async::Promise<Ret> MacrosCommandsController::runMacro(const std::string& name)
{
    const std::optional<Macro> macro = repository()->macro(name);
    if (!macro) {
        return async::make_promise<Ret>([](auto resolve) {
            return resolve(make_ret(Ret::Code::BadArgs, "macro not found"));
        });
    }

    return runner()->run(*macro);
}
