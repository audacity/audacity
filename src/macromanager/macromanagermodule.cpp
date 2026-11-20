/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodule.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractiveuriregister.h"
#include "framework/rcommand/icommandsregister.h"
#include "framework/rcommand/icommandsstate.h"

#include "internal/macrorunner.h"
#include "internal/macroscommandscontroller.h"
#include "internal/macroscommandsregister.h"
#include "internal/macroscommandsstate.h"
#include "internal/macrostephandlersregister.h"
#include "internal/macrosrepository.h"
#include "internal/runonfilesrequests.h"
#include "internal/runonfilesstarter.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::modularity;

static const std::string mname("macromanager");

std::string MacroManagerModule::moduleName() const
{
    return mname;
}

void MacroManagerModule::registerExports()
{
    m_repository = std::make_shared<MacrosRepository>();
    m_runOnFilesRequests = std::make_shared<RunOnFilesRequests>();

    globalIoc()->registerExport<IMacrosRepository>(mname, m_repository);
    globalIoc()->registerExport<IRunOnFilesRequests>(mname, m_runOnFilesRequests);
}

void MacroManagerModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://macromanager/macroname"), "Audacity.MacroManager", "MacroNameDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/editmacro"), "Audacity.MacroManager", "EditMacroDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/commandparams"), "Audacity.MacroManager", "CommandParamsDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/runonfiles"), "Audacity.MacroManager", "RunOnFilesDialog");
    }

    m_commandsRegister = std::make_shared<MacrosCommandsRegister>();
    auto cr = globalIoc()->resolve<muse::rcommand::ICommandsRegister>(mname);
    if (cr) {
        cr->reg(m_commandsRegister);
    }
}

void MacroManagerModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_repository->init();
    m_commandsRegister->init();
}

IContextSetup* MacroManagerModule::newContext(const ContextPtr& ctx) const
{
    return new MacroManagerContext(ctx);
}

void MacroManagerContext::registerExports()
{
    m_runner = std::make_shared<MacroRunner>(iocContext());
    m_stepHandlersRegister = std::make_shared<MacroStepHandlersRegister>();

    ioc()->registerExport<IMacroRunner>(mname, m_runner);
    ioc()->registerExport<IMacroStepHandlersRegister>(mname, m_stepHandlersRegister);
}

void MacroManagerContext::resolveImports()
{
    auto cs = ioc()->resolve<muse::rcommand::ICommandsState>(mname);
    if (cs) {
        cs->reg(std::make_shared<MacrosCommandsState>(iocContext()));
    }
}

void MacroManagerContext::onAllInited(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_runOnFilesStarter = std::make_shared<RunOnFilesStarter>(iocContext());
    m_runOnFilesStarter->init();

    m_commandsController = std::make_shared<MacrosCommandsController>(iocContext());
    m_commandsController->init();
}
