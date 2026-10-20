/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodule.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractiveuriregister.h"

#include "internal/macrorunner.h"
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
        ir->registerQmlUri(muse::Uri("audacity://macromanager/deletemacro"), "Audacity.MacroManager", "DeleteMacroDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/editmacro"), "Audacity.MacroManager", "EditMacroDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/commandparams"), "Audacity.MacroManager", "CommandParamsDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/runonfiles"), "Audacity.MacroManager", "RunOnFilesDialog");
    }
}

void MacroManagerModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_repository->init();
}

IContextSetup* MacroManagerModule::newContext(const ContextPtr& ctx) const
{
    return new MacroManagerContext(ctx);
}

void MacroManagerContext::registerExports()
{
    m_runner = std::make_shared<MacroRunner>(iocContext());

    ioc()->registerExport<IMacroRunner>(mname, m_runner);
}

void MacroManagerContext::onAllInited(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_runOnFilesStarter = std::make_shared<RunOnFilesStarter>(iocContext());
    m_runOnFilesStarter->init();
}
