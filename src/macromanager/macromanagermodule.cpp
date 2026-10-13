/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodule.h"

#include "modularity/ioc.h"

#include "interactive/iinteractiveuriregister.h"

#include "internal/macrosrepository.h"

using namespace au::macromanager;
using namespace muse;

static const std::string mname("macromanager");

std::string MacroManagerModule::moduleName() const
{
    return mname;
}

void MacroManagerModule::registerExports()
{
    m_repository = std::make_shared<MacrosRepository>();

    globalIoc()->registerExport<IMacrosRepository>(mname, m_repository);
}

void MacroManagerModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://macromanager/macroname"), "Audacity.MacroManager", "MacroNameDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/deletemacro"), "Audacity.MacroManager", "DeleteMacroDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/editmacro"), "Audacity.MacroManager", "EditMacroDialog");
        ir->registerQmlUri(muse::Uri("audacity://macromanager/commandparams"), "Audacity.MacroManager", "CommandParamsDialog");
    }
}

void MacroManagerModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_repository->init();
}
