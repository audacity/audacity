/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodule.h"

#include "modularity/ioc.h"

#include "interactive/iinteractiveuriregister.h"

using namespace au::macromanager;

static const std::string mname("macromanager");

std::string MacroManagerModule::moduleName() const
{
    return mname;
}

void MacroManagerModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://macromanager/newmacro"), "Audacity.MacroManager", "NewMacroDialog");
    }
}
