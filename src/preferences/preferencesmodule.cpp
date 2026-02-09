#include "preferencesmodule.h"

#include "framework/global/modularity/ioc.h"

#include "framework/interactive/iinteractiveuriregister.h"

using namespace au::preferences;

std::string PreferencesModule::moduleName() const
{
    return "preferences";
}

void PreferencesModule::resolveImports()
{
    auto ir = ioc()->resolve<muse::interactive::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://preferences"), "Audacity.Preferences", "PreferencesDialog");
    }
}
