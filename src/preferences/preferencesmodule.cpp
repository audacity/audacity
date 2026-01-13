#include "preferencesmodule.h"

#include "modularity/ioc.h"

#include "ui/iinteractiveuriregister.h"

using namespace au::preferences;

std::string PreferencesModule::moduleName() const
{
    return "preferences";
}

void PreferencesModule::resolveImports()
{
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://preferences"), "Audacity.Preferences", "PreferencesDialog");
    }
}
