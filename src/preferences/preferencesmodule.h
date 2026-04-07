#pragma once

#include "modularity/imodulesetup.h"

namespace au::preferences {
class PreferencesModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;

    void registerUiTypes() override;
    void resolveImports() override;
};
}
