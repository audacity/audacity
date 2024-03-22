#ifndef EXAMPLE_ALPHAMODULE_H
#define EXAMPLE_ALPHAMODULE_H

#include "modularity/imodulesetup.h"

namespace app::alpha {
class AlphaModule : public app::modularity::IModuleSetup
{
public:
    AlphaModule() = default;

    std::string moduleName() const override;

    void registerExports() override;
};
}

#endif // EXAMPLE_ALPHAMODULE_H
