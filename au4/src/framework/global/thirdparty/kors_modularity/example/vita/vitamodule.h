#ifndef EXAMPLE_VITAMODULE_H
#define EXAMPLE_VITAMODULE_H

#include "modularity/imodulesetup.h"

namespace app::vita {
class VitaModule : public modularity::IModuleSetup
{
public:
    VitaModule() = default;

    std::string moduleName() const override;

    void registerExports() override;
};
}

#endif // EXAMPLE_VITAMODULE_H
