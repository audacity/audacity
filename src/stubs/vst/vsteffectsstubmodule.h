/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects {
class VstEffectsModule : public muse::modularity::IModuleSetup
{
public:
    VstEffectsModule() = default;

    std::string moduleName() const override;
    void registerResources() override;
};
}
