/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects {
class NyquistEffectsModule : public muse::modularity::IModuleSetup
{
public:
    NyquistEffectsModule() = default;

    std::string moduleName() const override;
    void registerResources() override;
};
}
