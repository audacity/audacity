/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects {
class VstEffectsStubModule : public muse::modularity::IModuleSetup
{
public:
    VstEffectsStubModule() = default;

    std::string moduleName() const override;
    void registerResources() override;
};
}
