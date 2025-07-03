/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects {
class AudioUnitEffectsModule : public muse::modularity::IModuleSetup
{
public:
    AudioUnitEffectsModule() = default;

    std::string moduleName() const override;
    void registerResources() override;
};
}
