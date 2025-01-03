/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class VstEffectsRepository;
class VstEffectsModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;

private:
    std::shared_ptr<VstEffectsRepository> m_vstEffectsRepository;
};
}  // namespace au::effects
