/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class NyquistEffectsRepository;
class NyquistEffectsModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;

private:

    std::shared_ptr<NyquistEffectsRepository> m_nyquistEffectsRepository;
};
}
