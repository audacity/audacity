/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class BuiltinEffectsRepository;
class BuiltinEffectsModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void registerResources() override;
    void onPreInit(const muse::IApplication::RunMode& mode) override;

private:

    std::shared_ptr<BuiltinEffectsRepository> m_builtinEffectsRepository;
};
}
