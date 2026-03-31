/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects {
class BuiltinEffectsLoader;
class BuiltinEffectsScanner;

class BuiltinEffectsModule : public muse::modularity::IModuleSetup
{
public:
    BuiltinEffectsModule();

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDelayedInit() override;

private:
    const std::shared_ptr<BuiltinEffectsLoader> m_effectLoader;
    const std::shared_ptr<BuiltinEffectsScanner> m_pluginsScanner;
};
}
