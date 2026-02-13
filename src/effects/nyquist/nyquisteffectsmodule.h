/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class NyquistEffectsRepository;
class NyquistPluginsMetaReader;
class NyquistEffectsModule : public muse::modularity::IModuleSetup
{
public:
    NyquistEffectsModule();

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& runMode) override;
    void onDeinit() override;

private:

    const std::shared_ptr<NyquistPluginsMetaReader> m_nyquistMetaReader;
    std::shared_ptr<NyquistEffectsRepository> m_nyquistEffectsRepository;
};
}
