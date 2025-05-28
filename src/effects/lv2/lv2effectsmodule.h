/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class Lv2PluginMetaReader;

class Lv2EffectsModule : public muse::modularity::IModuleSetup
{
public:
    Lv2EffectsModule();

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    const std::shared_ptr<Lv2PluginMetaReader> m_metaReader;
};
}
