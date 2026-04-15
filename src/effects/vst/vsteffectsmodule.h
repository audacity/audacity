/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class VstEffectsRepository;
class MuseVstModulesRepository;
class Vst3PluginsMetaReader;
class VstEffectsModule : public muse::modularity::IModuleSetup
{
public:
    VstEffectsModule();

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:

    const std::shared_ptr<Vst3PluginsMetaReader> m_vstMetaReader;
    std::shared_ptr<MuseVstModulesRepository> m_museVstModulesRepository;
    std::shared_ptr<VstEffectsRepository> m_vstEffectsRepository;
};

class VstEffectsContext : public muse::modularity::IContextSetup
{
public:
    VstEffectsContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void resolveImports() override;
    void onDeinit() override;

private:
};
}
