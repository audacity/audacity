/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class NyquistEffectsRepository;
class NyquistPluginsMetaReader;
class NyquistPromptLoader;

class NyquistEffectsModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void registerResources() override;
    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& runMode) override;
    void onDeinit() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;
};

class NyquistEffectsContext : public muse::modularity::IContextSetup
{
public:
    NyquistEffectsContext(const muse::modularity::ContextPtr& ctx);

    void registerExports() override;
    void resolveImports() override;
    void onPreInit(const muse::IApplication::RunMode& runMode) override;
    void onInit(const muse::IApplication::RunMode& runMode) override;
    void onDeinit() override;

private:

    const std::unique_ptr<NyquistPromptLoader> m_nyquistPromptLoader;
    std::shared_ptr<NyquistEffectsRepository> m_nyquistEffectsRepository;
    std::shared_ptr<NyquistPluginsMetaReader> m_nyquistMetaReader;
};
}
