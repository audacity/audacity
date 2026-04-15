/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class AudioUnitEffectsRepository;
class AudioUnitPluginsMetaReader;

class AudioUnitEffectsModule : public muse::modularity::IModuleSetup
{
public:
    AudioUnitEffectsModule();

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    const std::shared_ptr<AudioUnitPluginsMetaReader> m_metaReader;
};

class AudioUnitEffectsContext : public muse::modularity::IContextSetup
{
public:
    AudioUnitEffectsContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void resolveImports() override;
    void onDeinit() override;
};
}
