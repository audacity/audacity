/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::effects {
class AudacityPluginEffectLoader;

class AudacityPluginEffectsModule final : public muse::modularity::IModuleSetup
{
public:
    AudacityPluginEffectsModule();

    std::string moduleName() const override;
    void resolveImports() override;
    void onDeinit() override;
    muse::modularity::IContextSetup* newContext(
        const muse::modularity::ContextPtr& context) const override;

private:
    std::shared_ptr<AudacityPluginEffectLoader> m_effectLoader;
};

class AudacityPluginEffectsContext final : public muse::modularity::IContextSetup
{
public:
    explicit AudacityPluginEffectsContext(const muse::modularity::ContextPtr& context)
        : muse::modularity::IContextSetup(context) {}

    void resolveImports() override;
};
} // namespace au::effects
