/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects {
class EffectsProvider;
class EffectsMenuProvider;
class EffectsConfiguration;
class EffectsActionsController;
class EffectsUiActions;
class RealtimeEffectService;

class EffectsModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onPreInit(const muse::IApplication::RunMode& mode) override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onAllInited(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<EffectsConfiguration> m_configuration;
    std::shared_ptr<EffectsProvider> m_effectsProvider;
};

class EffectsContext : public muse::modularity::IContextSetup
{
public:
    EffectsContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onAllInited(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<EffectsMenuProvider> m_effectsMenuProvider;
    std::shared_ptr<EffectsActionsController> m_actionsController;
    std::shared_ptr<RealtimeEffectService> m_realtimeEffectService;
};
}
