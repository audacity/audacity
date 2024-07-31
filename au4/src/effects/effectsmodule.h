/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects {
class IEffectsProvider;
class IEffectsConfiguration;
class EffectsActionsController;
class EffectsModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDelayedInit() override;
    void onDeinit() override;

private:
    std::shared_ptr<IEffectsProvider> m_provider;
    std::shared_ptr<IEffectsConfiguration> m_configuration;
    std::shared_ptr<EffectsActionsController> m_actionsController;
};
}
