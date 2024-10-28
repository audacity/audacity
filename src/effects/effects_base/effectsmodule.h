/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects {
class IEffectsProvider;
class EffectsConfiguration;
class EffectsActionsController;
class EffectsUiActions;
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

private:
    std::shared_ptr<IEffectsProvider> m_provider;
    std::shared_ptr<EffectsConfiguration> m_configuration;
    std::shared_ptr<EffectsActionsController> m_actionsController;
    std::shared_ptr<EffectsUiActions> m_uiActions;
};
}
