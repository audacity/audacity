/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_PROJECTSCENEMODULE_H
#define AU_PROJECTSCENE_PROJECTSCENEMODULE_H

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::projectscene {
class ProjectSceneUiActions;
class ProjectSceneActionsController;
class ProjectSceneConfiguration;
class RealtimeEffectPanelTrackSelection;
class ProjectSceneModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerResources() override;
    void registerExports() override;
    void resolveImports() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<ProjectSceneUiActions> m_uiActions;
    std::shared_ptr<ProjectSceneActionsController> m_projectSceneActionsController;
    std::shared_ptr<ProjectSceneConfiguration> m_configuration;
    std::shared_ptr<RealtimeEffectPanelTrackSelection> m_realtimeEffectPanelTrackSelection;
};
}

#endif // AU_PROJECTSCENE_PROJECTSCENEMODULE_H
