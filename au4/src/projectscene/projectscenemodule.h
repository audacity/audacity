/*
* Audacity: A Digital Audio Editor
*/
#ifndef MU_PROJECTSCENE_PROJECTSCENEMODULE_H
#define MU_PROJECTSCENE_PROJECTSCENEMODULE_H

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::projectscene {
class ProjectSceneUiActions;
class ProjectSceneActionController;
class ProjectSceneModule : public mu::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerUiTypes() override;
    void registerResources() override;
    void onInit(const mu::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:

    std::shared_ptr<ProjectSceneActionController> m_actionController;
    std::shared_ptr<ProjectSceneUiActions> m_projectSceneUiActions;
};
}

#endif // MU_PROJECTSCENE_PROJECTSCENEMODULE_H
