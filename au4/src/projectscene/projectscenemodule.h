/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_PROJECTSCENEMODULE_H
#define AU_PROJECTSCENE_PROJECTSCENEMODULE_H

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::projectscene {
class ProjectSceneUiActions;
class ProjectSceneActionController;
class ProjectSceneModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerUiTypes() override;
    void registerResources() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:

    std::shared_ptr<ProjectSceneActionController> m_actionController;
    std::shared_ptr<ProjectSceneUiActions> m_projectSceneUiActions;
};
}

#endif // AU_PROJECTSCENE_PROJECTSCENEMODULE_H
