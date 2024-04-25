/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_PROJECTSCENEMODULE_H
#define AU_PROJECTSCENE_PROJECTSCENEMODULE_H

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::projectscene {
class ProjectSceneModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerUiTypes() override;
    void registerResources() override;

};
}

#endif // AU_PROJECTSCENE_PROJECTSCENEMODULE_H
