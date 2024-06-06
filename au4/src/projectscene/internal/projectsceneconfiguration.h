#pragma once

#include "../iprojectsceneconfiguration.h"

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

namespace au::projectscene {
class ProjectSceneConfiguration : public IProjectSceneConfiguration
{
public:
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    ProjectSceneConfiguration() = default;

    void init();
};
}
