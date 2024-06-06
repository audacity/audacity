#pragma once

#include "modularity/imoduleinterface.h"

namespace au::projectscene {
class IProjectSceneConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectSceneConfiguration)
public:
    virtual ~IProjectSceneConfiguration() = default;
};
}
