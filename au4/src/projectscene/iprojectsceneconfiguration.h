#pragma once

#include "modularity/imoduleinterface.h"
#include "types/wavestyle.h"

namespace au::projectscene {
class IProjectSceneConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectSceneConfiguration)
public:
    virtual ~IProjectSceneConfiguration() = default;

    virtual const WaveStyle& waveStyle() const = 0;
};
}
