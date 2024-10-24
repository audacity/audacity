/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

namespace au::effects {
class IEffectsConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsConfiguration)
public:

    virtual ~IEffectsConfiguration() = default;
};
}
