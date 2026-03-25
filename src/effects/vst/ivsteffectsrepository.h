/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class IVstEffectsRepository : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IVstEffectsRepository)
public:
    virtual ~IVstEffectsRepository() = default;

    virtual bool ensurePluginIsLoaded(const EffectId&) const = 0;
};
}
