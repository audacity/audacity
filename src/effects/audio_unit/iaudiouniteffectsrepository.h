/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class IAudioUnitEffectsRepository : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAudioUnitEffectsRepository)
public:
    virtual ~IAudioUnitEffectsRepository() = default;

    virtual bool ensurePluginIsLoaded(const EffectId& effectId) const = 0;

    virtual EffectMetaList effectMetaList() const = 0;
};
}
