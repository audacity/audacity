/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/imoduleinterface.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class ILv2EffectsRepository : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ILv2EffectsRepository)
public:
    virtual ~ILv2EffectsRepository() = default;

    virtual EffectMetaList effectMetaList() const = 0;
    virtual bool ensurePluginIsLoaded(const EffectId&) const = 0;
};
}
