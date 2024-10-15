/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class IBuiltinEffectsRepository : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IBuiltinEffectsRepository)
public:
    virtual ~IBuiltinEffectsRepository() = default;

    virtual EffectMetaList effectMetaList() const = 0;
};
}
