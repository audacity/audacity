/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class IVstEffectsRepository : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IVstEffectsRepository)
public:
    virtual ~IVstEffectsRepository() = default;

    virtual EffectMetaList effectMetaList() const = 0;
};
}
