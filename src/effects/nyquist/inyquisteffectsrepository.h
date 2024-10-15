/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class INyquistEffectsRepository : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(INyquistEffectsRepository)
public:
    virtual ~INyquistEffectsRepository() = default;

    virtual EffectMetaList effectMetaList() const = 0;
};
}
