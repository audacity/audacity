/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/types/ret.h"

#include "modularity/imoduleinterface.h"

#include "effectstypes.h"

namespace au::effects {
class IEffectExecutionScenarion : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectExecutionScenarion)

public:
    virtual ~IEffectExecutionScenarion() = default;

    virtual muse::Ret performEffect(const EffectId& effectId) = 0;
};
}
