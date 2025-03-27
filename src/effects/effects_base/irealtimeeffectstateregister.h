#pragma once

#include "effectstypes.h"
#include "modularity/ioc.h"

namespace au::effects {
class IRealtimeEffectStateRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRealtimeEffectStateRegister);

public:
    virtual ~IRealtimeEffectStateRegister() = default;

    virtual RealtimeEffectStateId registerState(const RealtimeEffectStatePtr& state) = 0;
    virtual void unregisterState(RealtimeEffectStateId id) = 0;
    virtual RealtimeEffectStatePtr stateById(RealtimeEffectStateId id) const = 0;
    virtual void clear() = 0;
};
}
