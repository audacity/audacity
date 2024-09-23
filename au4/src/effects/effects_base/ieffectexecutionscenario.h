/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "global/types/ret.h"

#include "async/notification.h"
#include "modularity/imoduleinterface.h"

#include "effectstypes.h"

namespace au::effects {
class IEffectExecutionScenario : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectExecutionScenario)

public:
    virtual ~IEffectExecutionScenario() = default;

    virtual muse::Ret performEffect(const EffectId& effectId) = 0;
    virtual bool lastProcessorIsAvailable() const = 0;
    virtual muse::async::Notification lastProcessorIsNowAvailable() const = 0;
    virtual muse::Ret repeatLastProcessor() = 0;
};
}
