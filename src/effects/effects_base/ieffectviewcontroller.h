/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/types/ret.h"
#include "framework/global/modularity/imoduleinterface.h"

#include "effectstypes.h"

struct EffectSettings;
namespace au::effects {
class IEffectViewController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectViewController)

public:
    virtual ~IEffectViewController() = default;

    virtual muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) = 0;
    virtual void showEffect(const RealtimeEffectStatePtr& state) const = 0;
    virtual void hideEffect(const RealtimeEffectStatePtr& state) const = 0;
};
}
