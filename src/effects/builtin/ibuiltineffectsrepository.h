/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "modularity/imoduleinterface.h"

#include "effects/effects_base/effectstypes.h"

namespace au::effects {
class IBuiltinEffectsRepository : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IBuiltinEffectsRepository)
public:
    virtual ~IBuiltinEffectsRepository() = default;

    virtual muse::async::Notification effectMetaListUpdated() const = 0;
    virtual EffectMetaList effectMetaList() const = 0;
};
}
