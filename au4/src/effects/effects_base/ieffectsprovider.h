/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/types/ret.h"
#include "global/async/notification.h"

#include "modularity/imoduleinterface.h"

#include "effectstypes.h"

class AudacityProject;
class EffectSettings;
namespace au::effects {
class IEffectsProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsProvider)

public:
    virtual ~IEffectsProvider() = default;

    virtual void reloadEffects() = 0;

    virtual EffectMetaList effectMetaList() const = 0;
    virtual muse::async::Notification effectMetaListChanged() const = 0;

    virtual EffectCategoryList effectsCategoryList() const = 0;

    virtual EffectMeta meta(const muse::String& effectId) const = 0;

    // type - is Symbol of effect
    virtual muse::Ret showEffect(const muse::String& type, const EffectInstanceId& instanceId) = 0;

    virtual muse::Ret performEffect(AudacityProject& project, Effect* effect, std::shared_ptr<EffectInstance> effectInstance,
                                    EffectSettings& settings) = 0;
};
}
