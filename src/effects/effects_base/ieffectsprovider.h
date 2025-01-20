/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "global/types/ret.h"
#include "global/async/notification.h"

#include "modularity/imoduleinterface.h"

#include "au3wrap/au3types.h"

#include "effectstypes.h"

struct EffectSettings;
namespace au::effects {
class IEffectsProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsProvider)

public:
    virtual ~IEffectsProvider() = default;

    virtual EffectMetaList effectMetaList() const = 0;
    virtual muse::async::Notification effectMetaListChanged() const = 0;

    virtual EffectCategoryList effectsCategoryList() const = 0;

    virtual EffectMeta meta(const EffectId& effectId) const = 0;
    virtual std::string effectName(const std::string& effectId) const = 0;
    virtual std::string effectName(const effects::RealtimeEffectState& state) const = 0;
    virtual std::string effectSymbol(const std::string& effectId) const = 0;
    virtual Effect* effect(const EffectId& effectId) const = 0;

    virtual bool supportsMultipleClipSelection(const EffectId& effectId) const = 0;

    virtual muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) = 0;

    virtual void showEffect(const RealtimeEffectStatePtr& state) const = 0;
    virtual void hideEffect(const RealtimeEffectStatePtr& state) const = 0;
    virtual void toggleShowEffect(const RealtimeEffectStatePtr& state) const = 0;

    virtual muse::Ret performEffect(au3::Au3Project& project, Effect* effect, std::shared_ptr<EffectInstance> effectInstance,
                                    EffectSettings& settings) = 0;

    virtual muse::Ret previewEffect(au3::Au3Project& project, Effect* effect, EffectSettings& settings) = 0;
};
}
