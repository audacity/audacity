/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effectstypes.h"
#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

namespace au::effects {
class IEffectsConfiguration : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsConfiguration)
public:

    virtual ~IEffectsConfiguration() = default;

    virtual bool applyEffectToAllAudio() const = 0;
    virtual void setApplyEffectToAllAudio(bool value) = 0;
    virtual muse::async::Notification applyEffectToAllAudioChanged() const = 0;

    virtual EffectMenuOrganization effectMenuOrganization() const = 0;
    virtual void setEffectMenuOrganization(EffectMenuOrganization) = 0;
    virtual muse::async::Notification effectMenuOrganizationChanged() const = 0;

    virtual double previewMaxDuration() const = 0;
    virtual void setPreviewMaxDuration(double value) = 0;

    virtual EffectUIMode effectUIMode(const EffectId& effectId) const = 0;
    virtual void setEffectUIMode(const EffectId& effectId, EffectUIMode mode) = 0;
    virtual muse::async::Notification effectUIModeChanged() const = 0;
};
}
