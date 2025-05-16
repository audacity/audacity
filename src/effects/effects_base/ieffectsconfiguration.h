/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effectstypes.h"
#include "async/notification.h"
#include "modularity/imoduleinterface.h"

namespace au::effects {
class IEffectsConfiguration : MODULE_EXPORT_INTERFACE
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
};
}
