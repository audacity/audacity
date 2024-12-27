/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "global/async/notification.h"

#include "effectstypes.h"

struct EffectSettings;

namespace au::effects {
class IEffectInstancesRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectInstancesRegister)

public:
    virtual ~IEffectInstancesRegister() = default;

    virtual EffectInstanceId regInstance(const EffectId& effectId, const std::shared_ptr<EffectInstance>& i, EffectSettings* s) = 0;
    virtual void unregInstance(const std::shared_ptr<EffectInstance>& i) = 0;
    virtual void unregInstance(const EffectInstanceId& instanceId) = 0;

    virtual EffectInstanceId instanceIdOf(const std::shared_ptr<EffectInstance>& i) const = 0;
    virtual std::shared_ptr<EffectInstance> instanceById(const EffectInstanceId& instanceId) const = 0;
    virtual EffectId effectIdByInstanceId(const EffectInstanceId& instanceId) const = 0;

    virtual EffectSettings* settingsById(const EffectInstanceId& instanceId) const = 0;
    virtual void notifyAboutSettingsChanged(const EffectInstanceId& instanceId) = 0;
    virtual muse::async::Notification settingsChanged(const EffectInstanceId& instanceId) const = 0;
};
}
