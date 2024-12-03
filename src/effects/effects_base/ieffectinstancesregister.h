/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "global/async/notification.h"

#include "effectstypes.h"

class Effect;
struct EffectSettings;

namespace au::effects {
class IEffectInstancesRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectInstancesRegister)

public:
    virtual ~IEffectInstancesRegister() = default;

    virtual EffectInstanceId regInstance(const EffectId& effectId, Effect* e, EffectSettings* s) = 0;
    virtual void unregInstance(const Effect* e) = 0;
    virtual void unregInstance(const EffectInstanceId& instanceId) = 0;

    virtual EffectInstanceId instanceIdOf(const Effect* e) const = 0;
    virtual Effect* instanceById(const EffectInstanceId& instanceId) const = 0;
    virtual EffectId effectIdByInstanceId(const EffectInstanceId& instanceId) const = 0;

    virtual EffectSettings* settingsById(const EffectInstanceId& instanceId) const = 0;
    virtual void notifyAboutSettingsChanged(const EffectInstanceId& instanceId) = 0;
    virtual muse::async::Notification settingsChanged(const EffectInstanceId& instanceId) const = 0;
};
}
