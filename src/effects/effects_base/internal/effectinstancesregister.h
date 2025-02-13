/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "../ieffectinstancesregister.h"

namespace au::effects {
class EffectInstancesRegister : public IEffectInstancesRegister
{
public:
    EffectInstancesRegister() = default;

    EffectInstanceId regInstance(const EffectId& effectId, const std::shared_ptr<EffectInstance>& i, EffectSettingsAccessPtr) override;
    void unregInstance(const std::shared_ptr<EffectInstance>& i) override;
    void unregInstance(const EffectInstanceId& instanceId) override;

    EffectInstanceId instanceIdOf(const std::shared_ptr<EffectInstance>& i) const override;
    std::shared_ptr<EffectInstance> instanceById(const EffectInstanceId& instanceId) const override;
    EffectId effectIdByInstanceId(const EffectInstanceId& instanceId) const override;

    void requestUpdateSettings(const EffectInstanceId& instanceId) override;
    muse::async::Notification updateSettingsRequested(const EffectInstanceId& instanceId) const override;

    const EffectSettings* settingsById(const EffectInstanceId& instanceId) const override;
    EffectSettingsAccess* settingsAccessById(const EffectInstanceId& instanceId) const override;
    void notifyAboutSettingsChanged(const EffectInstanceId& instanceId) override;
    muse::async::Notification settingsChanged(const EffectInstanceId& instanceId) const override;

private:

    struct RegisteredEffectInstance {
        EffectId effectId;
        std::shared_ptr<EffectInstance> instance = nullptr;
        EffectSettingsAccessPtr access = nullptr;
        muse::async::Notification settingsChanged;
        muse::async::Notification updateSettingsRequest;
    };

    std::map<EffectInstanceId, RegisteredEffectInstance> m_data;
};
}
