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

    EffectInstanceId regInstance(Effect* e, EffectSettings* s) override;
    void unregInstance(const Effect* e) override;
    void unregInstance(const EffectInstanceId& instanceId) override;

    EffectInstanceId instanceIdOf(const Effect* e) const override;
    Effect* instanceById(const EffectInstanceId& instanceId) const override;
    EffectSettings* settingsById(const EffectInstanceId& instanceId) const override;

private:

    struct RegisteredEffectInstance {
        Effect* effect = nullptr;
        EffectSettings* settings = nullptr;
    };

    std::map<EffectInstanceId, RegisteredEffectInstance> m_data;
};
}
