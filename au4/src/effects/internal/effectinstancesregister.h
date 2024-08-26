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

    EffectInstanceId regInstance(Effect* e) override;
    void unregInstance(const Effect* e) override;
    void unregInstance(const EffectInstanceId& instanceId) override;

    EffectInstanceId instanceIdOf(const Effect* e) const override;
    Effect* instanceById(const EffectInstanceId& instanceId) const override;

private:

    std::map<EffectInstanceId, Effect*> m_data;
};
}
