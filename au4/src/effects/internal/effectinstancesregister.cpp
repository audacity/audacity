/*
* Audacity: A Digital Audio Editor
*/
#include "effectinstancesregister.h"

using namespace au::effects;

EffectInstanceId EffectInstancesRegister::regInstance(Effect* e)
{
    EffectInstanceId id = reinterpret_cast<EffectInstanceId>(e);
    m_data.insert({ id, e });
    return id;
}

void EffectInstancesRegister::unregInstance(const Effect* e)
{
    EffectInstanceId id = instanceIdOf(e);
    unregInstance(id);
}

void EffectInstancesRegister::unregInstance(const EffectInstanceId& instanceId)
{
    m_data.erase(instanceId);
}

EffectInstanceId EffectInstancesRegister::instanceIdOf(const Effect* e) const
{
    for (const auto& p : m_data) {
        if (p.second == e) {
            return p.first;
        }
    }

    return EffectInstanceId();
}

Effect* EffectInstancesRegister::instanceById(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second;
    }

    return nullptr;
}
