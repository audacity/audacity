/*
* Audacity: A Digital Audio Editor
*/
#include "effectinstancesregister.h"

using namespace au::effects;

EffectInstanceId EffectInstancesRegister::regInstance(Effect* e, EffectSettings* s)
{
    EffectInstanceId id = reinterpret_cast<EffectInstanceId>(e);
    m_data.insert({ id, { e, s } });
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
        if (p.second.effect == e) {
            return p.first;
        }
    }

    return EffectInstanceId();
}

Effect* EffectInstancesRegister::instanceById(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second.effect;
    }

    return nullptr;
}

EffectSettings* EffectInstancesRegister::settingsById(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second.settings;
    }

    return nullptr;
}
