/*
* Audacity: A Digital Audio Editor
*/
#include "effectinstancesregister.h"

using namespace au::effects;

EffectInstanceId EffectInstancesRegister::regInstance(const EffectId& effectId, Effect* e, EffectSettings* s)
{
    EffectInstanceId id = reinterpret_cast<EffectInstanceId>(e);
    m_data.insert({id, {effectId, e, s}});
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

EffectId EffectInstancesRegister::effectIdByInstanceId(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second.effectId;
    }

    return EffectId();
}

EffectSettings* EffectInstancesRegister::settingsById(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second.settings;
    }

    return nullptr;
}

void EffectInstancesRegister::notifyAboutSettingsChanged(const EffectInstanceId& instanceId)
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        it->second.settingsChanged.notify();
    }
}

muse::async::Notification EffectInstancesRegister::settingsChanged(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second.settingsChanged;
    }

    static muse::async::Notification null;
    return null;
}
