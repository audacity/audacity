/*
* Audacity: A Digital Audio Editor
*/
#include "effectinstancesregister.h"

using namespace au::effects;

EffectInstanceId EffectInstancesRegister::regInstance(const EffectId& effectId, const std::shared_ptr<EffectInstance>& i, EffectSettings* s)
{
    EffectInstanceId id = reinterpret_cast<EffectInstanceId>(i.get());
    m_data.insert({ id, { effectId, i, s, muse::async::Notification() } });
    return id;
}

void EffectInstancesRegister::unregInstance(const std::shared_ptr<EffectInstance>& i)
{
    EffectInstanceId id = instanceIdOf(i);
    unregInstance(id);
}

void EffectInstancesRegister::unregInstance(const EffectInstanceId& instanceId)
{
    m_data.erase(instanceId);
}

EffectInstanceId EffectInstancesRegister::instanceIdOf(const std::shared_ptr<EffectInstance>& i) const
{
    for (const auto& p : m_data) {
        if (p.second.instance == i) {
            return p.first;
        }
    }

    return EffectInstanceId();
}

std::shared_ptr<EffectInstance> EffectInstancesRegister::instanceById(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second.instance;
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
