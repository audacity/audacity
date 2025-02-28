/*
* Audacity: A Digital Audio Editor
*/
#include "effectinstancesregister.h"
#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-effects/EffectPlugin.h"

using namespace au::effects;

EffectInstanceId EffectInstancesRegister::regInstance(const EffectId& effectId, const std::shared_ptr<EffectInstance>& i,
                                                      EffectSettingsAccessPtr s)
{
    EffectInstanceId id = i->id();
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

std::shared_ptr<au::effects::EffectInstance> EffectInstancesRegister::instanceById(const EffectInstanceId& instanceId) const
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

void EffectInstancesRegister::requestUpdateSettings(const EffectInstanceId& instanceId)
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        it->second.updateSettingsRequest.notify();
    }
}

muse::async::Notification EffectInstancesRegister::updateSettingsRequested(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second.updateSettingsRequest;
    }

    static muse::async::Notification null;
    return null;
}

const EffectSettings* EffectInstancesRegister::settingsById(const EffectInstanceId& instanceId) const
{
    if (const auto access = settingsAccessById(instanceId)) {
        return &access->Get();
    }
    return nullptr;
}

EffectSettingsAccess* EffectInstancesRegister::settingsAccessById(const EffectInstanceId& instanceId) const
{
    auto it = m_data.find(instanceId);
    if (it != m_data.end()) {
        return it->second.access.get();
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
