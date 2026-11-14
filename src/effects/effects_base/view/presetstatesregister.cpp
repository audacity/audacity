/*
* Audacity: A Digital Audio Editor
*/
#include "presetstatesregister.h"

using namespace au::effects;

std::string PresetStatesRegister::keyString(const PresetKey& key)
{
    if (key.effectId.empty()) {
        return {};
    }

    if (key.realtimeEffectState.empty()) {
        return "destructive:" + key.effectId.toStdString();
    }

    return "realtime:" + key.effectId.toStdString() + ":" + key.realtimeEffectState;
}

std::optional<IPresetStatesRegister::PresetState> PresetStatesRegister::presetState(const PresetKey& key) const
{
    const auto it = m_presetStates.find(keyString(key));
    if (it == m_presetStates.cend()) {
        return std::nullopt;
    }

    return it->second;
}

void PresetStatesRegister::setPresetState(const PresetKey& key, const PresetState& state)
{
    m_presetStates.insert_or_assign(keyString(key), state);
}

void PresetStatesRegister::removePresetState(const PresetKey& key)
{
    m_presetStates.erase(keyString(key));
}
