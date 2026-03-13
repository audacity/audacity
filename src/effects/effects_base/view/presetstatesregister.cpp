/*
* Audacity: A Digital Audio Editor
*/
#include "presetstatesregister.h"

using namespace au::effects;

std::string PresetStatesRegister::makePresetStateKey(const EffectId& effectId, bool usedDestructively,
                                                     const std::string& presetStateKey) const
{
    if (effectId.empty()) {
        return {};
    }

    if (usedDestructively) {
        return "destructive:" + effectId.toStdString();
    }

    return !presetStateKey.empty() ? "realtime:" + presetStateKey : std::string();
}

std::optional<IPresetStatesRegister::PresetState> PresetStatesRegister::presetState(const std::string& key) const
{
    const auto it = m_presetStates.find(key);
    if (it == m_presetStates.cend()) {
        return std::nullopt;
    }

    return it->second;
}

void PresetStatesRegister::setPresetState(const std::string& key, const PresetState& state)
{
    m_presetStates.insert_or_assign(key, state);
}

void PresetStatesRegister::removePresetState(const std::string& key)
{
    m_presetStates.erase(key);
}
