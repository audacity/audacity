/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <unordered_map>

#include "effects/effects_base/ipresetstatesregister.h"

namespace au::effects {
class PresetStatesRegister : public IPresetStatesRegister
{
public:
    std::string makePresetStateKey(const EffectId& effectId, bool usedDestructively, const std::string& presetStateKey) const override;
    std::optional<PresetState> presetState(const std::string& key) const override;
    void setPresetState(const std::string& key, const PresetState& state) override;
    void removePresetState(const std::string& key) override;

private:
    std::unordered_map<std::string, PresetState> m_presetStates;
};
}
