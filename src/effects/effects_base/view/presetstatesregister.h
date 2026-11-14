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
    std::optional<PresetState> presetState(const PresetKey& key) const override;
    void setPresetState(const PresetKey& key, const PresetState& state) override;
    void removePresetState(const PresetKey& key) override;

private:
    static std::string keyString(const PresetKey& key);

    std::unordered_map<std::string, PresetState> m_presetStates;
};
}
