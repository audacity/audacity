/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>

#include "modularity/imoduleinterface.h"

#include "effectstypes.h"

namespace au::effects {
class IPresetStatesRegister : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPresetStatesRegister)

public:
    struct PresetState {
        std::string presetId;
        bool unsaved = false;
    };

    virtual ~IPresetStatesRegister() = default;

    virtual std::string makePresetStateKey(const EffectId& effectId, bool usedDestructively, const std::string& presetStateKey) const = 0;
    virtual std::optional<PresetState> presetState(const std::string& key) const = 0;
    virtual void setPresetState(const std::string& key, const PresetState& state) = 0;
    virtual void removePresetState(const std::string& key) = 0;
};
}
