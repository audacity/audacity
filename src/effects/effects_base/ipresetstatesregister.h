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

    virtual std::optional<PresetState> presetState(const PresetKey& key) const = 0;
    virtual void setPresetState(const PresetKey& key, const PresetState& state) = 0;
    virtual void removePresetState(const PresetKey& key) = 0;
};
}
