/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "effectstypes.h"

namespace au::effects {
class IEffectsPresetsController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsPresetsController)

public:

    virtual ~IEffectsPresetsController() = default;

    virtual PresetIdList factoryPresets(const EffectId& effectId) const = 0;
    virtual PresetIdList userPresets(const EffectId& effectId) const = 0;

    virtual void applyPreset(const EffectId& effectId, const PresetId& presetId) = 0;
    virtual void saveCurrentAsPreset(const EffectId& effectId) = 0;
    virtual void deletePreset(const EffectId& effectId, const PresetId& presetId) = 0;
    virtual void importPreset(const EffectId& effectId) = 0;
    virtual void exportPreset(const EffectId& effectId) = 0;
};
}
