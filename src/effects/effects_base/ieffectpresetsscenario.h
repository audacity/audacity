/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "effectstypes.h"

namespace au::effects {
class IEffectPresetsScenario : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsPresetsScenario)

public:

    virtual ~IEffectPresetsScenario() = default;

    // methods with interaction (select file, show error and etc)
    virtual void applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) = 0;
    virtual void saveCurrentAsPreset(const EffectInstanceId& effectInstanceId) = 0;
    virtual void deletePreset(const EffectId& effectId, const PresetId& presetId) = 0;
    virtual void importPreset(const EffectInstanceId& effectInstanceId) = 0;
    virtual void exportPreset(const EffectInstanceId& effectInstanceId) = 0;
};
}
