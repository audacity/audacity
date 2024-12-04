/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "global/types/ret.h"
#include "global/async/channel.h"
#include "effectstypes.h"

namespace au::effects {
class IEffectsPresetsController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsPresetsController)

public:

    virtual ~IEffectsPresetsController() = default;

    virtual PresetIdList factoryPresets(const EffectId& effectId) const = 0;
    virtual PresetIdList userPresets(const EffectId& effectId) const = 0;
    virtual muse::async::Channel<EffectId> userPresetsChanged() const = 0;

    virtual muse::Ret applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) = 0;
    virtual muse::Ret saveCurrentAsPreset(const EffectInstanceId& effectId) = 0;
    virtual muse::Ret deletePreset(const EffectId& effectId, const PresetId& presetId) = 0;
    virtual muse::Ret importPreset(const EffectInstanceId& effectInstanceId) = 0;
    virtual muse::Ret exportPreset(const EffectInstanceId& effectInstanceId) = 0;
};
}
