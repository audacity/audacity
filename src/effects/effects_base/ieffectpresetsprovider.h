/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"
#include "global/types/ret.h"
#include "global/io/path.h"
#include "global/async/channel.h"
#include "effectstypes.h"

namespace au::effects {
class IEffectPresetsProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IEffectsPresetsProvider)

public:

    virtual ~IEffectPresetsProvider() = default;

    virtual PresetIdList factoryPresets(const EffectId& effectId) const = 0;
    virtual PresetIdList userPresets(const EffectId& effectId) const = 0;
    virtual muse::async::Channel<EffectId> userPresetsChanged() const = 0;

    virtual muse::Ret applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) = 0;
    virtual muse::Ret saveCurrentAsPreset(const EffectInstanceId& effectInstanceId, const std::string& presetName) = 0;
    virtual muse::Ret deletePreset(const EffectId& effectId, const PresetId& presetId) = 0;
    virtual muse::Ret importPreset(const EffectInstanceId& effectInstanceId, const muse::io::path_t& filePath) = 0;
    virtual muse::Ret exportPreset(const EffectInstanceId& effectInstanceId, const muse::io::path_t& filePath) = 0;
};
}
