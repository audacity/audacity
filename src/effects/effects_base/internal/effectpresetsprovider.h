/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectpresetsprovider.h"

#include "modularity/ioc.h"
#include "../ieffectsprovider.h"
#include "../ieffectinstancesregister.h"

#include "../effectstypes.h"

class EffectSettingsManager;
namespace au::effects {
class EffectPresetsProvider : public IEffectPresetsProvider
{
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectInstancesRegister> instancesRegister;

public:
    EffectPresetsProvider() = default;

    PresetIdList factoryPresets(const EffectId& effectId) const override;
    PresetIdList userPresets(const EffectId& effectId) const override;
    muse::async::Channel<EffectId> userPresetsChanged() const override;

    muse::Ret applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) override;
    muse::Ret saveCurrentAsPreset(const EffectInstanceId& effectInstanceId, const std::string& presetName) override;
    muse::Ret deletePreset(const EffectId& effectId, const PresetId& presetId) override;
    muse::Ret importPreset(const EffectInstanceId& effectInstanceId, const muse::io::path_t& filePath) override;
    muse::Ret exportPreset(const EffectInstanceId& effectInstanceId, const muse::io::path_t& filePath) override;

private:

    const EffectSettingsManager& settingsManager(const EffectId& effectId) const;

    muse::async::Channel<EffectId> m_userPresetsChanged;
};
}
