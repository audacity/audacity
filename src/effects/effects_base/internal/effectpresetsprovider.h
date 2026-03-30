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
class EffectPresetsProvider : public IEffectPresetsProvider, public muse::Contextable
{
    muse::GlobalInject<IEffectsProvider> effectsProvider;
    muse::GlobalInject<IEffectInstancesRegister> instancesRegister;

public:
    EffectPresetsProvider(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    PresetIdList factoryPresets(const EffectId& effectId) const override;
    PresetIdList userPresets(const EffectId& effectId) const override;
    muse::async::Channel<EffectId> userPresetsChanged() const override;
    muse::async::Channel<PresetSavedInfo> presetSaved() const override;

    muse::Ret applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) override;
    bool hasUserPresetWithName(const EffectId& effectId, const std::string& presetName) const override;
    muse::Ret saveCurrentAsPreset(const EffectInstanceId& effectInstanceId, const std::string& presetName) override;
    muse::Ret deletePreset(const EffectId& effectId, const PresetId& presetId) override;
    muse::Ret importPreset(const EffectInstanceId& effectInstanceId, const muse::io::path_t& filePath) override;
    muse::Ret exportPreset(const EffectInstanceId& effectInstanceId, const muse::io::path_t& filePath) override;

private:

    const EffectSettingsManager& settingsManager(const EffectId& effectId) const;

    muse::async::Channel<EffectId> m_userPresetsChanged;
    muse::async::Channel<PresetSavedInfo> m_presetSaved;
};
}
