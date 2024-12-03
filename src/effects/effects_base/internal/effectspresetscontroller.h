/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectspresetscontroller.h"

#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "../ieffectsprovider.h"
#include "../ieffectinstancesregister.h"

#include "../effectstypes.h"

class EffectSettingsManager;
namespace au::effects {
class EffectsPresetsController : public IEffectsPresetsController
{
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<muse::IInteractive> interactive;

public:
    EffectsPresetsController() = default;

    PresetIdList factoryPresets(const EffectId& effectId) const override;
    PresetIdList userPresets(const EffectId& effectId) const override;
    muse::async::Channel<EffectId> userPresetsChanged() const override;

    muse::Ret applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId) override;
    muse::Ret saveCurrentAsPreset(const EffectInstanceId& effectInstanceId) override;
    void deletePreset(const EffectId& effectId, const PresetId& presetId) override;
    void importPreset(const EffectId& effectId) override;
    void exportPreset(const EffectId& effectId) override;

private:

    const EffectSettingsManager& settingsManager(const EffectId& effectId) const;

    muse::async::Channel<EffectId> m_userPresetsChanged;
};
}
