/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../ieffectspresetscontroller.h"

#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "../ieffectsprovider.h"

#include "../effectstypes.h"

namespace au::effects {
class EffectsPresetsController : public IEffectsPresetsController
{
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<muse::IInteractive> interactive;

public:
    EffectsPresetsController() = default;

    PresetIdList factoryPresets(const EffectId& effectId) const override;
    PresetIdList userPresets(const EffectId& effectId) const override;

    void applyPreset(const EffectId& effectId, const PresetId& presetId) override;
    void saveCurrentAsPreset(const EffectId& effectId) override;
    void deletePreset(const EffectId& effectId, const PresetId& presetId) override;
    void importPreset(const EffectId& effectId) override;
    void exportPreset(const EffectId& effectId) override;
};
}
