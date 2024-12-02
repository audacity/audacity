/*
* Audacity: A Digital Audio Editor
*/
#include "effectspresetscontroller.h"

#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-effects/EffectManager.h"

#include "log.h"

using namespace au::effects;

PresetIdList EffectsPresetsController::factoryPresets(const EffectId& effectId) const
{
    Effect* effect = effectsProvider()->effect(effectId);
    IF_ASSERT_FAILED(effect) {
        return {};
    }
    PresetIdList presets = effect->GetDefinition().GetFactoryPresets();
    return presets;
}

PresetIdList EffectsPresetsController::userPresets(const EffectId& effectId) const
{
    Effect* effect = effectsProvider()->effect(effectId);
    IF_ASSERT_FAILED(effect) {
        return {};
    }
    PresetIdList presets = GetUserPresets(*effect);
    return presets;
}

void EffectsPresetsController::applyPreset(const EffectId& effectId, const PresetId& presetId)
{
    UNUSED(effectId);
    UNUSED(presetId);
    interactive()->warning("Apply Preset", std::string("Not implemented"));
}

void EffectsPresetsController::saveCurrentAsPreset(const EffectId& effectId)
{
    UNUSED(effectId);
    interactive()->warning("Save Current As Preset", std::string("Not implemented"));
}

void EffectsPresetsController::deletePreset(const EffectId& effectId, const PresetId& presetId)
{
    UNUSED(effectId);
    UNUSED(presetId);
    interactive()->warning("Delete Preset", std::string("Not implemented"));
}

void EffectsPresetsController::importPreset(const EffectId& effectId)
{
    UNUSED(effectId);
    interactive()->warning("Import Preset", std::string("Not implemented"));
}

void EffectsPresetsController::exportPreset(const EffectId& effectId)
{
    UNUSED(effectId);
    interactive()->warning("Export Preset", std::string("Not implemented"));
}
