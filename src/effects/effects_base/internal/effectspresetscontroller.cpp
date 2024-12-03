/*
* Audacity: A Digital Audio Editor
*/
#include "effectspresetscontroller.h"

#include "global/containers.h"

#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-effects/EffectManager.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

const EffectSettingsManager& EffectsPresetsController::settingsManager(const EffectId& effectId) const
{
    Effect* effect = effectsProvider()->effect(effectId);
    DO_ASSERT(effect);
    return effect->GetDefinition();
}

PresetIdList EffectsPresetsController::factoryPresets(const EffectId& effectId) const
{
    const EffectSettingsManager& sm = settingsManager(effectId);
    return sm.GetFactoryPresets();
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

muse::Ret EffectsPresetsController::applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    const EffectSettingsManager& sm = settingsManager(effectId);
    EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);

    muse::Ret ret;

    // try apply factory
    bool isFactory = false;
    {
        PresetIdList presets = factoryPresets(effectId);
        int idx = muse::indexOf(presets, presetId);
        isFactory = idx >= 0;
        if (isFactory) {
            OptionalMessage msg = sm.LoadFactoryPreset(idx, *settings);
            ret = msg ? muse::make_ok() : muse::make_ret(Ret::Code::InternalError);
            if (!ret) {
                LOGE() << "failed load factory preset";
            }
        }
    }

    // try user
    if (!isFactory) {
        OptionalMessage msg = sm.LoadUserPreset(presetId, *settings);
        ret = msg ? muse::make_ok() : muse::make_ret(Ret::Code::InternalError);
        if (!ret) {
            LOGE() << "failed load user preset";
        }
    }

    if (ret) {
        instancesRegister()->notifyAboutSettingsChanged(effectInstanceId);
    }

    return ret;
}

void EffectsPresetsController::saveCurrentAsPreset(const EffectInstanceId& effectInstanceId)
{
    UNUSED(effectInstanceId);
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
