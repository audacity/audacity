/*
* Audacity: A Digital Audio Editor
*/
#include "effectspresetscontroller.h"

#include "global/containers.h"

#include "libraries/lib-effects/Effect.h"
#include "libraries/lib-effects/EffectManager.h"

#include "au3wrap/internal/wxtypes_convert.h"

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

muse::async::Channel<EffectId> EffectsPresetsController::userPresetsChanged() const
{
    return m_userPresetsChanged;
}

Ret EffectsPresetsController::applyPreset(const EffectInstanceId& effectInstanceId, const PresetId& presetId)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    const EffectSettingsManager& sm = settingsManager(effectId);
    EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);

    Ret ret;

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

Ret EffectsPresetsController::saveCurrentAsPreset(const EffectInstanceId& effectInstanceId)
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(effectInstanceId);
    const EffectSettingsManager& sm = settingsManager(effectId);
    EffectSettings* settings = instancesRegister()->settingsById(effectInstanceId);

    RetVal<Val> rv = interactive()->open("audacity://effects/presets/input_name");
    if (!rv.ret) {
        return rv.ret;
    }

    std::string name = rv.val.toString();
    if (name.empty()) {
        return muse::make_ret(Ret::Code::Cancel);
    }

    bool ok = sm.SaveUserPreset(UserPresetsGroup(wxString(name)), *settings);

    if (ok) {
        m_userPresetsChanged.send(effectId);
    }

    return ok ? muse::make_ok() : muse::make_ret(Ret::Code::InternalError);
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
