/*
* Audacity: A Digital Audio Editor
*/
#include "effectsactionscontroller.h"
#include "effects/effects_base/effectstypes.h"

#include "wx/string.h"

#include "log.h"

using namespace au::effects;

void EffectsActionsController::init()
{
    dispatcher()->reg(this, "effect-open", this, &EffectsActionsController::doEffect);
    dispatcher()->reg(this, "repeat-last-effect", this, &EffectsActionsController::repeatLastEffect);
    dispatcher()->reg(this, "realtimeeffect-add", this, &EffectsActionsController::addRealtimeEffect);
    dispatcher()->reg(this, "realtimeeffect-remove", this, &EffectsActionsController::removeRealtimeEffect);
    dispatcher()->reg(this, "realtimeeffect-replace", this, &EffectsActionsController::replaceRealtimeEffect);

    effectExecutionScenario()->lastProcessorIsNowAvailable().onNotify(this, [this] {
        m_canReceiveActionsChanged.send({ "repeat-last-effect" });
    });

    // presets
    dispatcher()->reg(this, "action://effects/presets/apply", this, &EffectsActionsController::applyPreset);
    dispatcher()->reg(this, "action://effects/presets/save", this, &EffectsActionsController::saveAsPreset);
    dispatcher()->reg(this, "action://effects/presets/delete", this, &EffectsActionsController::deletePreset);
    dispatcher()->reg(this, "action://effects/presets/import", this, &EffectsActionsController::importPreset);
    dispatcher()->reg(this, "action://effects/presets/export", this, &EffectsActionsController::exportPreset);
}

void EffectsActionsController::doEffect(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() > 0) {
        return;
    }

    muse::String effectId = args.arg<muse::String>(0);
    effectExecutionScenario()->performEffect(effectId);
}

void EffectsActionsController::repeatLastEffect()
{
    effectExecutionScenario()->repeatLastProcessor();
}

void EffectsActionsController::addRealtimeEffect(const muse::actions::ActionData& args)
{
    const auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        // Command issued without an open project ?..
        return;
    }

    const auto effectId = args.arg<EffectId>(0);
    const auto trackId = args.arg<TrackId>(1);
    realtimeEffectService()->addRealtimeEffect(*project, trackId, effectId);
}

void EffectsActionsController::removeRealtimeEffect(const muse::actions::ActionData& args)
{
    const auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        // Command issued without an open project ?..
        return;
    }

    const auto trackId = args.arg<TrackId>(0);
    const auto effectStateId = args.arg<EffectStateId>(1);
    realtimeEffectService()->removeRealtimeEffect(*project, trackId, effectStateId);
}

void EffectsActionsController::replaceRealtimeEffect(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    const auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        // Command issued without an open project ?..
        return;
    }

    const auto trackId = args.arg<TrackId>(0);
    const auto srcIndex = args.arg<int>(1);
    const auto dstEffectId = args.arg<EffectId>(2);

    realtimeEffectService()->replaceRealtimeEffect(*project, trackId, srcIndex, dstEffectId);
}

void EffectsActionsController::applyPreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 2) {
        return;
    }

    EffectId effectId = args.arg<EffectId>(0);
    PresetId presetId = args.arg<PresetId>(1);
    presetsController()->applyPreset(effectId, presetId);
}

void EffectsActionsController::saveAsPreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    EffectId effectId = args.arg<EffectId>(0);
    presetsController()->saveCurrentAsPreset(effectId);
}

void EffectsActionsController::deletePreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 2) {
        return;
    }

    EffectId effectId = args.arg<EffectId>(0);
    PresetId presetId = args.arg<PresetId>(1);
    presetsController()->deletePreset(effectId, presetId);
}

void EffectsActionsController::importPreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    EffectId effectId = args.arg<EffectId>(0);
    presetsController()->importPreset(effectId);
}

void EffectsActionsController::exportPreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    EffectId effectId = args.arg<EffectId>(0);
    presetsController()->exportPreset(effectId);
}

bool EffectsActionsController::canReceiveAction(const muse::actions::ActionCode& code) const
{
    if (code == "repeat-last-effect") {
        return effectExecutionScenario()->lastProcessorIsAvailable();
    } else {
        return true;
    }
}

muse::async::Channel<muse::actions::ActionCodeList> EffectsActionsController::canReceiveActionsChanged() const
{
    return m_canReceiveActionsChanged;
}
