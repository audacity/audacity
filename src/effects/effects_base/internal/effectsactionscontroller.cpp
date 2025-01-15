/*
* Audacity: A Digital Audio Editor
*/
#include "effectsactionscontroller.h"
#include "effects/effects_base/effectstypes.h"
#include "effectsuiactions.h"
#include "playback/iplayer.h"

#include "wx/string.h"

#include "log.h"

using namespace muse::actions;
using namespace au::effects;

void EffectsActionsController::init()
{
    m_uiActions = std::make_shared<EffectsUiActions>(shared_from_this());

    effectsProvider()->effectMetaListChanged().onNotify(this, [this](){
        registerActions();
    });

    registerActions();

    effectExecutionScenario()->lastProcessorIsNowAvailable().onNotify(this, [this] {
        m_canReceiveActionsChanged.send({ "repeat-last-effect" });
    });
}

void EffectsActionsController::registerActions()
{
    dispatcher()->unReg(this);

    EffectMetaList effects = effectsProvider()->effectMetaList();
    for (const EffectMeta& e : effects) {
        dispatcher()->reg(this, makeEffectOpenAction(e.id), [this](const ActionQuery& q) {
            onEffectTriggered(q);
        });
    }

    dispatcher()->reg(this, "repeat-last-effect", this, &EffectsActionsController::repeatLastEffect);
    dispatcher()->reg(this, "realtimeeffect-add", this, &EffectsActionsController::addRealtimeEffect);
    dispatcher()->reg(this, "realtimeeffect-remove", this, &EffectsActionsController::removeRealtimeEffect);
    dispatcher()->reg(this, "realtimeeffect-replace", this, &EffectsActionsController::replaceRealtimeEffect);

    // presets
    dispatcher()->reg(this, "action://effects/presets/apply", this, &EffectsActionsController::applyPreset);
    dispatcher()->reg(this, "action://effects/presets/save", this, &EffectsActionsController::saveAsPreset);
    dispatcher()->reg(this, "action://effects/presets/delete", this, &EffectsActionsController::deletePreset);
    dispatcher()->reg(this, "action://effects/presets/import", this, &EffectsActionsController::importPreset);
    dispatcher()->reg(this, "action://effects/presets/export", this, &EffectsActionsController::exportPreset);

    m_uiActions->reload();
    uiActionsRegister()->reg(m_uiActions);
}

void EffectsActionsController::onEffectTriggered(const muse::actions::ActionQuery& q)
{
    muse::String effectId = muse::String::fromStdString(q.param("effectId").toString());
    IF_ASSERT_FAILED(!effectId.empty()) {
        return;
    }

    playback()->player()->stop();

    effectExecutionScenario()->performEffect(effectId);
}

void EffectsActionsController::repeatLastEffect()
{
    playback()->player()->stop();
    effectExecutionScenario()->repeatLastProcessor();
}

void EffectsActionsController::addRealtimeEffect(const muse::actions::ActionData& args)
{
    const auto effectId = args.arg<EffectId>(0);
    const auto trackId = args.arg<TrackId>(1);
    if (const RealtimeEffectStatePtr state = realtimeEffectService()->addRealtimeEffect(trackId, effectId)) {
        effectsProvider()->showEffect(state);
    }
}

void EffectsActionsController::removeRealtimeEffect(const muse::actions::ActionData& args)
{
    const auto trackId = args.arg<TrackId>(0);
    const auto effectStateId = args.arg<RealtimeEffectStatePtr>(1);
    realtimeEffectService()->removeRealtimeEffect(trackId, effectStateId);
}

void EffectsActionsController::replaceRealtimeEffect(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    const auto trackId = args.arg<TrackId>(0);
    const auto srcIndex = args.arg<int>(1);
    const auto dstEffectId = args.arg<EffectId>(2);

    realtimeEffectService()->replaceRealtimeEffect(trackId, srcIndex, dstEffectId);
}

void EffectsActionsController::applyPreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 2) {
        return;
    }

    EffectInstanceId effectInstanceId = args.arg<EffectInstanceId>(0);
    PresetId presetId = args.arg<PresetId>(1);
    presetsScenario()->applyPreset(effectInstanceId, presetId);
}

void EffectsActionsController::saveAsPreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    EffectInstanceId effectInstanceId = args.arg<EffectInstanceId>(0);
    presetsScenario()->saveCurrentAsPreset(effectInstanceId);
}

void EffectsActionsController::deletePreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 2) {
        return;
    }

    EffectId effectId = args.arg<EffectId>(0);
    PresetId presetId = args.arg<PresetId>(1);
    presetsScenario()->deletePreset(effectId, presetId);
}

void EffectsActionsController::importPreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    EffectInstanceId effectInstanceId = args.arg<EffectInstanceId>(0);
    presetsScenario()->importPreset(effectInstanceId);
}

void EffectsActionsController::exportPreset(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    EffectInstanceId effectInstanceId = args.arg<EffectInstanceId>(0);
    presetsScenario()->exportPreset(effectInstanceId);
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
