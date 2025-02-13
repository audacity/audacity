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

    // presets
    dispatcher()->reg(this, ActionQuery("action://effects/presets/apply"), this, &EffectsActionsController::applyPreset);
    dispatcher()->reg(this, ActionQuery("action://effects/presets/save"), this, &EffectsActionsController::saveAsPreset);
    dispatcher()->reg(this, ActionQuery("action://effects/presets/delete"), this, &EffectsActionsController::deletePreset);
    dispatcher()->reg(this, ActionQuery("action://effects/presets/import"), this, &EffectsActionsController::importPreset);
    dispatcher()->reg(this, ActionQuery("action://effects/presets/export"), this, &EffectsActionsController::exportPreset);

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

void EffectsActionsController::applyPreset(const muse::actions::ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("instanceId") && q.contains("presetId")) {
        return;
    }

    EffectInstanceId effectInstanceId = q.param("instanceId").toInt();
    PresetId presetId = q.param("presetId").toString();
    presetsScenario()->applyPreset(effectInstanceId, presetId);
}

void EffectsActionsController::saveAsPreset(const ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("instanceId")) {
        return;
    }

    EffectInstanceId effectInstanceId = q.param("instanceId").toInt();
    presetsScenario()->saveCurrentAsPreset(effectInstanceId);
}

void EffectsActionsController::deletePreset(const ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("effectId") && q.contains("presetId")) {
        return;
    }

    EffectId effectId = EffectId::fromStdString(q.param("effectId").toString());
    PresetId presetId = q.param("presetId").toString();
    presetsScenario()->deletePreset(effectId, presetId);
}

void EffectsActionsController::importPreset(const ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("instanceId")) {
        return;
    }

    EffectInstanceId effectInstanceId = q.param("instanceId").toInt();
    presetsScenario()->importPreset(effectInstanceId);
}

void EffectsActionsController::exportPreset(const ActionQuery& q)
{
    IF_ASSERT_FAILED(q.contains("instanceId")) {
        return;
    }

    EffectInstanceId effectInstanceId = q.param("instanceId").toInt();
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
