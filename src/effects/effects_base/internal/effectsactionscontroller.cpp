/*
* Audacity: A Digital Audio Editor
*/
#include "effectsactionscontroller.h"

#include "log.h"

using namespace au::effects;

static const muse::actions::ActionCode EFFECT_OPEN_CODE = "effect-open";

void EffectsActionsController::init()
{
    dispatcher()->reg(this, EFFECT_OPEN_CODE, this, &EffectsActionsController::doEffect);
    dispatcher()->reg(this, "repeat-last-effect", this, &EffectsActionsController::repeatLastEffect);
    effectExecutionScenario()->lastProcessorIsNowAvailable().onNotify(this, [this] {
        m_canReceiveActionsChanged.send({ "repeat-last-effect" });
    });
}

void EffectsActionsController::doEffect(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() > 0) {
        return;
    }

    muse::String effectId = args.arg<muse::String>(0);

    const auto ret = effectExecutionScenario()->performEffect(effectId);
    if (!ret && muse::Ret::Code(ret.code()) != muse::Ret::Code::Cancel) {
        const auto& title = effectsProvider()->meta(effectId).title;
        interactive()->error(title.toStdString(), ret.text());
    }
}

void EffectsActionsController::repeatLastEffect()
{
    effectExecutionScenario()->repeatLastProcessor();
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
