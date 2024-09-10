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
}

void EffectsActionsController::doEffect(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() > 0) {
        return;
    }

    muse::String effectId = args.arg<muse::String>(0);

    effectExecutionScenarion()->performEffect(effectId);
}
