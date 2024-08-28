/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "async/asyncable.h"
#include "actions/actionable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "../ieffectexecutionscenario.h"

namespace au::effects {
class EffectsActionsController : public muse::actions::Actionable, public muse::async::Asyncable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<IEffectExecutionScenarion> effectExecutionScenarion;

public:
    void init();

private:
    void doEffect(const muse::actions::ActionData& args);
};
}
