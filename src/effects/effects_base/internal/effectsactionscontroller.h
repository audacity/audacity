/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "async/asyncable.h"
#include "async/channel.h"
#include "actions/actionable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "../ieffectexecutionscenario.h"
#include "../ieffectsprovider.h"
#include "iinteractive.h"

namespace au::effects {
class EffectsActionsController : public muse::actions::Actionable, public muse::async::Asyncable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<IEffectExecutionScenario> effectExecutionScenario;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<muse::IInteractive> interactive;

public:
    void init();
    bool canReceiveAction(const muse::actions::ActionCode&) const override;
    muse::async::Channel<muse::actions::ActionCodeList> canReceiveActionsChanged() const;

private:
    void doEffect(const muse::actions::ActionData& args);
    void repeatLastEffect();

    muse::async::Channel<muse::actions::ActionCodeList> m_canReceiveActionsChanged;
};
}
