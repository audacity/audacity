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
#include "effects/effects_base/irealtimeeffectservice.h"
#include "context/iglobalcontext.h"
#include "../ieffectspresetsprovider.h"
#include "iinteractive.h"

namespace au::effects {
class EffectsActionsController : public muse::actions::Actionable, public muse::async::Asyncable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<IEffectExecutionScenario> effectExecutionScenario;
    muse::Inject<IRealtimeEffectService> realtimeEffectService;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectsPresetsProvider> presetsController;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    void init();
    bool canReceiveAction(const muse::actions::ActionCode&) const override;
    muse::async::Channel<muse::actions::ActionCodeList> canReceiveActionsChanged() const;

private:
    void doEffect(const muse::actions::ActionData& args);
    void repeatLastEffect();
    void addRealtimeEffect(const muse::actions::ActionData& args);
    void removeRealtimeEffect(const muse::actions::ActionData& args);
    void replaceRealtimeEffect(const muse::actions::ActionData& args);

    void applyPreset(const muse::actions::ActionData& args);
    void saveAsPreset(const muse::actions::ActionData& args);
    void deletePreset(const muse::actions::ActionData& args);
    void importPreset(const muse::actions::ActionData& args);
    void exportPreset(const muse::actions::ActionData& args);

    muse::async::Channel<muse::actions::ActionCodeList> m_canReceiveActionsChanged;
};
}
