/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "async/asyncable.h"
#include "async/channel.h"
#include "actions/actionable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "ui/iuiactionsregister.h"
#include "../ieffectexecutionscenario.h"
#include "../ieffectsprovider.h"
#include "../ieffectpresetsscenario.h"
#include "iinteractive.h"
#include "playback/iplayback.h"

namespace au::effects {
class EffectsUiActions;
class EffectsActionsController : public muse::actions::Actionable, public muse::async::Asyncable,
    public std::enable_shared_from_this<EffectsActionsController>
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<muse::ui::IUiActionsRegister> uiActionsRegister;
    muse::Inject<IEffectExecutionScenario> effectExecutionScenario;
    muse::Inject<IEffectsProvider> effectsProvider;
    muse::Inject<IEffectPresetsScenario> presetsScenario;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<au::playback::IPlayback> playback;

public:
    void init();
    bool canReceiveAction(const muse::actions::ActionCode&) const override;
    muse::async::Channel<muse::actions::ActionCodeList> canReceiveActionsChanged() const;

private:
    void registerActions();

    void onEffectTriggered(const muse::actions::ActionQuery& q);
    void repeatLastEffect();

    void applyPreset(const muse::actions::ActionData& args);
    void saveAsPreset(const muse::actions::ActionData& args);
    void deletePreset(const muse::actions::ActionData& args);
    void importPreset(const muse::actions::ActionData& args);
    void exportPreset(const muse::actions::ActionData& args);

    std::shared_ptr<EffectsUiActions> m_uiActions;
    muse::async::Channel<muse::actions::ActionCodeList> m_canReceiveActionsChanged;
};
}
