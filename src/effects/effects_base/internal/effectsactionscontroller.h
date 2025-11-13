/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/iinteractive.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/async/channel.h"
#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/ui/iuiactionsregister.h"

#include "playback/iplaybackcontroller.h"
#include "../ieffectexecutionscenario.h"
#include "../ieffectsprovider.h"
#include "../ieffectpresetsscenario.h"

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
    muse::Inject<au::playback::IPlaybackController> playbackController;

public:
    void init();
    bool canReceiveAction(const muse::actions::ActionCode&) const override;
    muse::async::Channel<muse::actions::ActionCodeList> canReceiveActionsChanged() const;

private:
    void registerActions();

    void onEffectTriggered(const muse::actions::ActionQuery& q);
    void repeatLastEffect();

    void applyPreset(const muse::actions::ActionQuery& q);
    void saveAsPreset(const muse::actions::ActionQuery& q);
    void deletePreset(const muse::actions::ActionQuery& q);
    void importPreset(const muse::actions::ActionQuery& q);
    void exportPreset(const muse::actions::ActionQuery& q);

    std::shared_ptr<EffectsUiActions> m_uiActions;
    muse::async::Channel<muse::actions::ActionCodeList> m_canReceiveActionsChanged;
};
}
