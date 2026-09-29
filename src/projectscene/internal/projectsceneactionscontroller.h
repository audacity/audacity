/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "framework/actions/iactionsdispatcher.h"
#include "framework/actions/actionable.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/interactive/iinteractive.h"

#include "context/iglobalcontext.h"
#include "../iprojectsceneactionscontroller.h"
#include "../iprojectsceneconfiguration.h"
#include "../iprojectsceneuistate.h"

namespace au::projectscene {
class ProjectSceneActionsController : public IProjectSceneActionsController, public muse::actions::Actionable,
    public muse::rcommand::Commandable, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<IProjectSceneConfiguration> configuration;
    muse::ContextInject<IProjectSceneUiState> projectSceneUiState { this };

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<muse::IInteractive> interactive { this };

public:
    ProjectSceneActionsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;
    bool canReceiveAction(const muse::actions::ActionCode& code) const override;
    muse::async::Channel<muse::actions::ActionCode> actionEnabledChanged() const override;

private:
    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    muse::Ret toggleMinutesSecondsRuler();
    muse::Ret toggleBeatsMeasuresRuler();
    muse::Ret toggleVerticalRulers();
    muse::Ret toggleRMSInWaveform();
    muse::Ret toggleClippingInWaveform();
    muse::Ret toggleUpdateDisplayWhilePlaying();
    muse::Ret togglePinnedPlayHead();
    muse::Ret togglePlaybackOnRulerClickEnabled();
    muse::Ret toggleAutomation();
    muse::Ret toggleTrackHalfWave(const muse::rcommand::Params& params);

    void changeFontForLabels();

    muse::Ret openClipPitchAndSpeedEdit(const muse::rcommand::Params& params);

    muse::Ret openLabelEditor();

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
    muse::async::Channel<muse::actions::ActionCode> m_actionEnabledChanged;
};
}
