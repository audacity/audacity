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
#include "framework/ui/inavigationcontroller.h"

#include "context/iglobalcontext.h"
#include "../iprojectsceneactionscontroller.h"
#include "../iplaypositionviewcontroller.h"
#include "../itimelineviewcontroller.h"
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
    muse::ContextInject<muse::ui::INavigationController> navigationController { this };

public:
    ProjectSceneActionsController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    void setTimelineViewController(ITimelineViewController* controller) override;
    ITimelineViewController* timelineViewController() const override;

    void setPlayPositionViewController(IPlayPositionViewController* controller) override;
    IPlayPositionViewController* playPositionViewController() const override;

    muse::async::Notification effectsPanelFocusRequested() const override;
    muse::async::Notification audioSetupContextMenuRequested() const override;
    muse::async::Notification timelineContextMenuRequested() const override;
    muse::async::Notification splitToolToggleRequested() const override;
    muse::async::Notification realtimeEffectMoveUpRequested() const override;
    muse::async::Notification realtimeEffectMoveDownRequested() const override;

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;
    bool canReceiveAction(const muse::actions::ActionCode& code) const override;
    muse::async::Channel<muse::actions::ActionCode> actionEnabledChanged() const override;

private:
    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    template<typename ViewController>
    void registerViewCommand(const muse::rcommand::Command& command, ViewController * ProjectSceneActionsController::* view,
                             void (ViewController::* handler)());
    muse::Ret centerViewOnPlayhead(const muse::rcommand::Params& params);

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

    muse::Ret toggleEffectsPanel();
    muse::Ret requestAudioSetupContextMenu();
    muse::Ret openGetEffectsDialog();

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
    muse::async::Channel<muse::actions::ActionCode> m_actionEnabledChanged;
    muse::async::Notification m_effectsPanelFocusRequested;
    muse::async::Notification m_audioSetupContextMenuRequested;
    muse::async::Notification m_timelineContextMenuRequested;
    muse::async::Notification m_splitToolToggleRequested;
    muse::async::Notification m_realtimeEffectMoveUpRequested;
    muse::async::Notification m_realtimeEffectMoveDownRequested;

    ITimelineViewController* m_timelineViewController = nullptr;
    IPlayPositionViewController* m_playPositionViewController = nullptr;
};
}
