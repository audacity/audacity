/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "framework/actions/iactionsdispatcher.h"
#include "framework/actions/actionable.h"
#include "framework/interactive/iinteractive.h"

#include "context/iglobalcontext.h"
#include "../iprojectsceneactionscontroller.h"
#include "../iprojectsceneconfiguration.h"

namespace au::projectscene {
class ProjectSceneActionsController : public IProjectSceneActionsController, public muse::actions::Actionable,
    public muse::async::Asyncable, public muse::Injectable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<muse::IInteractive> interactive { this };
    muse::GlobalInject<IProjectSceneConfiguration> configuration;

public:
    ProjectSceneActionsController(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void init();

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;
    bool canReceiveAction(const muse::actions::ActionCode& code) const override;
    muse::async::Channel<muse::actions::ActionCode> actionEnabledChanged() const override;

private:
    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    void toggleMinutesSecondsRuler();
    void toggleBeatsMeasuresRuler();
    void toggleVerticalRulers();
    void toggleRMSInWaveform();
    void toggleClippingInWaveform();
    void toggleUpdateDisplayWhilePlaying();
    void togglePinnedPlayHead();
    void togglePlaybackOnRulerClickEnabled();
    void toggleTrackHalfWave(const muse::actions::ActionQuery& q);

    void changeFontForLabels();

    void openClipPitchAndSpeedEdit(const muse::actions::ActionData& args);

    void openLabelEditor();

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
    muse::async::Channel<muse::actions::ActionCode> m_actionEnabledChanged;
};
}
