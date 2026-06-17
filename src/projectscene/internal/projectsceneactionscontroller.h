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
#include "trackedit/internal/itracknavigationcontroller.h"
#include "../iprojectsceneactionscontroller.h"
#include "../iprojectsceneconfiguration.h"

namespace au::projectscene {
class ProjectSceneActionsController : public IProjectSceneActionsController, public muse::actions::Actionable,
    public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<IProjectSceneConfiguration> configuration;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<trackedit::ITrackNavigationController> trackNavigationController { this };

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

    void toggleMinutesSecondsRuler();
    void toggleBeatsMeasuresRuler();
    void toggleVerticalRulers();
    void toggleRMSInWaveform();
    void toggleClippingInWaveform();
    void toggleUpdateDisplayWhilePlaying();
    void togglePinnedPlayHead();
    void togglePlaybackOnRulerClickEnabled();
    void toggleAutomation();
    void toggleTrackHalfWave(const muse::actions::ActionQuery& q);
    void collapseAllTrackHeights();
    void expandAllTrackHeights();
    void autoFitTrackHeights();
    void collapseTrackHeight(const muse::actions::ActionData& args);
    void expandTrackHeight(const muse::actions::ActionData& args);

    void changeFontForLabels();

    void openClipPitchAndSpeedEdit(const muse::actions::ActionData& args);

    void openLabelEditor();
    trackedit::TrackId trackIdFromArgsOrFocus(const muse::actions::ActionData& args) const;

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
    muse::async::Channel<muse::actions::ActionCode> m_actionEnabledChanged;
};
}
