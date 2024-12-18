/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"

#include "../iprojectsceneactionscontroller.h"
#include "../iprojectsceneconfiguration.h"

namespace au::projectscene {
class ProjectSceneActionsController : public IProjectSceneActionsController, public muse::actions::Actionable, public muse::async::Asyncable
{
    INJECT_STATIC(muse::actions::IActionsDispatcher, dispatcher)
    INJECT_STATIC(au::context::IGlobalContext, globalContext)
    INJECT_STATIC(muse::IInteractive, interactive)
    muse::Inject<IProjectSceneConfiguration> configuration;

public:
    void init();

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;
    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    void toggleMinutesSecondsRuler();
    void toggleBeatsMeasuresRuler();
    void toggleVerticalRulers();
    void updateDisplayWhilePlaying();
    void pinnedPlayHead();

    void openClipPitchAndSpeedEdit(const muse::actions::ActionData& args);

    void insertSilence(const muse::actions::ActionData& args);

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
};
}
