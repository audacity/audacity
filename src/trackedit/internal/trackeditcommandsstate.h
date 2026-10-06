/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <map>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/icommandsregister.h"
#include "framework/rcommand/imodulecommandsstate.h"

#include "context/iglobalcontext.h"
#include "../iprojecthistory.h"
#include "../iselectioncontroller.h"
#include "../itrackeditinteraction.h"
#include "itracknavigationcontroller.h"

namespace au::trackedit {
class TrackeditCommandsState : public muse::rcommand::IModuleCommandsState, public muse::Contextable, public muse::async::Asyncable
{
    muse::GlobalInject<muse::rcommand::ICommandsRegister> commandsRegister;
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<IProjectHistory> projectHistory{ this };
    muse::ContextInject<ISelectionController> selectionController{ this };
    muse::ContextInject<ITrackeditInteraction> trackeditInteraction{ this };
    muse::ContextInject<ITrackNavigationController> trackNavigationController{ this };

public:
    TrackeditCommandsState(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    std::string moduleName() const override;

    void init() override;
    void deinit() override;

    muse::rcommand::CommandState commandState(const muse::rcommand::Command& command) const override;
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> commandStateChanged() const override;

private:
    void updateCommandStates(const std::vector<muse::rcommand::Command>& commands = {});

    ClipKeyList clipsForInteraction() const;
    LabelKeyList labelsForInteraction() const;
    bool canSilenceAudio() const;

    muse::rcommand::IModuleCommandsRegisterPtr m_moduleRegister;
    std::map<muse::rcommand::Command, muse::rcommand::CommandState> m_commandStates;
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> m_commandStateChanged;
};
}
