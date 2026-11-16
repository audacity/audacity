/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"

#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/rcommand/icommandsregister.h"

#include "trackedit/itrackeditconfiguration.h"

#include "trackedit/trackedittypes.h"

namespace au::trackedit {
class DeleteBehaviorOnboardingScenario : public muse::Contextable
{
    muse::GlobalInject<ITrackeditConfiguration> configuration;
    muse::GlobalInject<muse::rcommand::ICommandsRegister> commandsRegister;

    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher { this };

public:
    DeleteBehaviorOnboardingScenario(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    bool showOnboardingDialog() const;
    void showFollowupDialog() const;

private:
    bool commandExists(const muse::rcommand::Command& command) const;
};
}
