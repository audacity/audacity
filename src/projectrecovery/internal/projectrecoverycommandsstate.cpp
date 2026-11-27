/*
 * Audacity: A Digital Audio Editor
 */
#include "projectrecoverycommandsstate.h"

using namespace au::projectrecovery;

std::string ProjectRecoveryCommandsState::moduleName() const
{
    return "projectrecovery";
}

void ProjectRecoveryCommandsState::init()
{
}

void ProjectRecoveryCommandsState::deinit()
{
}

muse::rcommand::CommandState ProjectRecoveryCommandsState::commandState(const muse::rcommand::Command&) const
{
    return muse::rcommand::CommandState(true, false);
}

muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> ProjectRecoveryCommandsState::commandStateChanged() const
{
    return {};
}
