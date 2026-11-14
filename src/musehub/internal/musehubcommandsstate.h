/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/rcommand/imodulecommandsstate.h"

namespace au::musehub {
class MuseHubCommandsState : public muse::rcommand::IModuleCommandsState
{
public:
    std::string moduleName() const override;
    void init() override;
    void deinit() override;

    muse::rcommand::CommandState commandState(const muse::rcommand::Command& command) const override;
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> commandStateChanged() const override;
};
}
