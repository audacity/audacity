/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/imodulecommandsstate.h"

namespace au::macromanager {
class MacrosCommandsState : public muse::rcommand::IModuleCommandsState, public muse::Contextable
{
public:
    MacrosCommandsState(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    std::string moduleName() const override;

    void init() override;
    void deinit() override;

    muse::rcommand::CommandState commandState(const muse::rcommand::Command& command) const override;
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> commandStateChanged() const override;

private:
    muse::async::Channel<muse::rcommand::Command, muse::rcommand::CommandState> m_commandStateChanged;
};
}
