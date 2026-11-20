/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/rcommand/imodulecommandsregister.h"

namespace au::projectrecovery {
class ProjectRecoveryCommandsRegister : public muse::rcommand::IModuleCommandsRegister
{
public:
    std::string moduleName() const override;
    const std::vector<muse::rcommand::Command>& commandList() const override;
    const std::vector<muse::rcommand::CommandInfo>& commandInfoList() const override;
};
}
