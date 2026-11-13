/*
 * Audacity: A Digital Audio Editor
 */
#include "projectrecoverycommandsregister.h"

#include "../projectrecoverycommands.h"

using namespace au::projectrecovery;

std::string ProjectRecoveryCommandsRegister::moduleName() const
{
    return "projectrecovery";
}

const std::vector<muse::rcommand::Command>& ProjectRecoveryCommandsRegister::commandList() const
{
    static const std::vector<muse::rcommand::Command> commands = { PROJECT_RECOVERY_COMMAND };
    return commands;
}

const std::vector<muse::rcommand::CommandInfo>& ProjectRecoveryCommandsRegister::commandInfoList() const
{
    static const std::vector<muse::rcommand::CommandInfo> infos = {
        muse::rcommand::CommandInfo{
            PROJECT_RECOVERY_COMMAND,
            //: Command title: shown as a menu item or a button label; keep it short
            muse::TranslatableString("command", "&Project recovery…"),
            //: Command description: shown as a tooltip; can be a full sentence
            muse::TranslatableString("command_description", "Recover a corrupted project"),
            muse::rcommand::InputSchema(),
            muse::rcommand::Decoration()
        },
    };
    return infos;
}
