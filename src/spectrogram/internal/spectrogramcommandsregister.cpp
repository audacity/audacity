/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramcommandsregister.h"

#include "../spectrogramcommands.h"

namespace au::spectrogram {
namespace {
using muse::rcommand::CommandInfo;
using muse::rcommand::makeCommandInfo;

const std::vector<CommandInfo> s_commandInfos = {
    makeCommandInfo<TrackSpectrogramSettingsCommand>(),
};
}

std::string SpectrogramCommandsRegister::moduleName() const
{
    return "spectrogram";
}

const std::vector<muse::rcommand::Command>& SpectrogramCommandsRegister::commandList() const
{
    static std::vector<muse::rcommand::Command> commands;
    if (commands.empty()) {
        commands.reserve(s_commandInfos.size());
        for (const auto& info : s_commandInfos) {
            commands.push_back(info.command);
        }
    }
    return commands;
}

const std::vector<CommandInfo>& SpectrogramCommandsRegister::commandInfoList() const
{
    return s_commandInfos;
}
}
