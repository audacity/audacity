/*
* Audacity: A Digital Audio Editor
*/
#include "cloudcommandsregister.h"

#include "../cloudcommands.h"

using namespace au::au3cloud;
using namespace muse::rcommand;

namespace {
const std::vector<CommandInfo> s_commandInfos = {
    makeCommandInfo<ShowTourPageCommand>(),
    makeCommandInfo<OpenProjectPageCommand>(),
    makeCommandInfo<OpenAudioPageCommand>(),
    makeCommandInfo<OpenProfilePageCommand>(),
    makeCommandInfo<OpenUrlCommand>(),
};
}

std::string CloudCommandsRegister::moduleName() const
{
    return "au3cloud";
}

const std::vector<Command>& CloudCommandsRegister::commandList() const
{
    static std::vector<Command> commands;
    if (commands.empty()) {
        commands.reserve(s_commandInfos.size());
        for (const auto& info : s_commandInfos) {
            commands.push_back(info.command);
        }
    }
    return commands;
}

const std::vector<CommandInfo>& CloudCommandsRegister::commandInfoList() const
{
    return s_commandInfos;
}
