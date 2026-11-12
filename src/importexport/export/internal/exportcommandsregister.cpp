/*
* Audacity: A Digital Audio Editor
*/
#include "exportcommandsregister.h"

#include "framework/global/types/translatablestring.h"

#include "../exportcommands.h"

using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::vector<CommandInfo> s_commandInfos = {
    makeCommandInfo<ExportAudioCommand>(),
    makeCommandInfo<ExportWavCommand>(),
    makeCommandInfo<ExportMp3PresetCommand>(),
    makeCommandInfo<ExportMp3VbrCommand>(),
    makeCommandInfo<ExportMp3AbrCommand>(),
    makeCommandInfo<ExportMp3CbrCommand>(),
    makeCommandInfo<ExportFlacCommand>(),
    makeCommandInfo<ExportOggCommand>(),
};

std::vector<Command> makeCommandList(const std::vector<CommandInfo>& infos)
{
    std::vector<Command> commands;
    commands.reserve(infos.size());
    for (const CommandInfo& info : infos) {
        commands.push_back(info.command);
    }
    return commands;
}

const std::vector<Command> s_commands = makeCommandList(s_commandInfos);
}

std::string ExportCommandsRegister::moduleName() const
{
    return "exporter";
}

const std::vector<Command>& ExportCommandsRegister::commandList() const
{
    return s_commands;
}

const std::vector<CommandInfo>& ExportCommandsRegister::commandInfoList() const
{
    return s_commandInfos;
}
