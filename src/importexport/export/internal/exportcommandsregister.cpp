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
    CommandInfo{
        EXPORT_MP3_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Export as MP3"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Export the project audio as an MP3 file into the macro-output folder"),
        InputSchema(),
        Decoration()
    },
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
