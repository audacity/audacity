/*
* Audacity: A Digital Audio Editor
*/
#include "musehubcommandsregister.h"

#include "framework/ui/view/iconcodes.h"
#include "../musehubcommands.h"

using namespace au::musehub;
using namespace muse;
using namespace muse::rcommand;
using namespace muse::ui;

std::string MuseHubCommandsRegister::moduleName() const
{
    return "musehub";
}

const std::vector<Command>& MuseHubCommandsRegister::commandList() const
{
    static const std::vector<Command> commands = { GET_EFFECTS_COMMAND };
    return commands;
}

const std::vector<CommandInfo>& MuseHubCommandsRegister::commandInfoList() const
{
    static const std::vector<CommandInfo> infos = {
        CommandInfo{
            GET_EFFECTS_COMMAND,
            //: Command title: shown as a menu item or a button label; keep it short
            TranslatableString("command", "Get effects"),
            //: Command description: shown as a tooltip; can be a full sentence
            TranslatableString("command_description", "Open Get effects dialog"),
            InputSchema(),
            Decoration(IconCode::Code::PLUGIN)
        },
    };
    return infos;
}
