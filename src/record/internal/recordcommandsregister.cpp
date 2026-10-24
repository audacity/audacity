/*
* Audacity: A Digital Audio Editor
*/
#include "recordcommandsregister.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "../recordcommands.h"

using namespace au::record;
using namespace muse;
using namespace muse::rcommand;
using namespace muse::ui;

namespace {
const std::vector<CommandInfo> s_commandInfos = {
    CommandInfo{
        RECORD_START_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Record"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Record"),
        InputSchema(),
        Decoration(IconCode::Code::RECORD_FILL)
    },
    CommandInfo{
        RECORD_ON_CURRENT_TRACK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Record on current track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Record on current track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        RECORD_ON_NEW_TRACK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Record on new track"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Record on new track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        RECORD_PAUSE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Pause"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Pause"),
        InputSchema(),
        Decoration(IconCode::Code::PAUSE_FILL)
    },
    CommandInfo{
        RECORD_STOP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Stop"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Stop record"),
        InputSchema(),
        Decoration(IconCode::Code::STOP_FILL)
    },
    CommandInfo{
        RECORD_LEVEL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Record level"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Set record level"),
        InputSchema(),
        Decoration(IconCode::Code::MICROPHONE)
    },
    CommandInfo{
        RECORD_TOGGLE_MIC_METERING_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Show mic metering"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show mic metering"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        RECORD_TOGGLE_INPUT_MONITORING_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Turn on input monitoring"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Turn on input monitoring"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        RECORD_LEAD_IN_RECORDING_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Lead-in Recording"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Start lead-in recording"),
        InputSchema(),
        Decoration(IconCode::Code::RECORD_FILL)
    },
};
}

std::string RecordCommandsRegister::moduleName() const
{
    return "record";
}

const std::vector<Command>& RecordCommandsRegister::commandList() const
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

const std::vector<CommandInfo>& RecordCommandsRegister::commandInfoList() const
{
    return s_commandInfos;
}
