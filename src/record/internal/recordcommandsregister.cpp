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
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Record"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Record"),
        InputSchema(),
        Decoration(IconCode::Code::RECORD_FILL)
    },
    CommandInfo{
        RECORD_ON_CURRENT_TRACK_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Record on current track"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Record on current track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        RECORD_ON_NEW_TRACK_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Record on new track"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Record on new track"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        RECORD_PAUSE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Pause"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Pause"),
        InputSchema(),
        Decoration(IconCode::Code::PAUSE_FILL)
    },
    CommandInfo{
        RECORD_STOP_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Stop"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Stop record"),
        InputSchema(),
        Decoration(IconCode::Code::STOP_FILL)
    },
    CommandInfo{
        RECORD_LEVEL_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Record level"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Set record level"),
        InputSchema(),
        Decoration(IconCode::Code::MICROPHONE)
    },
    CommandInfo{
        RECORD_TOGGLE_MIC_METERING_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Show mic metering"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Show mic metering"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        RECORD_TOGGLE_INPUT_MONITORING_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Turn on input monitoring"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Turn on input monitoring"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        RECORD_LEAD_IN_RECORDING_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Lead-in Recording"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Start lead-in recording"),
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
