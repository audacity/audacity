/*
* Audacity: A Digital Audio Editor
*/
#include "appshellcommandsregister.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "../appshellcommands.h"

using namespace au::appshell;
using namespace muse;
using namespace muse::rcommand;
using namespace muse::ui;

namespace {
const std::vector<CommandInfo> s_commandInfos = {
    CommandInfo{
        APP_QUIT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Exit"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Exit"),
        InputSchema({
                { "installer_path", Arg(DataType::String, u"Path of an update package to apply after all windows are closed") },
            }),
        Decoration()
    },
    CommandInfo{
        APP_RESTART_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Restart"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Restart"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_TOGGLE_FULLSCREEN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Full screen"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Full screen"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        APP_ABOUT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&About Audacity…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "About Audacity"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_ABOUT_QT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "About &Qt…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "About Qt"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_ONLINE_HANDBOOK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Online &handbook"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open online handbook"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_ASK_HELP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "As&k for help"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Ask for help"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_PREFERENCES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Preferences"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Preferences…"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_REVERT_FACTORY_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Revert to &factory settings"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Revert to factory settings"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_AUDIO_SETTINGS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Audio settings"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open audio setup dialog"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_SHORTCUTS_PREFERENCES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Shortcuts"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open shortcuts preferences"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_EDITING_PREFERENCES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Editing"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open editing preferences"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_SPECTROGRAM_PREFERENCES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Spectrogram"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open spectrogram preferences"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_COPY_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Copy"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Copy"),
        InputSchema(),
        Decoration(IconCode::Code::COPY)
    },
    CommandInfo{
        APP_CUT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Cut"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Cut"),
        InputSchema(),
        Decoration(IconCode::Code::CUT)
    },
    CommandInfo{
        APP_PASTE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Paste"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Paste"),
        InputSchema(),
        Decoration(IconCode::Code::PASTE)
    },
    CommandInfo{
        APP_UNDO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Undo"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Undo"),
        InputSchema(),
        Decoration(IconCode::Code::UNDO)
    },
    CommandInfo{
        APP_REDO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Redo"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Redo"),
        InputSchema(),
        Decoration(IconCode::Code::REDO)
    },
    CommandInfo{
        APP_DELETE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "De&lete"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Delete"),
        InputSchema(),
        Decoration(IconCode::Code::DELETE_TANK)
    },
    CommandInfo{
        APP_CANCEL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Cancel"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Cancel"),
        InputSchema(),
        Decoration(IconCode::Code::DELETE_TANK)
    },
    CommandInfo{
        APP_TRIGGER_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Trigger"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Trigger"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_ENTER_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Enter"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Trigger the focused control or select the focused track item"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_SHIFT_ENTER_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Shift+&Enter"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Trigger the focused control or make a range selection of track items"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_CONTEXT_MENU_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Open item context menu"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open the context menu of the focused item"),
        InputSchema(),
        Decoration()
    },
};
}

std::string AppShellCommandsRegister::moduleName() const
{
    return "appshell";
}

const std::vector<Command>& AppShellCommandsRegister::commandList() const
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

const std::vector<CommandInfo>& AppShellCommandsRegister::commandInfoList() const
{
    return s_commandInfos;
}
