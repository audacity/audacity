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
                { INSTALLER_PATH_PARAM, Arg(DataType::String, u"Path of an update package to apply after all windows are closed") },
            }),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_RESTART_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Restart"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Restart"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_TOGGLE_FULLSCREEN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Full screen"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Full screen"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::UI
    },
    CommandInfo{
        APP_ABOUT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&About Audacity…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "About Audacity"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_ABOUT_QT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "About &Qt…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "About Qt"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_ONLINE_HANDBOOK_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Online &handbook"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open online handbook"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_ASK_HELP_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "As&k for help"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Ask for help"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_PREFERENCES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Preferences"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Preferences…"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_REVERT_FACTORY_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Revert to &factory settings"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Revert to factory settings"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_AUDIO_SETTINGS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Audio settings"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open audio setup dialog"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_SHORTCUTS_PREFERENCES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Shortcuts"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open shortcuts preferences"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_EDITING_PREFERENCES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Editing"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open editing preferences"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_SPECTROGRAM_PREFERENCES_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Spectrogram"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open spectrogram preferences"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_CONTEXT_MENU_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Open item context menu"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open the context menu of the focused item"),
        InputSchema(),
        Decoration(),
        Availability::UI
    },
    CommandInfo{
        APP_TOGGLE_TRANSPORT_TOOLBAR_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Playback controls"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show/hide playback controls"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::UI
    },
    CommandInfo{
        APP_TOGGLE_TRACKS_PANEL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Tracks"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show/hide tracks"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::UI
    },
    CommandInfo{
        APP_TOGGLE_HISTORY_PANEL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Show history"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show history"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::UI
    },
    CommandInfo{
        APP_TOGGLE_MACRO_MANAGER_PANEL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Macro manager"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show/hide macro manager"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        APP_TOGGLE_STATUSBAR_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Status bar"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show/hide status bar"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes),
        Availability::UI
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
