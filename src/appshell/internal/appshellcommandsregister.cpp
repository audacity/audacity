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
        GLOBAL_QUIT_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Exit"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Exit"),
        InputSchema({
                { "installer_path", Arg(DataType::String, u"Path of an update package to apply after all windows are closed") },
            }),
        Decoration()
    },
    CommandInfo{
        GLOBAL_RESTART_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Restart"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Restart"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_TOGGLE_FULLSCREEN_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Full screen"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Full screen"),
        InputSchema(),
        Decoration(rcommand::Checkable::Yes)
    },
    CommandInfo{
        APP_ABOUT_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&About Audacity…"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "About Audacity"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_ABOUT_QT_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "About &Qt…"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "About Qt"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_ONLINE_HANDBOOK_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Online &handbook"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Open online handbook"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_ASK_HELP_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "As&k for help"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Ask for help"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_PREFERENCES_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Preferences"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Preferences…"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_REVERT_FACTORY_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Revert to &factory settings"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Revert to factory settings"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_AUDIO_SETTINGS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Audio settings"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Open audio setup dialog"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_SHORTCUTS_PREFERENCES_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Shortcuts"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Open shortcuts preferences"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_EDITING_PREFERENCES_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Editing"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Open editing preferences"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        APP_SPECTROGRAM_PREFERENCES_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Spectrogram"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Open spectrogram preferences"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        GLOBAL_COPY_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Copy"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Copy"),
        InputSchema(),
        Decoration(IconCode::Code::COPY)
    },
    CommandInfo{
        GLOBAL_CUT_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Cut"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Cut"),
        InputSchema(),
        Decoration(IconCode::Code::CUT)
    },
    CommandInfo{
        GLOBAL_PASTE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Paste"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Paste"),
        InputSchema(),
        Decoration(IconCode::Code::PASTE)
    },
    CommandInfo{
        GLOBAL_UNDO_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Undo"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Undo"),
        InputSchema(),
        Decoration(IconCode::Code::UNDO)
    },
    CommandInfo{
        GLOBAL_REDO_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Redo"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Redo"),
        InputSchema(),
        Decoration(IconCode::Code::REDO)
    },
    CommandInfo{
        GLOBAL_DELETE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "De&lete"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Delete"),
        InputSchema(),
        Decoration(IconCode::Code::DELETE_TANK)
    },
    CommandInfo{
        GLOBAL_CANCEL_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Cancel"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Cancel"),
        InputSchema(),
        Decoration(IconCode::Code::DELETE_TANK)
    },
    CommandInfo{
        GLOBAL_TRIGGER_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Trigger"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Trigger"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        GLOBAL_ENTER_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "&Enter"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Trigger the focused control or select the focused track item"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        GLOBAL_SHIFT_ENTER_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Shift+&Enter"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Trigger the focused control or make a range selection of track items"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        GLOBAL_CONTEXT_MENU_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        TranslatableString("action", "Open item context menu"),
        //: Action description: shown as a tooltip; can be a full sentence
        TranslatableString("action_description", "Open the context menu of the focused item"),
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
