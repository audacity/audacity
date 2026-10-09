/*
* Audacity: A Digital Audio Editor
*/
#include "projectcommandsregister.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "../projectcommands.h"

using namespace au::project;
using namespace muse;
using namespace muse::rcommand;
using namespace muse::ui;

namespace {
const std::vector<CommandInfo> commandInfos = {
    CommandInfo{
        PROJECT_NEW_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&New…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "New…"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_OPEN_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Open…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open…"),
        InputSchema({
                { PROJECT_URL_PARAM, Arg(DataType::String, u"URL of the project or media file to open; asks for a file when omitted") },
                { PROJECT_DISPLAY_NAME_PARAM, Arg(DataType::String, u"Display name override for the opened project") },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECT_OPEN_CLOUD_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Open"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open cloud project"),
        InputSchema({
                { PROJECT_ID_PARAM, Arg(DataType::String, u"Cloud project id") },
                { PROJECT_SNAPSHOT_ID_PARAM, Arg(DataType::String, u"Cloud project snapshot id") },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECT_CLEAR_RECENT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Clear recent files"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Clear recent files"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_IMPORT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Import…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Import…"),
        InputSchema({
                { PROJECT_FILES_PARAM, Arg(DataType::Array, u"Paths of the media files to import; asks for files when omitted") },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECT_IMPORT_STARTUP_MEDIA_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Import startup media"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Import media files into a new project"),
        InputSchema({
                { PROJECT_FILES_PARAM, Arg(DataType::Array, u"Paths of the media files to import") },
                { PROJECT_REMOVE_AFTER_IMPORT_PARAM, Arg(DataType::Boolean, u"Remove the files after importing them") },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECT_SAVE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Save"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Save"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_SAVE_AS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Save &as…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Save as…"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_SAVE_TO_CLOUD_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Save to clo&ud…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Save to cloud…"),
        InputSchema(),
        Decoration(IconCode::Code::CLOUD_FILE)
    },
    CommandInfo{
        PROJECT_CLOSE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Close project"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Close project"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_SHARE_AUDIO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Share audio"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Share audio"),
        InputSchema(),
        Decoration(IconCode::Code::SHARE_AUDIO)
    },
    CommandInfo{
        PROJECT_OPEN_CLOUD_AUDIO_FILE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Open"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open cloud audio file"),
        InputSchema({
                { PROJECT_AUDIO_ID_PARAM, Arg(DataType::String, u"Cloud audio file id") },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Update cloud audio preview"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Update cloud audio preview"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_UPDATE_CLOUD_AUDIO_PREVIEW_FOR_PROJECT_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Update audio preview"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Update audio preview"),
        InputSchema({
                { PROJECT_ID_PARAM, Arg(DataType::String, u"Cloud project id") },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECT_EXPORT_AUDIO_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Export audio…"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Export audio…"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_EXPORT_LABELS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Export labels"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Export labels"),
        InputSchema({
                { PROJECT_TRACK_ID_PARAM, Arg(DataType::Integer, u"Label track id; exports all label tracks when omitted") },
            }),
        Decoration()
    },
    CommandInfo{
        PROJECT_EXPORT_MIDI_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "&Export MIDI"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Export MIDI"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_OPEN_METADATA_DIALOG_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Show metadata editor"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Show metadata editor"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_OPEN_CUSTOM_FFMPEG_OPTIONS_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Custom FFmpeg options"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open the custom FFmpeg export options"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        PROJECT_OPEN_CUSTOM_MAPPING_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        TranslatableString("command", "Custom channel mapping"),
        //: Command description: shown as a tooltip; can be a full sentence
        TranslatableString("command_description", "Open the custom export channel mapping"),
        InputSchema(),
        Decoration()
    },
};
}

std::string ProjectCommandsRegister::moduleName() const
{
    return "project";
}

const std::vector<Command>& ProjectCommandsRegister::commandList() const
{
    static std::vector<Command> commands;
    if (commands.empty()) {
        commands.reserve(commandInfos.size());
        for (const auto& info : commandInfos) {
            commands.push_back(info.command);
        }
    }
    return commands;
}

const std::vector<CommandInfo>& ProjectCommandsRegister::commandInfoList() const
{
    return commandInfos;
}
