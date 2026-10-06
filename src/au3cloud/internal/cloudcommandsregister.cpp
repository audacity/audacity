/*
* Audacity: A Digital Audio Editor
*/
#include "cloudcommandsregister.h"

#include "framework/global/types/translatablestring.h"

#include "../cloudcommands.h"

using namespace au::au3cloud;
using namespace muse::rcommand;

namespace {
const std::vector<CommandInfo> s_commandInfos = {
    CommandInfo{
        CLOUD_SHOW_TOUR_PAGE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("command", "Show audio.com tour"),
        //: Command description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("command_description", "Open the audio.com tour page, signing in first if needed"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        CLOUD_OPEN_PROJECT_PAGE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("command", "View project on audio.com"),
        //: Command description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("command_description", "View project on audio.com"),
        InputSchema({
                { CLOUD_OPEN_PROJECT_PAGE_ID_PARAM, Arg(DataType::String, u"Id of the cloud project") },
            }),
        Decoration()
    },
    CommandInfo{
        CLOUD_OPEN_AUDIO_PAGE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("command", "View on audio.com"),
        //: Command description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("command_description", "View on audio.com"),
        InputSchema({
                { CLOUD_OPEN_AUDIO_PAGE_SLUG_PARAM, Arg(DataType::String, u"Slug of the audio") },
            }),
        Decoration()
    },
    CommandInfo{
        CLOUD_OPEN_PROFILE_PAGE_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("command", "View profile on audio.com"),
        //: Command description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("command_description", "Open the signed in user's audio.com profile page"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        CLOUD_OPEN_URL_COMMAND,
        //: Command title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("command", "Open audacity URL"),
        //: Command description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("command_description", "Handle an audacity:// URL"),
        InputSchema({
                { CLOUD_OPEN_URL_URL_PARAM, Arg(DataType::String, u"The URL to handle") },
            }),
        Decoration()
    },
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
