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
        //: Action title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("action", "Show audio.com tour"),
        //: Action description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("action_description", "Open the audio.com tour page, signing in first if needed"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        CLOUD_OPEN_PROJECT_PAGE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("action", "View project on audio.com"),
        //: Action description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("action_description", "View project on audio.com"),
        InputSchema({
                { "id", Arg(DataType::String, u"Id of the cloud project") },
            }),
        Decoration()
    },
    CommandInfo{
        CLOUD_OPEN_AUDIO_PAGE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("action", "View on audio.com"),
        //: Action description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("action_description", "View on audio.com"),
        InputSchema({
                { "slug", Arg(DataType::String, u"Slug of the audio") },
            }),
        Decoration()
    },
    CommandInfo{
        CLOUD_OPEN_PROFILE_PAGE_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("action", "View profile on audio.com"),
        //: Action description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("action_description", "Open the signed in user's audio.com profile page"),
        InputSchema(),
        Decoration()
    },
    CommandInfo{
        CLOUD_OPEN_URL_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("action", "Open audacity URL"),
        //: Action description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("action_description", "Handle an audacity:// URL"),
        InputSchema({
                { "url", Arg(DataType::String, u"The URL to handle") },
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
