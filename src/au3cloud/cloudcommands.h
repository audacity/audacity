/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/rcommand/typedcommand.h"

namespace au::au3cloud {
struct ShowTourPageCommand {
    static inline const muse::rcommand::Command id { "command://cloud/show-tour-page" };
    //: Command title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("command", "Show audio.com tour");
    //: Command description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("command_description",
                                                                                        "Open the audio.com tour page, signing in first if needed");
};

struct OpenProjectPageCommand {
    static inline const muse::rcommand::Command id { "command://cloud/open-project-page" };
    //: Command title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("command", "View project on audio.com");
    //: Command description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("command_description", "View project on audio.com");

    std::string projectId;

    static constexpr auto input = std::tuple {
        muse::rcommand::Field("id", &OpenProjectPageCommand::projectId, "Id of the cloud project"),
    };
};

struct OpenAudioPageCommand {
    static inline const muse::rcommand::Command id { "command://cloud/open-audio-page" };
    //: Command title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("command", "View on audio.com");
    //: Command description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("command_description", "View on audio.com");

    std::string slug;

    static constexpr auto input = std::tuple {
        muse::rcommand::Field("slug", &OpenAudioPageCommand::slug, "Slug of the audio"),
    };
};

struct OpenProfilePageCommand {
    static inline const muse::rcommand::Command id { "command://cloud/open-profile-page" };
    //: Command title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("command", "View profile on audio.com");
    //: Command description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("command_description",
                                                                                        "Open the signed in user’s audio.com profile page");
};

struct OpenUrlCommand {
    static inline const muse::rcommand::Command id { "command://cloud/open-url" };
    //: Command title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("command", "Open audacity URL");
    //: Command description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("command_description", "Handle an audacity:// URL");

    std::string url;

    static constexpr auto input = std::tuple {
        muse::rcommand::Field("url", &OpenUrlCommand::url, "The URL to handle"),
    };
};
}
