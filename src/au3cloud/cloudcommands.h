/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/rcommand/typedcommand.h"

namespace au::au3cloud {
struct ShowTourPageCommand {
    static inline const muse::rcommand::Command id { "command://cloud/show-tour-page" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "Show audio.com tour");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description",
                                                                                        "Open the audio.com tour page, signing in first if needed");

    static constexpr auto fields() { return std::tuple {}; }
};

struct OpenProjectPageCommand {
    static inline const muse::rcommand::Command id { "command://cloud/open-project-page" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "View project on audio.com");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "View project on audio.com");

    std::string projectId;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "id", &OpenProjectPageCommand::projectId, u"Id of the cloud project" },
        };
    }
};

struct OpenAudioPageCommand {
    static inline const muse::rcommand::Command id { "command://cloud/open-audio-page" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "View on audio.com");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "View on audio.com");

    std::string slug;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "slug", &OpenAudioPageCommand::slug, u"Slug of the audio" },
        };
    }
};

struct OpenProfilePageCommand {
    static inline const muse::rcommand::Command id { "command://cloud/open-profile-page" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "View profile on audio.com");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description",
                                                                                        "Open the signed in user’s audio.com profile page");

    static constexpr auto fields() { return std::tuple {}; }
};

struct OpenUrlCommand {
    static inline const muse::rcommand::Command id { "command://cloud/open-url" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "Open audacity URL");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Handle an audacity:// URL");

    std::string url;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "url", &OpenUrlCommand::url, u"The URL to handle" },
        };
    }
};
}
