/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/rcommand/typedcommand.h"

namespace au::spectrogram {
struct TrackSpectrogramSettingsCommand {
    static inline const muse::rcommand::Command id { "command://spectrogram/track-settings" };
    //: Action title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("action", "Spectrogram settings…");
    //: Action description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("action_description", "Spectrogram settings…");

    int trackId = -1;

    static constexpr auto fields()
    {
        using muse::rcommand::Field;
        return std::tuple {
            Field { "trackId", &TrackSpectrogramSettingsCommand::trackId, u"Id of the track" },
        };
    }
};
}
