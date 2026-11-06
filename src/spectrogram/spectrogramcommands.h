/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/rcommand/typedcommand.h"

namespace au::spectrogram {
struct TrackSpectrogramSettingsCommand {
    static inline const muse::rcommand::Command id { "command://spectrogram/track-settings" };
    //: Command title: shown as a menu item or a button label; keep it short
    static inline const muse::TranslatableString title = muse::TranslatableString("command", "Spectrogram settings…");
    //: Command description: shown as a tooltip; can be a full sentence
    static inline const muse::TranslatableString description = muse::TranslatableString("command_description", "Spectrogram settings…");

    int trackId = -1;

    static constexpr auto input = std::tuple {
        muse::rcommand::Field("trackId", &TrackSpectrogramSettingsCommand::trackId, "Id of the track"),
    };
};
}
