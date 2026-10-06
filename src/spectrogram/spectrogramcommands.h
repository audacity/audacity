/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/rcommand/commandtypes.h"

namespace au::spectrogram {
inline static const muse::rcommand::Command TRACK_SPECTROGRAM_SETTINGS_COMMAND("command://spectrogram/track-settings");
inline static const std::string TRACK_SPECTROGRAM_SETTINGS_TRACK_ID_PARAM("trackId");
}
