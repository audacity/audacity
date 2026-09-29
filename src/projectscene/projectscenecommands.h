/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/rcommand/commandtypes.h"

namespace au::projectscene {
inline static const muse::rcommand::Command PROJECTSCENE_MINUTES_SECONDS_RULER_COMMAND("command://projectscene/minutes-seconds-ruler");
inline static const muse::rcommand::Command PROJECTSCENE_BEATS_MEASURES_RULER_COMMAND("command://projectscene/beats-measures-ruler");
inline static const muse::rcommand::Command PROJECTSCENE_TOGGLE_VERTICAL_RULERS_COMMAND("command://projectscene/toggle-vertical-rulers");
inline static const muse::rcommand::Command PROJECTSCENE_TOGGLE_RMS_IN_WAVEFORM_COMMAND("command://projectscene/toggle-rms-in-waveform");
inline static const muse::rcommand::Command PROJECTSCENE_TOGGLE_CLIPPING_IN_WAVEFORM_COMMAND(
    "command://projectscene/toggle-clipping-in-waveform");
inline static const muse::rcommand::Command PROJECTSCENE_TOGGLE_UPDATE_DISPLAY_WHILE_PLAYING_COMMAND(
    "command://projectscene/toggle-update-display-while-playing");
inline static const muse::rcommand::Command PROJECTSCENE_TOGGLE_PINNED_PLAY_HEAD_COMMAND("command://projectscene/toggle-pinned-play-head");
inline static const muse::rcommand::Command PROJECTSCENE_TOGGLE_PLAYBACK_ON_RULER_CLICK_COMMAND(
    "command://projectscene/toggle-playback-on-ruler-click");
inline static const muse::rcommand::Command PROJECTSCENE_TOGGLE_TRACK_HALF_WAVE_COMMAND("command://projectscene/toggle-track-half-wave");
inline static const muse::rcommand::Command PROJECTSCENE_TOGGLE_CLIP_GAIN_AUTOMATION_COMMAND(
    "command://projectscene/toggle-clip-gain-automation");
inline static const muse::rcommand::Command PROJECTSCENE_CLIP_PITCH_AND_SPEED_COMMAND("command://projectscene/clip-pitch-and-speed");
inline static const muse::rcommand::Command PROJECTSCENE_OPEN_LABEL_EDITOR_COMMAND("command://projectscene/open-label-editor");
}
