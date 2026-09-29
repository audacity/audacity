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

inline static const muse::rcommand::Command PROJECTSCENE_ZOOM_IN_COMMAND("command://projectscene/zoom-in");
inline static const muse::rcommand::Command PROJECTSCENE_ZOOM_OUT_COMMAND("command://projectscene/zoom-out");
inline static const muse::rcommand::Command PROJECTSCENE_ZOOM_DEFAULT_COMMAND("command://projectscene/zoom-default");
inline static const muse::rcommand::Command PROJECTSCENE_ZOOM_TO_SELECTION_COMMAND("command://projectscene/zoom-to-selection");
inline static const muse::rcommand::Command PROJECTSCENE_ZOOM_TO_FIT_PROJECT_COMMAND("command://projectscene/zoom-to-fit-project");
inline static const muse::rcommand::Command PROJECTSCENE_ZOOM_TOGGLE_COMMAND("command://projectscene/zoom-toggle");
inline static const muse::rcommand::Command PROJECTSCENE_CENTER_VIEW_ON_PLAYHEAD_COMMAND("command://projectscene/center-view-on-playhead");
inline static const muse::rcommand::Command PROJECTSCENE_TIMELINE_CONTEXT_MENU_COMMAND("command://projectscene/timeline-context-menu");

inline static const muse::rcommand::Command PROJECTSCENE_PLAY_POSITION_DECREASE_COMMAND("command://projectscene/play-position-decrease");
inline static const muse::rcommand::Command PROJECTSCENE_PLAY_POSITION_INCREASE_COMMAND("command://projectscene/play-position-increase");
inline static const muse::rcommand::Command PROJECTSCENE_SELECTION_EXTEND_LEFT_COMMAND("command://projectscene/selection-extend-left");
inline static const muse::rcommand::Command PROJECTSCENE_SELECTION_EXTEND_RIGHT_COMMAND("command://projectscene/selection-extend-right");
inline static const muse::rcommand::Command PROJECTSCENE_SELECTION_CONTRACT_LEFT_COMMAND("command://projectscene/selection-contract-left");
inline static const muse::rcommand::Command PROJECTSCENE_SELECTION_CONTRACT_RIGHT_COMMAND("command://projectscene/selection-contract-right");
inline static const muse::rcommand::Command PROJECTSCENE_CURSOR_TO_SELECTION_START_COMMAND(
    "command://projectscene/cursor-to-selection-start");
inline static const muse::rcommand::Command PROJECTSCENE_CURSOR_TO_SELECTION_END_COMMAND("command://projectscene/cursor-to-selection-end");
}
