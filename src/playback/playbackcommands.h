/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/rcommand/commandtypes.h"

namespace au::playback {
inline static const muse::rcommand::Command PLAYBACK_TOGGLE_PLAY_PAUSE_COMMAND("command://playback/toggle-play-pause");
inline static const muse::rcommand::Command PLAYBACK_TOGGLE_PLAY_STOP_COMMAND("command://playback/toggle-play-stop");
inline static const muse::rcommand::Command PLAYBACK_TOGGLE_PLAY_STOP_AND_SET_CURSOR_COMMAND(
    "command://playback/toggle-play-stop-and-set-cursor");
inline static const muse::rcommand::Command PLAYBACK_PLAY_SELECTION_COMMAND("command://playback/play-selection");
inline static const muse::rcommand::Command PLAYBACK_PLAY_TRACKS_COMMAND("command://playback/play-tracks");
inline static const muse::rcommand::Command PLAYBACK_PAUSE_COMMAND("command://playback/pause");
inline static const muse::rcommand::Command PLAYBACK_STOP_COMMAND("command://playback/stop");
inline static const muse::rcommand::Command PLAYBACK_REWIND_START_COMMAND("command://playback/rewind-start");
inline static const muse::rcommand::Command PLAYBACK_REWIND_END_COMMAND("command://playback/rewind-end");
inline static const muse::rcommand::Command PLAYBACK_SEEK_COMMAND("command://playback/seek");
inline static const std::string PLAYBACK_SEEK_TIME_PARAM("seekTime");
inline static const std::string PLAYBACK_SEEK_TRIGGER_PLAY_PARAM("triggerPlay");
inline static const muse::rcommand::Command PLAYBACK_CHANGE_PLAY_REGION_COMMAND("command://playback/play-region-change");
inline static const std::string PLAYBACK_CHANGE_PLAY_REGION_START_PARAM("start");
inline static const std::string PLAYBACK_CHANGE_PLAY_REGION_END_PARAM("end");

inline static const muse::rcommand::Command PLAYBACK_CHANGE_AUDIO_API_COMMAND("command://playback/change-api");
inline static const std::string PLAYBACK_CHANGE_AUDIO_API_INDEX_PARAM("api_index");
inline static const muse::rcommand::Command PLAYBACK_CHANGE_PLAYBACK_DEVICE_COMMAND("command://playback/change-playback-device");
inline static const std::string PLAYBACK_CHANGE_PLAYBACK_DEVICE_INDEX_PARAM("device_index");
inline static const std::string PLAYBACK_CHANGE_PLAYBACK_DEVICE_IS_DEFAULT_PARAM("is_default_device");
inline static const muse::rcommand::Command PLAYBACK_CHANGE_RECORDING_DEVICE_COMMAND("command://playback/change-recording-device");
inline static const std::string PLAYBACK_CHANGE_RECORDING_DEVICE_INDEX_PARAM("device_index");
inline static const std::string PLAYBACK_CHANGE_RECORDING_DEVICE_IS_DEFAULT_PARAM("is_default_device");
inline static const muse::rcommand::Command PLAYBACK_CHANGE_INPUT_CHANNELS_COMMAND("command://playback/change-input-channels");
inline static const muse::rcommand::Command PLAYBACK_RESCAN_DEVICES_COMMAND("command://playback/rescan-devices");

inline static const muse::rcommand::Command PLAYBACK_TOGGLE_PLAY_REPEATS_COMMAND("command://playback/toggle-play-repeats");
inline static const muse::rcommand::Command PLAYBACK_TOGGLE_AUTOMATIC_PAN_COMMAND("command://playback/toggle-automatic-pan");

inline static const muse::rcommand::Command PLAYBACK_TOGGLE_LOOP_REGION_COMMAND("command://playback/toggle-loop-region");
inline static const muse::rcommand::Command PLAYBACK_CLEAR_LOOP_REGION_COMMAND("command://playback/clear-loop-region");
inline static const muse::rcommand::Command PLAYBACK_SET_LOOP_REGION_TO_SELECTION_COMMAND(
    "command://playback/set-loop-region-to-selection");
inline static const muse::rcommand::Command PLAYBACK_SET_SELECTION_TO_LOOP_COMMAND("command://playback/set-selection-to-loop");
inline static const muse::rcommand::Command PLAYBACK_SET_LOOP_REGION_IN_OUT_COMMAND("command://playback/set-loop-region-in-out");
inline static const muse::rcommand::Command PLAYBACK_TOGGLE_SELECTION_FOLLOWS_LOOP_REGION_COMMAND(
    "command://playback/toggle-selection-follows-loop-region");

inline static const muse::rcommand::Command PLAYBACK_TOGGLE_MUTE_FOCUSED_TRACK_COMMAND("command://playback/toggle-mute-focused-track");
inline static const muse::rcommand::Command PLAYBACK_TOGGLE_SOLO_FOCUSED_TRACK_COMMAND("command://playback/toggle-solo-focused-track");
inline static const muse::rcommand::Command PLAYBACK_MUTE_ALL_TRACKS_COMMAND("command://playback/mute-all-tracks");
inline static const muse::rcommand::Command PLAYBACK_UNMUTE_ALL_TRACKS_COMMAND("command://playback/unmute-all-tracks");
inline static const muse::rcommand::Command PLAYBACK_MUTE_SELECTED_TRACKS_COMMAND("command://playback/mute-selected-tracks");
inline static const muse::rcommand::Command PLAYBACK_UNMUTE_SELECTED_TRACKS_COMMAND("command://playback/unmute-selected-tracks");

inline static const muse::rcommand::Command PLAYBACK_LEVEL_COMMAND("command://playback/level");
inline static const muse::rcommand::Command PLAYBACK_METRONOME_COMMAND("command://playback/metronome");
inline static const muse::rcommand::Command PLAYBACK_TIME_COMMAND("command://playback/time");
inline static const muse::rcommand::Command PLAYBACK_BPM_COMMAND("command://playback/bpm");
inline static const muse::rcommand::Command PLAYBACK_TIME_SIGNATURE_COMMAND("command://playback/time-signature");
}
