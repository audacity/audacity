/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/rcommand/commandtypes.h"

namespace au::trackedit {
inline static const muse::rcommand::Command TRACKEDIT_UNDO_COMMAND("command://trackedit/undo");
inline static const muse::rcommand::Command TRACKEDIT_REDO_COMMAND("command://trackedit/redo");
inline static const muse::rcommand::Command TRACKEDIT_CANCEL_COMMAND("command://trackedit/cancel");

inline static const muse::rcommand::Command TRACKEDIT_PASTE_OVERLAP_COMMAND("command://trackedit/paste-overlap");
inline static const muse::rcommand::Command TRACKEDIT_PASTE_INSERT_COMMAND("command://trackedit/paste-insert");
inline static const muse::rcommand::Command TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND(
    "command://trackedit/paste-insert-all-tracks-ripple");

inline static const muse::rcommand::Command TRACKEDIT_CLIP_CUT_COMMAND("command://trackedit/clip-cut");
inline static const muse::rcommand::Command TRACKEDIT_CLIP_COPY_COMMAND("command://trackedit/clip-copy");
inline static const muse::rcommand::Command TRACKEDIT_CLIP_DELETE_COMMAND("command://trackedit/clip-delete");
inline static const muse::rcommand::Command TRACKEDIT_CLIP_SPLIT_CUT_COMMAND("command://trackedit/clip-split-cut");
inline static const muse::rcommand::Command TRACKEDIT_CLIP_SPLIT_DELETE_COMMAND("command://trackedit/clip-split-delete");
inline static const muse::rcommand::Command TRACKEDIT_CLIP_PITCH_SPEED_OPEN_COMMAND("command://trackedit/clip-pitch-speed-open");
inline static const muse::rcommand::Command TRACKEDIT_CLIP_RENDER_PITCH_SPEED_COMMAND("command://trackedit/clip-render-pitch-speed");
inline static const muse::rcommand::Command TRACKEDIT_CLIP_RESET_PITCH_SPEED_COMMAND("command://trackedit/clip-reset-pitch-speed");
inline static const muse::rcommand::Command TRACKEDIT_STRETCH_CLIP_TO_MATCH_TEMPO_COMMAND(
    "command://trackedit/stretch-clip-to-match-tempo");

inline static const muse::rcommand::Command TRACKEDIT_TRACK_SPLIT_AT_COMMAND("command://trackedit/track-split-at");

inline static const muse::rcommand::Command TRACKEDIT_NEW_MONO_TRACK_COMMAND("command://trackedit/new-mono-track");
inline static const muse::rcommand::Command TRACKEDIT_NEW_STEREO_TRACK_COMMAND("command://trackedit/new-stereo-track");
inline static const muse::rcommand::Command TRACKEDIT_NEW_LABEL_TRACK_COMMAND("command://trackedit/new-label-track");

inline static const muse::rcommand::Command TRACKEDIT_TRACK_DUPLICATE_COMMAND("command://trackedit/track-duplicate");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_MOVE_UP_COMMAND("command://trackedit/track-move-up");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_MOVE_DOWN_COMMAND("command://trackedit/track-move-down");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_MOVE_TOP_COMMAND("command://trackedit/track-move-top");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_MOVE_BOTTOM_COMMAND("command://trackedit/track-move-bottom");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_SWAP_CHANNELS_COMMAND("command://trackedit/track-swap-channels");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_SPLIT_STEREO_TO_LR_COMMAND("command://trackedit/track-split-stereo-to-lr");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_SPLIT_STEREO_TO_CENTER_COMMAND(
    "command://trackedit/track-split-stereo-to-center");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_CHANGE_RATE_CUSTOM_COMMAND("command://trackedit/track-change-rate-custom");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_MAKE_STEREO_COMMAND("command://trackedit/track-make-stereo");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_RESAMPLE_COMMAND("command://trackedit/track-resample");

inline static const muse::rcommand::Command TRACKEDIT_TRIM_AUDIO_OUTSIDE_SELECTION_COMMAND(
    "command://trackedit/trim-audio-outside-selection");
inline static const muse::rcommand::Command TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND("command://trackedit/silence-audio-selection");

inline static const muse::rcommand::Command TRACKEDIT_GROUP_CLIPS_COMMAND("command://trackedit/group-clips");
inline static const muse::rcommand::Command TRACKEDIT_UNGROUP_CLIPS_COMMAND("command://trackedit/ungroup-clips");

inline static const muse::rcommand::Command TRACKEDIT_SELECT_ALL_COMMAND("command://trackedit/select-all");
inline static const muse::rcommand::Command TRACKEDIT_CLEAR_SELECTION_COMMAND("command://trackedit/clear-selection");
inline static const muse::rcommand::Command TRACKEDIT_SELECT_ALL_TRACKS_COMMAND("command://trackedit/select-all-tracks");
inline static const muse::rcommand::Command TRACKEDIT_SELECT_LEFT_OF_PLAYBACK_POSITION_COMMAND(
    "command://trackedit/select-left-of-playback-position");
inline static const muse::rcommand::Command TRACKEDIT_SELECT_RIGHT_OF_PLAYBACK_POSITION_COMMAND(
    "command://trackedit/select-right-of-playback-position");
inline static const muse::rcommand::Command TRACKEDIT_SELECT_TRACK_START_TO_CURSOR_COMMAND(
    "command://trackedit/select-track-start-to-cursor");
inline static const muse::rcommand::Command TRACKEDIT_SELECT_CURSOR_TO_TRACK_END_COMMAND(
    "command://trackedit/select-cursor-to-track-end");
inline static const muse::rcommand::Command TRACKEDIT_SELECT_TRACK_START_TO_END_COMMAND(
    "command://trackedit/select-track-start-to-end");
inline static const muse::rcommand::Command TRACKEDIT_SET_SELECTION_COMMAND("command://trackedit/set-selection");
inline static const muse::rcommand::Command TRACKEDIT_SELECT_TRACK_COMMAND("command://trackedit/select-track");
inline static const muse::rcommand::Command TRACKEDIT_ZERO_CROSS_COMMAND("command://trackedit/zero-cross");

inline static const muse::rcommand::Command TRACKEDIT_CLIP_CHANGE_COLOR_AUTO_COMMAND("command://trackedit/clip/change-color-auto");
inline static const muse::rcommand::Command TRACKEDIT_CLIP_CHANGE_COLOR_COMMAND("command://trackedit/clip/change-color");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_CHANGE_COLOR_COMMAND("command://trackedit/track/change-color");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_CHANGE_FORMAT_COMMAND("command://trackedit/track/change-format");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_CHANGE_RATE_COMMAND("command://trackedit/track/change-rate");

inline static const muse::rcommand::Command TRACKEDIT_GLOBAL_VIEW_SPECTROGRAM_COMMAND("command://trackedit/global-view-spectrogram");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_WAVEFORM_COMMAND("command://trackedit/track-view-waveform");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_SPECTROGRAM_COMMAND("command://trackedit/track-view-spectrogram");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_MULTI_COMMAND("command://trackedit/track-view-multi");

inline static const muse::rcommand::Command TRACKEDIT_LABEL_ADD_COMMAND("command://trackedit/label-add");
inline static const muse::rcommand::Command TRACKEDIT_RENAME_ITEM_COMMAND("command://trackedit/rename-item");

inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_ITEM_MOVE_LEFT_COMMAND("command://trackedit/track-view-item-move-left");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_ITEM_MOVE_RIGHT_COMMAND(
    "command://trackedit/track-view-item-move-right");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_ITEM_MOVE_UP_COMMAND("command://trackedit/track-view-item-move-up");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_ITEM_MOVE_DOWN_COMMAND("command://trackedit/track-view-item-move-down");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_ITEM_EXTEND_LEFT_COMMAND(
    "command://trackedit/track-view-item-extend-left");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_ITEM_EXTEND_RIGHT_COMMAND(
    "command://trackedit/track-view-item-extend-right");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_ITEM_REDUCE_LEFT_COMMAND(
    "command://trackedit/track-view-item-reduce-left");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_VIEW_ITEM_REDUCE_RIGHT_COMMAND(
    "command://trackedit/track-view-item-reduce-right");

inline static const muse::rcommand::Command TRACKEDIT_MERGE_SELECTED_ON_TRACKS_COMMAND("command://trackedit/merge-selected-on-tracks");
inline static const muse::rcommand::Command TRACKEDIT_DUPLICATE_SELECTED_COMMAND("command://trackedit/duplicate-selected");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_DELETE_COMMAND("command://trackedit/track-delete");

inline static const muse::rcommand::Command TRACKEDIT_COPY_COMMAND("command://trackedit/copy");
inline static const muse::rcommand::Command TRACKEDIT_CUT_COMMAND("command://trackedit/cut");
inline static const muse::rcommand::Command TRACKEDIT_DELETE_COMMAND("command://trackedit/delete");
inline static const muse::rcommand::Command TRACKEDIT_PASTE_DEFAULT_COMMAND("command://trackedit/paste-default");

inline static const muse::rcommand::Command TRACKEDIT_SPLIT_COMMAND("command://trackedit/split");
inline static const muse::rcommand::Command TRACKEDIT_SPLIT_INTO_NEW_TRACK_COMMAND("command://trackedit/split-into-new-track");
inline static const muse::rcommand::Command TRACKEDIT_JOIN_COMMAND("command://trackedit/join");
inline static const muse::rcommand::Command TRACKEDIT_DISJOIN_COMMAND("command://trackedit/disjoin");
inline static const muse::rcommand::Command TRACKEDIT_DUPLICATE_COMMAND("command://trackedit/duplicate");
inline static const muse::rcommand::Command TRACKEDIT_TRACK_SPLIT_COMMAND("command://trackedit/track-split");

inline static const muse::rcommand::Command TRACKEDIT_CUT_LEAVE_GAP_COMMAND("command://trackedit/cut-leave-gap");
inline static const muse::rcommand::Command TRACKEDIT_CUT_PER_CLIP_RIPPLE_COMMAND("command://trackedit/cut-per-clip-ripple");
inline static const muse::rcommand::Command TRACKEDIT_CUT_PER_TRACK_RIPPLE_COMMAND("command://trackedit/cut-per-track-ripple");
inline static const muse::rcommand::Command TRACKEDIT_CUT_ALL_TRACKS_RIPPLE_COMMAND("command://trackedit/cut-all-tracks-ripple");
inline static const muse::rcommand::Command TRACKEDIT_DELETE_LEAVE_GAP_COMMAND("command://trackedit/delete-leave-gap");
inline static const muse::rcommand::Command TRACKEDIT_DELETE_PER_CLIP_RIPPLE_COMMAND("command://trackedit/delete-per-clip-ripple");
inline static const muse::rcommand::Command TRACKEDIT_DELETE_PER_TRACK_RIPPLE_COMMAND("command://trackedit/delete-per-track-ripple");
inline static const muse::rcommand::Command TRACKEDIT_DELETE_ALL_TRACKS_RIPPLE_COMMAND("command://trackedit/delete-all-tracks-ripple");

inline static const std::string TRACKEDIT_TRACK_ID_PARAM("trackId");
inline static const std::string TRACKEDIT_CLIP_ID_PARAM("clipId");
inline static const std::string TRACKEDIT_TRACK_IDS_PARAM("trackIds");
inline static const std::string TRACKEDIT_PIVOTS_PARAM("pivots");
inline static const std::string TRACKEDIT_START_PARAM("start");
inline static const std::string TRACKEDIT_END_PARAM("end");
inline static const std::string TRACKEDIT_TRACK_INDEX_PARAM("trackIndex");
inline static const std::string TRACKEDIT_COLOR_INDEX_PARAM("colorindex");
inline static const std::string TRACKEDIT_FORMAT_PARAM("format");
inline static const std::string TRACKEDIT_RATE_PARAM("rate");
}
