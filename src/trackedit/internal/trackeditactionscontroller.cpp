/*
* Audacity: A Digital Audio Editor
*/
#include "trackeditactionscontroller.h"

#include <algorithm>
#include <limits>
#include <type_traits>

#include "framework/global/translation.h"
#include "framework/global/defer.h"
#include "framework/rcommand/actiontocommand.h"

#include "../trackeditcommands.h"

using namespace muse;
using namespace au::trackedit;
using namespace muse::async;
using namespace muse::actions;
using namespace muse::rcommand;

namespace {
muse::UriQuery makePlaybackPosUri()
{
    muse::UriQuery uri("audacity://trackedit/custom_time");
    uri.addParam("title", muse::Val(muse::trc("trackedit", "Playback position")));
    return uri;
}

CommandQuery queryParamsConv(const Command& command, const ActionData& args)
{
    CommandQuery query(command);
    if (args.empty()) {
        return query;
    }

    const ActionQuery legacy(args.arg<std::string>(0));
    query.setParams(legacy.params());
    return query;
}

CommandQuery clipKeyConv(const Command& command, const ActionData& args)
{
    CommandQuery query(command);
    if (args.empty()) {
        return query;
    }

    const ClipKey clipKey = args.arg<ClipKey>(0);
    query.addParam(TRACKEDIT_TRACK_ID_PARAM, Val(static_cast<int64_t>(clipKey.trackId)));
    query.addParam(TRACKEDIT_CLIP_ID_PARAM, Val(static_cast<int64_t>(clipKey.itemId)));
    return query;
}

CommandQuery trackSplitAtConv(const Command& command, const ActionData& args)
{
    CommandQuery query(command);
    IF_ASSERT_FAILED(args.count() == 2) {
        return query;
    }

    ValList trackIds;
    for (const TrackId& trackId : args.arg<TrackIdList>(0)) {
        trackIds.push_back(Val(static_cast<int64_t>(trackId)));
    }

    ValList pivots;
    for (const secs_t& pivot : args.arg<std::vector<secs_t> >(1)) {
        pivots.push_back(Val(pivot.to_double()));
    }

    query.addParam(TRACKEDIT_TRACK_IDS_PARAM, Val(trackIds));
    query.addParam(TRACKEDIT_PIVOTS_PARAM, Val(pivots));
    return query;
}

template<typename Op, typename ... Args>
Ret applyOp(const Op& op, Args&&... args)
{
    if constexpr (std::is_void_v<std::invoke_result_t<const Op&, Args...> >) {
        op(std::forward<Args>(args)...);
        return make_ok();
    } else {
        return op(std::forward<Args>(args)...);
    }
}

template<typename Op>
ICommandDispatcher::CallBackRet apply(Op op)
{
    return [op]() -> Ret {
        return applyOp(op);
    };
}

template<typename Op>
ICommandDispatcher::CallBackParamsRet applyToClip(Op op)
{
    return [op](const Params& params) -> Ret {
        if (!params.contains(TRACKEDIT_TRACK_ID_PARAM) || !params.contains(TRACKEDIT_CLIP_ID_PARAM)) {
            return make_ret(Ret::Code::BadArgs);
        }

        const ClipKey clipKey(params.at(TRACKEDIT_TRACK_ID_PARAM).toInt64(), params.at(TRACKEDIT_CLIP_ID_PARAM).toInt64());
        if (!clipKey.isValid()) {
            return make_ret(Ret::Code::BadArgs);
        }

        return applyOp(op, clipKey);
    };
}

template<typename Op>
ICommandDispatcher::CallBackParamsRet applyToTrack(Op op)
{
    return [op](const Params& params) -> Ret {
        if (!params.contains(TRACKEDIT_TRACK_ID_PARAM)) {
            return make_ret(Ret::Code::BadArgs);
        }

        return applyOp(op, TrackId(params.at(TRACKEDIT_TRACK_ID_PARAM).toInt64()));
    };
}

ActionCode legacyActionCode(const ActionQuery& query, const Params& params)
{
    ActionQuery legacy(query);
    for (const auto& [key, val] : params) {
        legacy.addParam(key, val);
    }
    return legacy.toString();
}
}

static const ActionCode TRACKEDIT_COPY_CODE("action://trackedit/copy");
static const ActionCode TRACKEDIT_CUT_CODE("action://trackedit/cut");
static const ActionCode TRACKEDIT_UNDO("action://trackedit/undo");
static const ActionCode TRACKEDIT_REDO("action://trackedit/redo");
static const ActionCode TRACKEDIT_DELETE_CODE("action://trackedit/delete");
static const ActionCode TRACKEDIT_CANCEL_CODE("action://trackedit/cancel");

static const ActionCode TRACKEDIT_PASTE_DEFAULT_CODE("action://trackedit/paste-default");
static const ActionCode TRACKEDIT_PASTE_OVERLAP_CODE("action://trackedit/paste-overlap");
static const ActionCode TRACKEDIT_PASTE_INSERT_CODE("action://trackedit/paste-insert");
static const ActionCode TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_CODE("action://trackedit/paste-insert-all-tracks-ripple");

static const ActionCode SPLIT_CODE("split");
static const ActionCode SPLIT_INTO_NEW_TRACK_CODE("split-into-new-track");
static const ActionCode JOIN_CODE("join");
static const ActionCode DISJOIN_CODE("disjoin");
static const ActionCode DUPLICATE_CODE("duplicate");

static const ActionCode CUT_PER_CLIP_RIPPLE_CODE("cut-per-clip-ripple");
static const ActionCode CUT_PER_TRACK_RIPPLE_CODE("cut-per-track-ripple");
static const ActionCode CUT_ALL_TRACKS_RIPPLE_CODE("cut-all-tracks-ripple");

static const ActionCode CUT_LEAVE_GAP_CODE("cut-leave-gap");
static const ActionCode DELETE_LEAVE_GAP_CODE("delete-leave-gap");
static const ActionCode DELETE_PER_CLIP_RIPPLE_CODE("delete-per-clip-ripple");
static const ActionCode DELETE_PER_TRACK_RIPPLE_CODE("delete-per-track-ripple");
static const ActionCode DELETE_ALL_TRACKS_RIPPLE_CODE("delete-all-tracks-ripple");

static const ActionCode CLIP_CUT_CODE("clip-cut");
static const ActionCode MULTI_CLIP_CUT_CODE("multi-clip-cut");
static const ActionCode RANGE_SELECTION_CUT_CODE("clip-cut-selected");

static const ActionCode CLIP_COPY_CODE("clip-copy");
static const ActionCode MULTI_CLIP_COPY_CODE("multi-clip-copy");
static const ActionCode RANGE_SELECTION_COPY_CODE("clip-copy-selected");

static const ActionCode CLIP_DELETE_CODE("clip-delete");
static const ActionCode MULTI_CLIP_DELETE_CODE("multi-clip-delete");
static const ActionCode RANGE_SELECTION_DELETE_CODE("clip-delete-selected");

static const ActionCode OPEN_CLIP_AND_SPEED_CODE("clip-pitch-speed-open");
static const ActionCode CLIP_RENDER_PITCH_AND_SPEED_CODE("clip-render-pitch-speed");
static const ActionCode CLIP_RESET_PITCH_AND_SPEED_CODE("clip-reset-pitch-speed");
static const ActionCode TRACK_SPLIT("track-split");
static const ActionCode TRACK_SPLIT_AT("track-split-at");
static const ActionCode SPLIT_CLIPS_AT_SILENCES("split-clips-at-silences");
static const ActionCode SPLIT_RANGE_SELECTION_AT_SILENCES("split-range-selection-at-silences");
static const ActionCode SPLIT_RANGE_SELECTION_INTO_NEW_TRACKS("split-range-selection-into-new-tracks");
static const ActionCode SPLIT_CLIPS_INTO_NEW_TRACKS("split-clips-into-new-tracks");
static const ActionCode MERGE_SELECTED_ON_TRACK("merge-selected-on-tracks");
static const ActionCode DUPLICATE_RANGE_SELECTION_CODE("duplicate-selected");
static const ActionCode DUPLICATE_CLIPS_CODE("duplicate-clips");
static const ActionCode CLIP_SPLIT_CUT("clip-split-cut");
static const ActionCode CLIP_SPLIT_DELETE("clip-split-delete");
static const ActionCode RANGE_SELECTION_SPLIT_CUT("split-cut-selected");
static const ActionCode RANGE_SELECTION_SPLIT_DELETE("split-delete-selected");
static const ActionCode NEW_MONO_TRACK("new-mono-track");
static const ActionCode NEW_STEREO_TRACK("new-stereo-track");
static const ActionCode NEW_LABEL_TRACK("new-label-track");
static const ActionCode TRACK_DELETE("track-delete");
static const ActionCode TRACK_DUPLICATE_CODE("track-duplicate");
static const ActionCode TRACK_MOVE_UP("track-move-up");
static const ActionCode TRACK_MOVE_DOWN("track-move-down");
static const ActionCode TRACK_MOVE_TOP("track-move-top");
static const ActionCode TRACK_MOVE_BOTTOM("track-move-bottom");
static const ActionCode TRACK_SWAP_CHANNELS("track-swap-channels");
static const ActionCode TRACK_SPLIT_STEREO_TO_LR("track-split-stereo-to-lr");
static const ActionCode TRACK_SPLIT_STEREO_TO_CENTER("track-split-stereo-to-center");
static const ActionCode TRACK_CHANGE_RATE_CUSTOM("track-change-rate-custom");
static const ActionCode TRACK_MAKE_STEREO("track-make-stereo");
static const ActionCode TRACK_RESAMPLE("track-resample");

static const ActionCode TRIM_AUDIO_OUTSIDE_SELECTION("trim-audio-outside-selection");
static const ActionCode SILENCE_AUDIO_SELECTION("silence-audio-selection");

static const ActionCode STRETCH_ENABLED_CODE("stretch-clip-to-match-tempo");

static const ActionCode GROUP_CLIPS_CODE("group-clips");
static const ActionCode UNGROUP_CLIPS_CODE("ungroup-clips");
static const ActionCode RENAME_ITEM_CODE("rename-item");

static const ActionCode SELECT_ALL("select-all");
static const ActionCode SELECT_CLEAR("clear-selection");
static const ActionCode SELECT_ALL_TRACKS("select-all-tracks");
static const ActionCode SELECT_LEFT_OF_PLAYBACK_POS("select-left-of-playback-position");
static const ActionCode SELECT_RIGHT_OF_PLAYBACK_POS("select-right-of-playback-position");
static const ActionCode SELECT_TRACK_START_TO_CURSOR("select-track-start-to-cursor");
static const ActionCode SELECT_CURSOR_TO_TRACK_END("select-cursor-to-track-end");
static const ActionCode SELECT_TRACK_START_TO_END("select-track-start-to-end");
static const ActionQuery SET_SELECTION("action://trackedit/set-selection");
static const ActionQuery SELECT_TRACK("action://trackedit/select-track");
static const ActionCode SELECT_ZERO_CROSSING("zero-cross");

static const ActionQuery AUTO_COLOR_QUERY("action://trackedit/clip/change-color-auto");
static const ActionQuery CHANGE_COLOR_QUERY("action://trackedit/clip/change-color");
static const ActionQuery TRACK_CHANGE_COLOR_QUERY("action://trackedit/track/change-color");
static const ActionQuery TRACK_CHANGE_FORMAT_QUERY("action://trackedit/track/change-format");
static const ActionQuery TRACK_CHANGE_RATE_QUERY("action://trackedit/track/change-rate");

static const ActionQuery TOGGLE_GLOBAL_VIEW_SPECTROGRAM("action://trackedit/global-view-spectrogram");
static const ActionQuery SET_TRACK_VIEW_WAVEFORM("action://trackedit/track-view-waveform");
static const ActionQuery SET_TRACK_VIEW_SPECTROGRAM("action://trackedit/track-view-spectrogram");
static const ActionQuery SET_TRACK_VIEW_MULTI("action://trackedit/track-view-multi");

static const ActionCode LABEL_ADD_CODE("label-add");

static const ActionCode LABEL_DELETE_MULTI_CODE("label-delete-multi");
static const ActionCode LABEL_CUT_MULTI_CODE("label-cut-multi");
static const ActionCode LABEL_COPY_MULTI_CODE("label-copy-multi");

static const ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

static const ActionCode TRACK_VIEW_ITEM_MOVE_LEFT_CODE("track-view-item-move-left");
static const ActionCode TRACK_VIEW_ITEM_MOVE_RIGHT_CODE("track-view-item-move-right");
static const ActionCode TRACK_VIEW_ITEM_MOVE_UP_CODE("track-view-item-move-up");
static const ActionCode TRACK_VIEW_ITEM_MOVE_DOWN_CODE("track-view-item-move-down");
static const ActionCode TRACK_VIEW_ITEM_EXTEND_LEFT_CODE("track-view-item-extend-left");
static const ActionCode TRACK_VIEW_ITEM_EXTEND_RIGHT_CODE("track-view-item-extend-right");
static const ActionCode TRACK_VIEW_ITEM_REDUCE_LEFT_CODE("track-view-item-reduce-left");
static const ActionCode TRACK_VIEW_ITEM_REDUCE_RIGHT_CODE("track-view-item-reduce-right");

constexpr double MIN_CLIP_WIDTH = 3.0;

// In principle, disabled are actions that modify the data involved in playback.
static const std::vector<ActionCode> actionsDisabledDuringRecording {
    TRACKEDIT_CUT_CODE,
    CUT_LEAVE_GAP_CODE,
    CUT_PER_CLIP_RIPPLE_CODE,
    CUT_PER_TRACK_RIPPLE_CODE,
    CUT_ALL_TRACKS_RIPPLE_CODE,
    TRACKEDIT_DELETE_CODE,
    DELETE_LEAVE_GAP_CODE,
    DELETE_PER_CLIP_RIPPLE_CODE,
    DELETE_PER_TRACK_RIPPLE_CODE,
    DELETE_ALL_TRACKS_RIPPLE_CODE,
    SPLIT_CODE,
    SPLIT_INTO_NEW_TRACK_CODE,
    JOIN_CODE,
    DUPLICATE_CODE,
    CLIP_CUT_CODE,
    MULTI_CLIP_CUT_CODE,
    RANGE_SELECTION_CUT_CODE,
    CLIP_DELETE_CODE,
    MULTI_CLIP_DELETE_CODE,
    RANGE_SELECTION_DELETE_CODE,
    CLIP_RENDER_PITCH_AND_SPEED_CODE,
    CLIP_RESET_PITCH_AND_SPEED_CODE,
    TRACKEDIT_PASTE_DEFAULT_CODE,
    TRACKEDIT_PASTE_OVERLAP_CODE,
    TRACKEDIT_PASTE_INSERT_CODE,
    TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_CODE,
    TRACK_SPLIT,
    TRACK_SPLIT_AT,
    SPLIT_CLIPS_AT_SILENCES,
    SPLIT_RANGE_SELECTION_AT_SILENCES,
    SPLIT_RANGE_SELECTION_INTO_NEW_TRACKS,
    SPLIT_CLIPS_INTO_NEW_TRACKS,
    MERGE_SELECTED_ON_TRACK,
    DUPLICATE_RANGE_SELECTION_CODE,
    DUPLICATE_CLIPS_CODE,
    CLIP_SPLIT_CUT,
    CLIP_SPLIT_DELETE,
    RANGE_SELECTION_SPLIT_CUT,
    RANGE_SELECTION_SPLIT_DELETE,
    NEW_MONO_TRACK,
    NEW_STEREO_TRACK,
    NEW_LABEL_TRACK,
    TRIM_AUDIO_OUTSIDE_SELECTION,
    SILENCE_AUDIO_SELECTION,
    TRACKEDIT_UNDO,
    TRACKEDIT_REDO,
    STRETCH_ENABLED_CODE,
    TRACK_DELETE,
    TRACK_DUPLICATE_CODE,
    TRACK_SWAP_CHANNELS,
    TRACK_SPLIT_STEREO_TO_LR,
    TRACK_SPLIT_STEREO_TO_CENTER,
    TRACK_CHANGE_RATE_CUSTOM,
    TRACK_MAKE_STEREO,
    TRACK_RESAMPLE,
    GROUP_CLIPS_CODE,
    UNGROUP_CLIPS_CODE,
};

void TrackeditActionsController::init()
{
    dispatcher()->reg(this, TRACKEDIT_COPY_CODE, this, &TrackeditActionsController::doGlobalCopy);
    dispatcher()->reg(this, TRACKEDIT_CUT_CODE, this, &TrackeditActionsController::doGlobalCut);
    dispatcher()->reg(this, TRACKEDIT_DELETE_CODE, this, &TrackeditActionsController::doGlobalDelete);

    dispatcher()->reg(this, TRACKEDIT_PASTE_DEFAULT_CODE, this, &TrackeditActionsController::pasteDefault);

    dispatcher()->reg(this, SPLIT_CODE, this, &TrackeditActionsController::doGlobalSplit);
    dispatcher()->reg(this, SPLIT_INTO_NEW_TRACK_CODE, this, &TrackeditActionsController::doGlobalSplitIntoNewTrack);
    dispatcher()->reg(this, JOIN_CODE, this, &TrackeditActionsController::doGlobalJoin);
    dispatcher()->reg(this, DISJOIN_CODE, this, &TrackeditActionsController::doGlobalDisjoin);
    dispatcher()->reg(this, DUPLICATE_CODE, this, &TrackeditActionsController::doGlobalDuplicate);

    dispatcher()->reg(this, CUT_LEAVE_GAP_CODE, this, &TrackeditActionsController::doGlobalCutLeaveGap);
    dispatcher()->reg(this, CUT_PER_CLIP_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalCutPerClipRipple);
    dispatcher()->reg(this, CUT_PER_TRACK_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalCutPerTrackRipple);
    dispatcher()->reg(this, CUT_ALL_TRACKS_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalCutAllTracksRipple);

    dispatcher()->reg(this, DELETE_LEAVE_GAP_CODE, this, &TrackeditActionsController::doGlobalDeleteLeaveGap);
    dispatcher()->reg(this, DELETE_PER_CLIP_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalDeletePerClipRipple);
    dispatcher()->reg(this, DELETE_PER_TRACK_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalDeletePerTrackRipple);
    dispatcher()->reg(this, DELETE_ALL_TRACKS_RIPPLE_CODE, this, &TrackeditActionsController::doGlobalDeleteAllTracksRipple);

    dispatcher()->reg(this, MULTI_CLIP_CUT_CODE, this, &TrackeditActionsController::multiClipCut);
    dispatcher()->reg(this, RANGE_SELECTION_CUT_CODE, this, &TrackeditActionsController::rangeSelectionCut);

    dispatcher()->reg(this, MULTI_CLIP_COPY_CODE, this, &TrackeditActionsController::multiClipCopy);
    dispatcher()->reg(this, RANGE_SELECTION_COPY_CODE, this, &TrackeditActionsController::rangeSelectionCopy);

    dispatcher()->reg(this, MULTI_CLIP_DELETE_CODE, this, &TrackeditActionsController::multiClipDelete);
    dispatcher()->reg(this, RANGE_SELECTION_DELETE_CODE, this, &TrackeditActionsController::rangeSelectionDelete);

    dispatcher()->reg(this, TRACK_SPLIT, this, &TrackeditActionsController::trackSplit);
    dispatcher()->reg(this, SPLIT_RANGE_SELECTION_AT_SILENCES, this, &TrackeditActionsController::splitRangeSelectionAtSilences);
    dispatcher()->reg(this, SPLIT_CLIPS_AT_SILENCES, this, &TrackeditActionsController::splitClipsAtSilences);
    dispatcher()->reg(this, SPLIT_RANGE_SELECTION_INTO_NEW_TRACKS, this, &TrackeditActionsController::splitRangeSelectionIntoNewTracks);
    dispatcher()->reg(this, SPLIT_CLIPS_INTO_NEW_TRACKS, this, &TrackeditActionsController::splitClipsIntoNewTracks);
    dispatcher()->reg(this, MERGE_SELECTED_ON_TRACK, this, &TrackeditActionsController::mergeSelectedOnTrack);
    dispatcher()->reg(this, DUPLICATE_RANGE_SELECTION_CODE, this, &TrackeditActionsController::duplicateSelected);
    dispatcher()->reg(this, DUPLICATE_CLIPS_CODE, this, &TrackeditActionsController::duplicateClips);
    dispatcher()->reg(this, RANGE_SELECTION_SPLIT_CUT, this, &TrackeditActionsController::splitCutSelected);
    dispatcher()->reg(this, RANGE_SELECTION_SPLIT_DELETE, this, &TrackeditActionsController::splitDeleteSelected);
    dispatcher()->reg(this, TRACK_DELETE, this, &TrackeditActionsController::deleteTracks);

    dispatcher()->reg(this, LABEL_DELETE_MULTI_CODE, this, &TrackeditActionsController::labelDeleteMulti);
    dispatcher()->reg(this, LABEL_CUT_MULTI_CODE, this, &TrackeditActionsController::labelCutMulti);
    dispatcher()->reg(this, LABEL_COPY_MULTI_CODE, this, &TrackeditActionsController::labelCopyMulti);

    auto cd = commandDispatcher();
    cd->onRequest(this, TRACKEDIT_UNDO_COMMAND, apply([this]() { return trackeditInteraction()->undo(); }));
    cd->onRequest(this, TRACKEDIT_REDO_COMMAND, apply([this]() { return trackeditInteraction()->redo(); }));
    cd->onRequest(this, TRACKEDIT_CANCEL_COMMAND, [this]() { return doGlobalCancel(); });

    cd->onRequest(this, TRACKEDIT_PASTE_OVERLAP_COMMAND, [this]() { return pasteOverlap(); });
    cd->onRequest(this, TRACKEDIT_PASTE_INSERT_COMMAND, [this]() { return pasteInsert(); });
    cd->onRequest(this, TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND, [this]() { return pasteInsertRipple(); });

    cd->onRequest(this, TRACKEDIT_CLIP_CUT_COMMAND, applyToClip([this](const ClipKey& clipKey) {
        trackeditInteraction()->clearClipboard();
        return trackeditInteraction()->cutClipIntoClipboard(clipKey);
    }));
    cd->onRequest(this, TRACKEDIT_CLIP_COPY_COMMAND, applyToClip([this](const ClipKey& clipKey) {
        trackeditInteraction()->clearClipboard();
        return trackeditInteraction()->copyClipIntoClipboard(clipKey);
    }));
    cd->onRequest(this, TRACKEDIT_CLIP_DELETE_COMMAND, applyToClip([this](const ClipKey& clipKey) {
        selectionController()->resetSelectedClips();
        return trackeditInteraction()->removeClip(clipKey);
    }));
    cd->onRequest(this, TRACKEDIT_CLIP_SPLIT_CUT_COMMAND, applyToClip([this](const ClipKey& clipKey) {
        trackeditInteraction()->clearClipboard();
        return trackeditInteraction()->clipSplitCut(clipKey);
    }));
    cd->onRequest(this, TRACKEDIT_CLIP_SPLIT_DELETE_COMMAND, applyToClip([this](const ClipKey& clipKey) {
        return trackeditInteraction()->clipSplitDelete(clipKey);
    }));
    cd->onRequest(this, TRACKEDIT_CLIP_PITCH_SPEED_OPEN_COMMAND, [this]() { return openClipPitchAndSpeed(); });
    cd->onRequest(this, TRACKEDIT_CLIP_RENDER_PITCH_SPEED_COMMAND, applyToClip([this](const ClipKey& clipKey) {
        return trackeditInteraction()->renderClipPitchAndSpeed(clipKey);
    }));
    cd->onRequest(this, TRACKEDIT_CLIP_RESET_PITCH_SPEED_COMMAND, applyToClip([this](const ClipKey& clipKey) {
        return trackeditInteraction()->resetClipPitchAndSpeed(clipKey);
    }));
    cd->onRequest(this, TRACKEDIT_STRETCH_CLIP_TO_MATCH_TEMPO_COMMAND, applyToClip([this](const ClipKey& clipKey) {
        const bool toggled = trackeditInteraction()->toggleStretchToMatchProjectTempo(clipKey);
        notifyActionCheckedChanged(STRETCH_ENABLED_CODE);
        return toggled;
    }));

    cd->onRequest(this, TRACKEDIT_TRACK_SPLIT_AT_COMMAND, [this](const Params& params) { return tracksSplitAt(params); });

    cd->onRequest(this, TRACKEDIT_NEW_MONO_TRACK_COMMAND, apply([this]() { return trackeditInteraction()->newMonoTrack(); }));
    cd->onRequest(this, TRACKEDIT_NEW_STEREO_TRACK_COMMAND, apply([this]() { return trackeditInteraction()->newStereoTrack(); }));
    cd->onRequest(this, TRACKEDIT_NEW_LABEL_TRACK_COMMAND, apply([this]() { return trackeditInteraction()->newLabelTrack().ret; }));

    cd->onRequest(this, TRACKEDIT_TRACK_DUPLICATE_COMMAND, [this]() { return duplicateTracks(); });
    cd->onRequest(this, TRACKEDIT_TRACK_MOVE_UP_COMMAND, [this]() { return moveTracksUp(); });
    cd->onRequest(this, TRACKEDIT_TRACK_MOVE_DOWN_COMMAND, [this]() { return moveTracksDown(); });
    cd->onRequest(this, TRACKEDIT_TRACK_MOVE_TOP_COMMAND, [this]() { return moveTracksToTop(); });
    cd->onRequest(this, TRACKEDIT_TRACK_MOVE_BOTTOM_COMMAND, [this]() { return moveTracksToBottom(); });
    cd->onRequest(this, TRACKEDIT_TRACK_SWAP_CHANNELS_COMMAND, [this]() { return swapStereoChannels(); });
    cd->onRequest(this, TRACKEDIT_TRACK_SPLIT_STEREO_TO_LR_COMMAND, [this]() { return splitStereoToLR(); });
    cd->onRequest(this, TRACKEDIT_TRACK_SPLIT_STEREO_TO_CENTER_COMMAND, [this]() { return splitStereoToCenter(); });
    cd->onRequest(this, TRACKEDIT_TRACK_CHANGE_RATE_CUSTOM_COMMAND, [this]() { return setCustomTrackRate(); });
    cd->onRequest(this, TRACKEDIT_TRACK_MAKE_STEREO_COMMAND, [this]() { return makeStereoTrack(); });
    cd->onRequest(this, TRACKEDIT_TRACK_RESAMPLE_COMMAND, [this]() { return resampleTracks(); });

    cd->onRequest(this, TRACKEDIT_TRIM_AUDIO_OUTSIDE_SELECTION_COMMAND, [this]() { return trimAudioOutsideSelection(); });
    cd->onRequest(this, TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND, [this]() { return doGlobalSilence(); });

    cd->onRequest(this, TRACKEDIT_GROUP_CLIPS_COMMAND, [this]() { return groupClips(); });
    cd->onRequest(this, TRACKEDIT_UNGROUP_CLIPS_COMMAND, [this]() { return ungroupClips(); });

    cd->onRequest(this, TRACKEDIT_SELECT_ALL_COMMAND, apply([this]() { selectionController()->setSelectedAllAudioData(); }));
    cd->onRequest(this, TRACKEDIT_CLEAR_SELECTION_COMMAND, [this]() { return selectNone(); });
    cd->onRequest(this, TRACKEDIT_SELECT_ALL_TRACKS_COMMAND, [this]() { return selectAllTracks(); });
    cd->onRequest(this, TRACKEDIT_SELECT_LEFT_OF_PLAYBACK_POSITION_COMMAND, [this]() { return selectLeftOfPlaybackPos(); });
    cd->onRequest(this, TRACKEDIT_SELECT_RIGHT_OF_PLAYBACK_POSITION_COMMAND, [this]() { return selectRightOfPlaybackPos(); });
    cd->onRequest(this, TRACKEDIT_SELECT_TRACK_START_TO_CURSOR_COMMAND, [this]() { return selectTrackStartToCursor(); });
    cd->onRequest(this, TRACKEDIT_SELECT_CURSOR_TO_TRACK_END_COMMAND, [this]() { return selectCursorToTrackEnd(); });
    cd->onRequest(this, TRACKEDIT_SELECT_TRACK_START_TO_END_COMMAND, [this]() { return selectTrackStartToEnd(); });
    cd->onRequest(this, TRACKEDIT_SET_SELECTION_COMMAND, [this](const Params& params) { return setSelection(params); });
    cd->onRequest(this, TRACKEDIT_SELECT_TRACK_COMMAND, [this](const Params& params) { return selectTrackByIndex(params); });
    cd->onRequest(this, TRACKEDIT_ZERO_CROSS_COMMAND, [this]() { return moveCursorToClosestZeroCrossing(); });

    cd->onRequest(this, TRACKEDIT_CLIP_CHANGE_COLOR_AUTO_COMMAND, [this](const Params& params) {
        return setClipColor(params, [this, params]() { notifyActionCheckedChanged(legacyActionCode(AUTO_COLOR_QUERY, params)); });
    });
    cd->onRequest(this, TRACKEDIT_CLIP_CHANGE_COLOR_COMMAND, [this](const Params& params) {
        return setClipColor(params, [this, params]() { notifyActionCheckedChanged(legacyActionCode(CHANGE_COLOR_QUERY, params)); });
    });
    cd->onRequest(this, TRACKEDIT_TRACK_CHANGE_COLOR_COMMAND, [this](const Params& params) { return setTrackColor(params); });
    cd->onRequest(this, TRACKEDIT_TRACK_CHANGE_FORMAT_COMMAND, [this](const Params& params) { return setTrackFormat(params); });
    cd->onRequest(this, TRACKEDIT_TRACK_CHANGE_RATE_COMMAND, [this](const Params& params) { return setTrackRate(params); });

    cd->onRequest(this, TRACKEDIT_GLOBAL_VIEW_SPECTROGRAM_COMMAND, [this]() { return toggleGlobalSpectrogramView(); });
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_WAVEFORM_COMMAND, applyToTrack([this](const TrackId& trackId) {
        return changeTrackView(trackId, TrackViewType::Waveform);
    }));
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_SPECTROGRAM_COMMAND, applyToTrack([this](const TrackId& trackId) {
        return changeTrackView(trackId, TrackViewType::Spectrogram);
    }));
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_MULTI_COMMAND, applyToTrack([this](const TrackId& trackId) {
        return changeTrackView(trackId, TrackViewType::WaveformAndSpectrogram);
    }));

    cd->onRequest(this, TRACKEDIT_LABEL_ADD_COMMAND, [this]() { return addLabel(); });
    cd->onRequest(this, TRACKEDIT_RENAME_ITEM_COMMAND, [this]() { return renameSelectedItem(); });

    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_ITEM_MOVE_LEFT_COMMAND, apply([this]() { moveFocusedItem(-calculateStepSize(), 0); }));
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_ITEM_MOVE_RIGHT_COMMAND, apply([this]() { moveFocusedItem(calculateStepSize(), 0); }));
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_ITEM_MOVE_UP_COMMAND, apply([this]() { moveFocusedItem(0.0, -1); }));
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_ITEM_MOVE_DOWN_COMMAND, apply([this]() { moveFocusedItem(0.0, 1); }));
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_ITEM_EXTEND_LEFT_COMMAND, [this]() { return extendFocusedItemBoundaryLeft(); });
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_ITEM_EXTEND_RIGHT_COMMAND, [this]() { return extendFocusedItemBoundaryRight(); });
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_ITEM_REDUCE_LEFT_COMMAND, [this]() { return reduceFocusedItemBoundaryLeft(); });
    cd->onRequest(this, TRACKEDIT_TRACK_VIEW_ITEM_REDUCE_RIGHT_COMMAND, [this]() { return reduceFocusedItemBoundaryRight(); });

    static const std::vector<ActionToCommand> actionToCommand = {
        { TRACKEDIT_UNDO, TRACKEDIT_UNDO_COMMAND, {} },
        { TRACKEDIT_REDO, TRACKEDIT_REDO_COMMAND, {} },
        { TRACKEDIT_CANCEL_CODE, TRACKEDIT_CANCEL_COMMAND, {} },
        { TRACKEDIT_PASTE_OVERLAP_CODE, TRACKEDIT_PASTE_OVERLAP_COMMAND, {} },
        { TRACKEDIT_PASTE_INSERT_CODE, TRACKEDIT_PASTE_INSERT_COMMAND, {} },
        { TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_CODE, TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND, {} },
        { CLIP_CUT_CODE, TRACKEDIT_CLIP_CUT_COMMAND, clipKeyConv },
        { CLIP_COPY_CODE, TRACKEDIT_CLIP_COPY_COMMAND, clipKeyConv },
        { CLIP_DELETE_CODE, TRACKEDIT_CLIP_DELETE_COMMAND, clipKeyConv },
        { CLIP_SPLIT_CUT, TRACKEDIT_CLIP_SPLIT_CUT_COMMAND, clipKeyConv },
        { CLIP_SPLIT_DELETE, TRACKEDIT_CLIP_SPLIT_DELETE_COMMAND, clipKeyConv },
        { OPEN_CLIP_AND_SPEED_CODE, TRACKEDIT_CLIP_PITCH_SPEED_OPEN_COMMAND, {} },
        { CLIP_RENDER_PITCH_AND_SPEED_CODE, TRACKEDIT_CLIP_RENDER_PITCH_SPEED_COMMAND, clipKeyConv },
        { CLIP_RESET_PITCH_AND_SPEED_CODE, TRACKEDIT_CLIP_RESET_PITCH_SPEED_COMMAND, clipKeyConv },
        { STRETCH_ENABLED_CODE, TRACKEDIT_STRETCH_CLIP_TO_MATCH_TEMPO_COMMAND, clipKeyConv },
        { TRACK_SPLIT_AT, TRACKEDIT_TRACK_SPLIT_AT_COMMAND, trackSplitAtConv },
        { NEW_MONO_TRACK, TRACKEDIT_NEW_MONO_TRACK_COMMAND, {} },
        { NEW_STEREO_TRACK, TRACKEDIT_NEW_STEREO_TRACK_COMMAND, {} },
        { NEW_LABEL_TRACK, TRACKEDIT_NEW_LABEL_TRACK_COMMAND, {} },
        { TRACK_DUPLICATE_CODE, TRACKEDIT_TRACK_DUPLICATE_COMMAND, {} },
        { TRACK_MOVE_UP, TRACKEDIT_TRACK_MOVE_UP_COMMAND, {} },
        { TRACK_MOVE_DOWN, TRACKEDIT_TRACK_MOVE_DOWN_COMMAND, {} },
        { TRACK_MOVE_TOP, TRACKEDIT_TRACK_MOVE_TOP_COMMAND, {} },
        { TRACK_MOVE_BOTTOM, TRACKEDIT_TRACK_MOVE_BOTTOM_COMMAND, {} },
        { TRACK_SWAP_CHANNELS, TRACKEDIT_TRACK_SWAP_CHANNELS_COMMAND, {} },
        { TRACK_SPLIT_STEREO_TO_LR, TRACKEDIT_TRACK_SPLIT_STEREO_TO_LR_COMMAND, {} },
        { TRACK_SPLIT_STEREO_TO_CENTER, TRACKEDIT_TRACK_SPLIT_STEREO_TO_CENTER_COMMAND, {} },
        { TRACK_CHANGE_RATE_CUSTOM, TRACKEDIT_TRACK_CHANGE_RATE_CUSTOM_COMMAND, {} },
        { TRACK_MAKE_STEREO, TRACKEDIT_TRACK_MAKE_STEREO_COMMAND, {} },
        { TRACK_RESAMPLE, TRACKEDIT_TRACK_RESAMPLE_COMMAND, {} },
        { TRIM_AUDIO_OUTSIDE_SELECTION, TRACKEDIT_TRIM_AUDIO_OUTSIDE_SELECTION_COMMAND, {} },
        { SILENCE_AUDIO_SELECTION, TRACKEDIT_SILENCE_AUDIO_SELECTION_COMMAND, {} },
        { GROUP_CLIPS_CODE, TRACKEDIT_GROUP_CLIPS_COMMAND, {} },
        { UNGROUP_CLIPS_CODE, TRACKEDIT_UNGROUP_CLIPS_COMMAND, {} },
        { SELECT_ALL, TRACKEDIT_SELECT_ALL_COMMAND, {} },
        { SELECT_CLEAR, TRACKEDIT_CLEAR_SELECTION_COMMAND, {} },
        { SELECT_ALL_TRACKS, TRACKEDIT_SELECT_ALL_TRACKS_COMMAND, {} },
        { SELECT_LEFT_OF_PLAYBACK_POS, TRACKEDIT_SELECT_LEFT_OF_PLAYBACK_POSITION_COMMAND, {} },
        { SELECT_RIGHT_OF_PLAYBACK_POS, TRACKEDIT_SELECT_RIGHT_OF_PLAYBACK_POSITION_COMMAND, {} },
        { SELECT_TRACK_START_TO_CURSOR, TRACKEDIT_SELECT_TRACK_START_TO_CURSOR_COMMAND, {} },
        { SELECT_CURSOR_TO_TRACK_END, TRACKEDIT_SELECT_CURSOR_TO_TRACK_END_COMMAND, {} },
        { SELECT_TRACK_START_TO_END, TRACKEDIT_SELECT_TRACK_START_TO_END_COMMAND, {} },
        { SET_SELECTION.toString(), TRACKEDIT_SET_SELECTION_COMMAND, queryParamsConv },
        { SELECT_TRACK.toString(), TRACKEDIT_SELECT_TRACK_COMMAND, queryParamsConv },
        { SELECT_ZERO_CROSSING, TRACKEDIT_ZERO_CROSS_COMMAND, {} },
        { AUTO_COLOR_QUERY.toString(), TRACKEDIT_CLIP_CHANGE_COLOR_AUTO_COMMAND, {} },
        { CHANGE_COLOR_QUERY.toString(), TRACKEDIT_CLIP_CHANGE_COLOR_COMMAND, queryParamsConv },
        { TRACK_CHANGE_COLOR_QUERY.toString(), TRACKEDIT_TRACK_CHANGE_COLOR_COMMAND, queryParamsConv },
        { TRACK_CHANGE_FORMAT_QUERY.toString(), TRACKEDIT_TRACK_CHANGE_FORMAT_COMMAND, queryParamsConv },
        { TRACK_CHANGE_RATE_QUERY.toString(), TRACKEDIT_TRACK_CHANGE_RATE_COMMAND, queryParamsConv },
        { TOGGLE_GLOBAL_VIEW_SPECTROGRAM.toString(), TRACKEDIT_GLOBAL_VIEW_SPECTROGRAM_COMMAND, {} },
        { SET_TRACK_VIEW_WAVEFORM.toString(), TRACKEDIT_TRACK_VIEW_WAVEFORM_COMMAND, queryParamsConv },
        { SET_TRACK_VIEW_SPECTROGRAM.toString(), TRACKEDIT_TRACK_VIEW_SPECTROGRAM_COMMAND, queryParamsConv },
        { SET_TRACK_VIEW_MULTI.toString(), TRACKEDIT_TRACK_VIEW_MULTI_COMMAND, queryParamsConv },
        { LABEL_ADD_CODE, TRACKEDIT_LABEL_ADD_COMMAND, {} },
        { RENAME_ITEM_CODE, TRACKEDIT_RENAME_ITEM_COMMAND, {} },
        { TRACK_VIEW_ITEM_MOVE_LEFT_CODE, TRACKEDIT_TRACK_VIEW_ITEM_MOVE_LEFT_COMMAND, {} },
        { TRACK_VIEW_ITEM_MOVE_RIGHT_CODE, TRACKEDIT_TRACK_VIEW_ITEM_MOVE_RIGHT_COMMAND, {} },
        { TRACK_VIEW_ITEM_MOVE_UP_CODE, TRACKEDIT_TRACK_VIEW_ITEM_MOVE_UP_COMMAND, {} },
        { TRACK_VIEW_ITEM_MOVE_DOWN_CODE, TRACKEDIT_TRACK_VIEW_ITEM_MOVE_DOWN_COMMAND, {} },
        { TRACK_VIEW_ITEM_EXTEND_LEFT_CODE, TRACKEDIT_TRACK_VIEW_ITEM_EXTEND_LEFT_COMMAND, {} },
        { TRACK_VIEW_ITEM_EXTEND_RIGHT_CODE, TRACKEDIT_TRACK_VIEW_ITEM_EXTEND_RIGHT_COMMAND, {} },
        { TRACK_VIEW_ITEM_REDUCE_LEFT_CODE, TRACKEDIT_TRACK_VIEW_ITEM_REDUCE_LEFT_COMMAND, {} },
        { TRACK_VIEW_ITEM_REDUCE_RIGHT_CODE, TRACKEDIT_TRACK_VIEW_ITEM_REDUCE_RIGHT_COMMAND, {} },
    };
    registerActionToCommand(this, actionToCommand, commandDispatcher(), dispatcher());

    projectHistory()->historyChanged().onReceive(this, [this](auto) {
        notifyActionEnabledChanged(TRACKEDIT_UNDO);
        notifyActionEnabledChanged(TRACKEDIT_REDO);
        notifyActionEnabledChanged(SILENCE_AUDIO_SELECTION);
    });

    globalContext()->isRecordingChanged().onNotify(this, [this]() {
        for (const auto& actionCode : actionsDisabledDuringRecording) {
            notifyActionEnabledChanged(actionCode);
        }
    });

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList&) {
        notifyActionEnabledChanged(GROUP_CLIPS_CODE);
        notifyActionEnabledChanged(UNGROUP_CLIPS_CODE);
        notifyActionEnabledChanged(JOIN_CODE);
        notifyActionEnabledChanged(SILENCE_AUDIO_SELECTION);
        notifyActionEnabledChanged(RENAME_ITEM_CODE);
    });

    selectionController()->labelsSelected().onReceive(this, [this](const trackedit::LabelKeyList&) {
        notifyActionEnabledChanged(RENAME_ITEM_CODE);
    });

    selectionController()->clipsIntersectingRangeSelectionChanged().onReceive(this, [this](const trackedit::ClipKeyList&) {
        notifyActionEnabledChanged(JOIN_CODE);
    });

    selectionController()->selectedTracksChanged().onReceive(this, [this](const trackedit::TrackIdList&) {
        notifyActionEnabledChanged(SILENCE_AUDIO_SELECTION);
    });

    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t) {
        notifyActionEnabledChanged(SILENCE_AUDIO_SELECTION);
    });

    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t) {
        notifyActionEnabledChanged(SILENCE_AUDIO_SELECTION);
    });
}

bool TrackeditActionsController::actionEnabled(const muse::actions::ActionCode& actionCode) const
{
    if (!canReceiveAction(actionCode)) {
        return false;
    }

    return true;
}

muse::async::Channel<muse::actions::ActionCode> TrackeditActionsController::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

void TrackeditActionsController::notifyActionEnabledChanged(const ActionCode& actionCode)
{
    m_actionEnabledChanged.send(actionCode);
}

void TrackeditActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

bool TrackeditActionsController::isFocusedItemClip() const
{
    const std::optional<TrackItemKey> focusedItemKey = trackNavigationController()->focus().itemKey();
    if (!focusedItemKey) {
        return false;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    return prj ? prj->track(focusedItemKey->trackId)->type != TrackType::Label : false;
}

ClipKeyList TrackeditActionsController::clipsForInteraction() const
{
    ClipKeyList result = selectionController()->selectedClips();

    const std::optional<TrackItemKey> focusedItemKey = trackNavigationController()->focus().itemKey();
    if (focusedItemKey && !muse::contains(result, *focusedItemKey)) {
        if (isFocusedItemClip()) {
            result.insert(result.cbegin(), *focusedItemKey);
        }
    }

    return result;
}

bool TrackeditActionsController::isFocusedItemLabel() const
{
    const std::optional<TrackItemKey> focusedItemKey = trackNavigationController()->focus().itemKey();
    if (!focusedItemKey) {
        return false;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    return prj ? prj->track(focusedItemKey->trackId)->type == TrackType::Label : false;
}

LabelKeyList TrackeditActionsController::labelsForInteraction() const
{
    LabelKeyList result = selectionController()->selectedLabels();

    const std::optional<TrackItemKey> focusedItemKey = trackNavigationController()->focus().itemKey();
    if (focusedItemKey && !muse::contains(result, *focusedItemKey)) {
        if (isFocusedItemLabel()) {
            result.insert(result.cbegin(), *focusedItemKey);
        }
    }

    return result;
}

void TrackeditActionsController::doGlobalCopy()
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_COPY_CODE);
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_COPY_CODE);
        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_COPY_MULTI_CODE);
        return;
    }
}

void TrackeditActionsController::doGlobalCut()
{
    const bool isTrackSelected = selectionController()->timeSelectionIsEmpty() && !selectionController()->selectedTracks().empty()
                                 && !selectionController()->hasSelectedClips() && !selectionController()->hasSelectedLabels();

    const bool wasSet = configuration()->deleteBehavior() != DeleteBehavior::NotSet;

    if (!isTrackSelected) {
        if (!wasSet && !m_deleteBehaviorOnboardingScenario.showOnboardingDialog()) {
            return;
        }

        muse::Defer showFollowup([this, wasSet]() {
            if (!wasSet) {
                m_deleteBehaviorOnboardingScenario.showFollowupDialog();
            }
        });

        const DeleteBehavior deleteBehavior = configuration()->deleteBehavior();
        IF_ASSERT_FAILED(deleteBehavior != DeleteBehavior::NotSet) {
            return;
        }

        if (deleteBehavior != DeleteBehavior::NotSet && deleteBehavior != DeleteBehavior::LeaveGap) {
            switch (configuration()->closeGapBehavior()) {
            case CloseGapBehavior::ClipRipple:
                doGlobalCutPerClipRipple();
                break;
            case CloseGapBehavior::TrackRipple:
                doGlobalCutPerTrackRipple();
                break;
            case CloseGapBehavior::AllTracksRipple:
                doGlobalCutAllTracksRipple();
                break;
            default:
                LOGE() << "Unexpected close gap behavior: " << static_cast<int>(configuration()->closeGapBehavior());
                assert(false);
            }
            return;
        }
    }

    if (!selectionController()->timeSelectionIsEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(RANGE_SELECTION_SPLIT_CUT,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));

        frequencySelectionController()->resetFrequencySelection();
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_CUT_CODE, ActionData::make_arg1(false));
        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_CUT_MULTI_CODE, ActionData::make_arg1(false));
        return;
    }
}

void TrackeditActionsController::doGlobalCutLeaveGap()
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(RANGE_SELECTION_SPLIT_CUT,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));

        frequencySelectionController()->resetFrequencySelection();
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_CUT_CODE, ActionData::make_arg1(false));
        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_CUT_MULTI_CODE, ActionData::make_arg1(false));
        return;
    }
}

void TrackeditActionsController::doGlobalCutPerClipRipple()
{
    auto moveClips = ActionData::make_arg1(false);
    if (!selectionController()->timeSelectionIsEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_CUT_CODE, moveClips);
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_CUT_CODE, moveClips);
        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_CUT_MULTI_CODE, moveClips);
        return;
    }
}

void TrackeditActionsController::doGlobalCutPerTrackRipple()
{
    auto moveClips = ActionData::make_arg1(true);
    if (!selectionController()->timeSelectionIsEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_CUT_CODE, moveClips);
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_CUT_CODE, moveClips);
        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_CUT_MULTI_CODE, moveClips);
        return;
    }
}

void TrackeditActionsController::doGlobalCutAllTracksRipple()
{
    auto moveClips = ActionData::make_arg1(true);

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackIdList();

    if (!selectionController()->timeSelectionIsEmpty()) {
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        trackeditInteraction()->clearClipboard();
        trackeditInteraction()->cutItemDataIntoClipboard(tracks, selectedStartTime, selectedEndTime, true, true /*isRangeSelection*/);

        selectionController()->resetDataSelection();
        return;
    }

    auto selectedStartTime = selectionController()->leftMostSelectedItemStartTime();
    auto selectedEndTime = selectionController()->rightMostSelectedItemEndTime();

    if (selectedStartTime.has_value() && selectedEndTime.has_value()) {
        trackeditInteraction()->clearClipboard();
        trackeditInteraction()->cutItemDataIntoClipboard(tracks, selectedStartTime.value(), selectedEndTime.value(), true,
                                                         false /*isRangeSelection*/);

        selectionController()->resetSelectedClips();
        selectionController()->resetSelectedLabels();
    }
}

void TrackeditActionsController::doGlobalDelete()
{
    const bool isTrackSelected = selectionController()->timeSelectionIsEmpty() && !selectionController()->selectedTracks().empty()
                                 && !selectionController()->hasSelectedClips() && !selectionController()->hasSelectedLabels();

    const bool wasSet = configuration()->deleteBehavior() != DeleteBehavior::NotSet;

    if (!isTrackSelected) {
        if (!wasSet && !m_deleteBehaviorOnboardingScenario.showOnboardingDialog()) {
            return;
        }

        muse::Defer showFollowup([this, wasSet]() {
            if (!wasSet) {
                m_deleteBehaviorOnboardingScenario.showFollowupDialog();
            }
        });

        const DeleteBehavior deleteBehavior = configuration()->deleteBehavior();
        IF_ASSERT_FAILED(deleteBehavior != DeleteBehavior::NotSet) {
            return;
        }

        if (deleteBehavior != DeleteBehavior::LeaveGap) {
            switch (configuration()->closeGapBehavior()) {
            case CloseGapBehavior::ClipRipple:
                doGlobalDeletePerClipRipple();
                break;
            case CloseGapBehavior::TrackRipple:
                doGlobalDeletePerTrackRipple();
                break;
            case CloseGapBehavior::AllTracksRipple:
                doGlobalDeleteAllTracksRipple();
                break;
            default:
                LOGE() << "Unexpected close gap behavior: " << static_cast<int>(configuration()->closeGapBehavior());
                assert(false);
            }
            return;
        }
    }

    const auto frequencySelection = frequencySelectionController()->frequencySelection();
    if (frequencySelectionController()->showsSpectrogram(frequencySelection.trackId) && frequencySelection.isValid()) {
        using namespace spectrogram;
        if (const std::optional<SpectralEffect> effect = spectralEffectsRegister()->spectralEffect(SpectralEffectId::DeleteSelection)) {
            dispatcher()->dispatch(effect->action);
            return;
        }
    }

    if (!selectionController()->timeSelectionIsEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(RANGE_SELECTION_SPLIT_DELETE,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));

        frequencySelectionController()->resetFrequencySelection();
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_DELETE_CODE, ActionData::make_arg1(false));

        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_DELETE_MULTI_CODE, ActionData::make_arg1(false));
        return;
    }

    if (!selectionController()->selectedTracks().empty()) {
        dispatcher()->dispatch(TRACK_DELETE);
        return;
    }

    //: Title of an error dialog shown when an action requires selected audio
    interactive()->errorSync(muse::trc("trackedit", "No audio selected"),
                             //: Message of an error dialog shown when an action requires selected audio
                             muse::trc("trackedit", "Select the audio to delete and try again."));
}

muse::Ret TrackeditActionsController::doGlobalCancel()
{
    const bool interactionOngoing = projectHistory()->interactionOngoing();
    trackeditInteraction()->notifyAboutCancelDragEdit();

    // Cancel the drag without clearing the selection restored by rollback.
    if (interactionOngoing) {
        return make_ok();
    }

    const std::optional<TrackItemKey> focusedItem = trackNavigationController()->focus().itemKey();
    const ClipKeyList selectedClips = selectionController()->selectedClips();
    const LabelKeyList selectedLabels = selectionController()->selectedLabels();

    const TrackId currentTrack = currentFocusedOrSelectedTrack();

    //! [Stage 1] A clip/label is focused: drop the navigation focus, then step out of the current
    //! item: onto the selection if the focus is not part of it, otherwise back onto the track.
    if (focusedItem) {
        trackNavigationController()->resetNavigation();

        if (stepFocusOutOfSelection(selectedClips, *focusedItem, currentTrack,
                                    [this]() { selectionController()->resetSelectedClips(); })) {
            return make_ok();
        }
        if (stepFocusOutOfSelection(selectedLabels, *focusedItem, currentTrack,
                                    [this]() { selectionController()->resetSelectedLabels(); })) {
            return make_ok();
        }

        focusTrack(currentTrack);
        return make_ok();
    }

    //! [Stage 2] No item is focused: reset the active selection and move the focus back to the track.
    if (!selectedClips.empty()) {
        selectionController()->resetSelectedClips();
    } else if (!selectedLabels.empty()) {
        selectionController()->resetSelectedLabels();
    } else if (!selectionController()->timeSelectionIsEmpty()) {
        selectionController()->resetTimeSelection();
    } else {
        return make_ok();
    }

    focusTrack(currentTrack);
    return make_ok();
}

void TrackeditActionsController::focusTrack(const TrackId& trackId)
{
    if (trackId != INVALID_TRACK) {
        trackNavigationController()->setFocus(TrackFocus::track(trackId), false /*highlight*/);
    }
}

bool TrackeditActionsController::stepFocusOutOfSelection(const TrackItemKeyList& selectedItems,
                                                         const TrackItemKey& focusedItem, const TrackId& currentTrack,
                                                         const std::function<void()>& resetSelection)
{
    if (selectedItems.empty()) {
        return false;
    }

    //! NOTE: if the focus sits on one of the selected items, cancel that selection and fall back to
    //! the track; otherwise step the focus onto the selection
    if (muse::contains(selectedItems, focusedItem)) {
        resetSelection();
        focusTrack(currentTrack);
    } else {
        trackNavigationController()->setFocus(TrackFocus::item(selectedItems.front()), false /*highlight*/);
    }

    return true;
}

TrackId TrackeditActionsController::currentFocusedOrSelectedTrack() const
{
    const TrackId focusedTrack = trackNavigationController()->focus().trackId;
    if (focusedTrack != INVALID_TRACK) {
        return focusedTrack;
    }

    const TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (!selectedTracks.empty()) {
        return selectedTracks.front();
    }

    const ClipKeyList selectedClips = selectionController()->selectedClipsInTrackOrder();
    if (!selectedClips.empty()) {
        return selectedClips.front().trackId;
    }

    const LabelKeyList selectedLabels = selectionController()->selectedLabelsInTrackOrder();
    if (!selectedLabels.empty()) {
        return selectedLabels.front().trackId;
    }

    return INVALID_TRACK;
}

void TrackeditActionsController::doGlobalDeleteLeaveGap()
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(RANGE_SELECTION_SPLIT_DELETE,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));

        frequencySelectionController()->resetFrequencySelection();
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_DELETE_CODE, ActionData::make_arg1(false));
        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_DELETE_MULTI_CODE, ActionData::make_arg1(false));
        return;
    }

    interactive()->errorSync(muse::trc("trackedit", "No audio selected"),
                             muse::trc("trackedit", "Select the audio to delete and try again."));
}

void TrackeditActionsController::doGlobalDeletePerClipRipple()
{
    auto moveClips = ActionData::make_arg1(false);

    if (!selectionController()->timeSelectionIsEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_DELETE_CODE, moveClips);
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_DELETE_CODE, moveClips);
        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_DELETE_MULTI_CODE, moveClips);
        return;
    }

    interactive()->errorSync(muse::trc("trackedit", "No audio selected"),
                             muse::trc("trackedit", "Select the audio to delete and try again."));
}

void TrackeditActionsController::doGlobalDeletePerTrackRipple()
{
    auto moveClips = ActionData::make_arg1(true);

    if (!selectionController()->timeSelectionIsEmpty()) {
        dispatcher()->dispatch(RANGE_SELECTION_DELETE_CODE, moveClips);
        return;
    }

    if (!clipsForInteraction().empty()) {
        dispatcher()->dispatch(MULTI_CLIP_DELETE_CODE, moveClips);
        return;
    }

    if (!labelsForInteraction().empty()) {
        dispatcher()->dispatch(LABEL_DELETE_MULTI_CODE, moveClips);
        return;
    }

    interactive()->errorSync(muse::trc("trackedit", "No audio selected"),
                             muse::trc("trackedit", "Select the audio to delete and try again."));
}

void TrackeditActionsController::doGlobalDeleteAllTracksRipple()
{
    auto moveClips = ActionData::make_arg1(true);

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackIdList();

    if (!selectionController()->timeSelectionIsEmpty()) {
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        trackeditInteraction()->removeTracksData(tracks, selectedStartTime, selectedEndTime, true);

        selectionController()->resetDataSelection();
        return;
    }

    auto selectedStartTime = selectionController()->leftMostSelectedItemStartTime();
    auto selectedEndTime = selectionController()->rightMostSelectedItemEndTime();

    if (selectedStartTime.has_value() && selectedEndTime.has_value()) {
        trackeditInteraction()->removeTracksData(tracks, selectedStartTime.value(), selectedEndTime.value(), true);

        selectionController()->resetDataSelection();
        return;
    }

    interactive()->errorSync(muse::trc("trackedit", "No audio selected"),
                             muse::trc("trackedit", "Select the audio to delete and try again."));
}

void TrackeditActionsController::doGlobalSplit()
{
    TrackIdList tracksIdsToSplit = selectionController()->selectedTracks();

    if (tracksIdsToSplit.empty()) {
        return;
    }

    std::vector<secs_t> pivots;
    if (!selectionController()->timeSelectionIsEmpty()) {
        pivots.push_back(selectionController()->dataSelectedStartTime());
        pivots.push_back(selectionController()->dataSelectedEndTime());
    } else {
        pivots.push_back(playbackState()->playbackPosition());
    }

    dispatcher()->dispatch(TRACK_SPLIT_AT, ActionData::make_arg2<TrackIdList, std::vector<secs_t> >(tracksIdsToSplit, pivots));
}

void TrackeditActionsController::doGlobalSplitIntoNewTrack()
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();
        dispatcher()->dispatch(SPLIT_RANGE_SELECTION_INTO_NEW_TRACKS,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    ClipKeyList selectedClips = clipsForInteraction();
    if (selectedClips.empty()) {
        return;
    }

    dispatcher()->dispatch(SPLIT_CLIPS_INTO_NEW_TRACKS, ActionData::make_arg1<ClipKeyList>(selectedClips));
}

void TrackeditActionsController::doGlobalJoin()
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        auto selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(MERGE_SELECTED_ON_TRACK,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    const std::optional<ContiguousClipsSpan> span = contiguousSelectedClipsSpan();
    if (!span) {
        return;
    }

    dispatcher()->dispatch(MERGE_SELECTED_ON_TRACK,
                           ActionData::make_arg3<TrackIdList, secs_t, secs_t>(TrackIdList { span->trackId }, span->begin, span->end));
}

std::optional<TrackeditActionsController::ContiguousClipsSpan> TrackeditActionsController::contiguousSelectedClipsSpan() const
{
    const ClipKeyList clipKeys = selectionController()->selectedClips();
    if (clipKeys.size() < 2) {
        return std::nullopt;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return std::nullopt;
    }

    const TrackId trackId = clipKeys.front().trackId;
    for (const ClipKey& key : clipKeys) {
        if (key.trackId != trackId) {
            return std::nullopt;
        }
    }

    const muse::async::NotifyList<Clip> trackClips = prj->clipList(trackId);

    size_t selectedCount = 0;
    double begin = std::numeric_limits<double>::max();
    double end = std::numeric_limits<double>::lowest();
    for (const Clip& clip : trackClips) {
        if (muse::contains(clipKeys, clip.key)) {
            ++selectedCount;
            begin = std::min(begin, clip.startTime);
            end = std::max(end, clip.endTime);
        }
    }

    if (selectedCount != clipKeys.size()) {
        return std::nullopt;
    }

    for (const Clip& clip : trackClips) {
        if (!muse::contains(clipKeys, clip.key) && clip.startTime < end && clip.endTime > begin) {
            return std::nullopt;
        }
    }

    return ContiguousClipsSpan { trackId, begin, end };
}

bool TrackeditActionsController::rangeSelectionCoversMultipleClips() const
{
    ClipKeyList clips = selectionController()->clipsIntersectingRangeSelection();
    std::sort(clips.begin(), clips.end());

    for (size_t i = 1; i < clips.size(); ++i) {
        if (clips[i].trackId == clips[i - 1].trackId) {
            return true;
        }
    }

    return false;
}

void TrackeditActionsController::doGlobalDisjoin()
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        TrackIdList selectedTracks = selectionController()->selectedTracks();
        secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
        secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

        dispatcher()->dispatch(SPLIT_RANGE_SELECTION_AT_SILENCES,
                               ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime, selectedEndTime));
        return;
    }

    ClipKeyList selectedClips = clipsForInteraction();
    if (selectedClips.empty()) {
        return;
    }
    dispatcher()->dispatch(SPLIT_CLIPS_AT_SILENCES, ActionData::make_arg1<ClipKeyList>(selectedClips));
}

void TrackeditActionsController::doGlobalDuplicate()
{
    const auto selectedTracks = selectionController()->selectedTracks();

    if (!selectedTracks.empty()) {
        const auto selectedClips = clipsForInteraction();
        if (selectedClips.empty()) {
            secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
            secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

            //If no range is selected, duplicate all content of the selected tracks
            //Otherwise, duplicate only the selected range
            (selectedStartTime == selectedEndTime)
            ? dispatcher()->dispatch(TRACK_DUPLICATE_CODE, ActionData::make_arg1<TrackIdList>(selectedTracks))
            : dispatcher()->dispatch(DUPLICATE_RANGE_SELECTION_CODE,
                                     ActionData::make_arg3<TrackIdList, secs_t, secs_t>(selectedTracks, selectedStartTime,
                                                                                        selectedEndTime));
        } else {
            dispatcher()->dispatch(DUPLICATE_CLIPS_CODE, ActionData::make_arg1<ClipKeyList>(selectedClips));
        }
    }
}

void TrackeditActionsController::multiClipDelete(const ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    ClipKeyList selectedClips = clipsForInteraction();
    if (selectedClips.empty()) {
        return;
    }

    selectionController()->resetSelectedClips();

    trackeditInteraction()->removeClips(selectedClips, moveClips);
}

void TrackeditActionsController::labelDeleteMulti(const ActionData& args)
{
    bool moveLabels = false;

    if (args.count() >= 1) {
        moveLabels = args.arg<bool>(0);
    }

    LabelKeyList selectedLabelKeys = labelsForInteraction();
    if (selectedLabelKeys.empty()) {
        return;
    }

    selectionController()->resetSelectedLabels();

    trackeditInteraction()->removeLabels(selectedLabelKeys, moveLabels);
}

void TrackeditActionsController::labelCutMulti(const muse::actions::ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    auto selectedLabelKeys = labelsForInteraction();
    if (selectedLabelKeys.empty()) {
        return;
    }

    trackeditInteraction()->clearClipboard();
    labelCopyMulti();
    selectionController()->resetSelectedLabels();

    trackeditInteraction()->removeLabels(selectedLabelKeys, moveClips);
}

void TrackeditActionsController::labelCopyMulti()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedLabels = labelsForInteraction();
    auto tracks = project->trackeditProject()->trackList();

    trackeditInteraction()->clearClipboard();

    secs_t offset = 0.0;
    std::optional<secs_t> leftmostItemStartTime = selectionController()->leftMostSelectedItemStartTime();
    if (leftmostItemStartTime.has_value()) {
        offset = -leftmostItemStartTime.value();
    }

    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        LabelKeyList selectedTrackLabels;
        for (const auto& label : selectedLabels) {
            if (label.trackId == track.id) {
                selectedTrackLabels.push_back(label);
            }
        }

        trackeditInteraction()->copyNonContinuousTrackDataIntoClipboard(track.id, selectedTrackLabels, offset);
    }
}

void TrackeditActionsController::multiClipCut(const ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    auto selectedClips = clipsForInteraction();
    if (selectedClips.empty()) {
        return;
    }

    trackeditInteraction()->clearClipboard();
    multiClipCopy();
    selectionController()->resetSelectedClips();

    trackeditInteraction()->removeClips(selectedClips, moveClips);
}

void TrackeditActionsController::rangeSelectionCut(const ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->cutItemDataIntoClipboard(selectedTracks, selectedStartTime, selectedEndTime, moveClips,
                                                     true /*isRangeSelection*/);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::multiClipCopy()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedClips = clipsForInteraction();
    auto selectedLabels = labelsForInteraction();
    auto tracks = project->trackeditProject()->trackList();

    trackeditInteraction()->clearClipboard();

    secs_t offset = 0.0;
    std::optional<secs_t> leftmostItemStartTime = selectionController()->leftMostSelectedItemStartTime();
    if (leftmostItemStartTime.has_value()) {
        offset = -leftmostItemStartTime.value();
    }

    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        ClipKeyList selectedTrackClips;
        for (const auto& clip : selectedClips) {
            if (clip.trackId == track.id) {
                selectedTrackClips.push_back(clip);
            }
        }

        trackeditInteraction()->copyNonContinuousTrackDataIntoClipboard(track.id, selectedTrackClips, offset);
    }
}

void TrackeditActionsController::rangeSelectionCopy()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    secs_t selectedStartTime = selectionController()->dataSelectedStartTime();
    secs_t selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    trackeditInteraction()->clearClipboard();

    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        trackeditInteraction()->copyContinuousTrackDataIntoClipboard(track.id, selectedStartTime, selectedEndTime);
    }
}

void TrackeditActionsController::rangeSelectionDelete(const ActionData& args)
{
    bool moveClips = false;

    if (args.count() >= 1) {
        moveClips = args.arg<bool>(0);
    }

    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();

    if (selectedTracks.empty()) {
        return;
    }

    trackeditInteraction()->removeTracksData(selectedTracks, selectedStartTime, selectedEndTime, moveClips);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::pasteDefault()
{
    switch (configuration()->pasteBehavior()) {
    case PasteBehavior::PasteOverlap:
        dispatcher()->dispatch(TRACKEDIT_PASTE_OVERLAP_CODE);
        break;
    case PasteBehavior::PasteInsert:
        switch (configuration()->pasteInsertBehavior()) {
        case PasteInsertBehavior::PasteInsert:
            dispatcher()->dispatch(TRACKEDIT_PASTE_INSERT_CODE);
            break;
        case PasteInsertBehavior::PasteInsertRipple:
            dispatcher()->dispatch(TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_CODE);
            break;
        }
        break;
    }
}

muse::Ret TrackeditActionsController::pasteOverlap()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    const double selectedStartTime = playbackState()->playbackPosition();

    if (selectedStartTime < 0) {
        return make_ret(Ret::Code::NotSupported);
    }

    const Ret ret = trackeditInteraction()->pasteFromClipboard(selectedStartTime, false);
    if (!ret && !ret.text().empty()) {
        interactive()->error(muse::trc("trackedit", "Paste error"), ret.text());
    }
    return ret;
}

muse::Ret TrackeditActionsController::pasteInsert()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    const double selectedStartTime = playbackState()->playbackPosition();

    if (selectedStartTime < 0) {
        return make_ret(Ret::Code::NotSupported);
    }

    const Ret ret = trackeditInteraction()->pasteFromClipboard(selectedStartTime, true);
    if (!ret && !ret.text().empty()) {
        interactive()->error(muse::trc("trackedit", "Paste error"), ret.text());
    }
    return ret;
}

muse::Ret TrackeditActionsController::pasteInsertRipple()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto tracks = project->trackeditProject()->trackList();
    const double selectedStartTime = playbackState()->playbackPosition();

    if (selectedStartTime < 0) {
        return make_ret(Ret::Code::NotSupported);
    }

    const Ret ret = trackeditInteraction()->pasteFromClipboard(selectedStartTime, false, true);
    if (!ret && !ret.text().empty()) {
        interactive()->error(muse::trc("trackedit", "Paste error"), ret.text());
    }
    return ret;
}

void TrackeditActionsController::trackSplit(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    TrackId trackIdToSplit = args.arg<TrackId>(0);
    if (trackIdToSplit == -1) {
        return;
    }

    const secs_t playbackPosition = playbackState()->playbackPosition();

    dispatcher()->dispatch(TRACK_SPLIT_AT, ActionData::make_arg2<TrackIdList, secs_t>({ trackIdToSplit }, playbackPosition));
}

muse::Ret TrackeditActionsController::tracksSplitAt(const Params& params)
{
    if (!params.contains(TRACKEDIT_TRACK_IDS_PARAM) || !params.contains(TRACKEDIT_PIVOTS_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    const auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return make_ret(Ret::Code::NotSupported);
    }

    TrackIdList tracksIds;
    for (const Val& trackId : params.at(TRACKEDIT_TRACK_IDS_PARAM).toList()) {
        tracksIds.push_back(trackId.toInt64());
    }

    muse::remove_if(tracksIds, [&prj](const TrackId& trackId) {
        const std::optional<Track> track = prj->track(trackId);
        return !track.has_value() || track->type == TrackType::Label;
    });

    if (tracksIds.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    std::vector<secs_t> pivots;
    for (const Val& pivot : params.at(TRACKEDIT_PIVOTS_PARAM).toList()) {
        pivots.push_back(pivot.toDouble());
    }

    return Ret(trackeditInteraction()->splitTracksAt(tracksIds, pivots));
}

void TrackeditActionsController::splitClipsAtSilences(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKeyList clipKeyList = args.arg<ClipKeyList>(0);
    if (clipKeyList.empty()) {
        return;
    }

    trackeditInteraction()->splitClipsAtSilences(clipKeyList);
}

void TrackeditActionsController::splitRangeSelectionAtSilences(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->splitRangeSelectionAtSilences(tracksIds, begin, end);
}

void TrackeditActionsController::splitRangeSelectionIntoNewTracks(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->splitRangeSelectionIntoNewTracks(tracksIds, begin, end);
}

void TrackeditActionsController::splitClipsIntoNewTracks(const ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKeyList clipKeyList = args.arg<ClipKeyList>(0);
    if (clipKeyList.empty()) {
        return;
    }

    trackeditInteraction()->splitClipsIntoNewTracks(clipKeyList);
}

void TrackeditActionsController::mergeSelectedOnTrack(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->mergeSelectedOnTracks(tracksIds, begin, end);
}

void TrackeditActionsController::duplicateSelected(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->duplicateSelectedOnTracks(tracksIds, begin, end);
}

void TrackeditActionsController::duplicateClips(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 1) {
        return;
    }

    ClipKeyList clipKeyList = args.arg<ClipKeyList>(0);
    trackeditInteraction()->duplicateClips(clipKeyList);
}

void TrackeditActionsController::splitCutSelected(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->clearClipboard();
    trackeditInteraction()->splitCutSelectedOnTracks(tracksIds, begin, end);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::splitDeleteSelected(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() == 3) {
        return;
    }

    TrackIdList tracksIds = args.arg<TrackIdList>(0);
    if (tracksIds.empty()) {
        return;
    }

    secs_t begin = args.arg<secs_t>(1);
    secs_t end = args.arg<secs_t>(2);

    trackeditInteraction()->splitDeleteSelectedOnTracks(tracksIds, begin, end);

    selectionController()->resetDataSelection();
}

void TrackeditActionsController::deleteTracks(const muse::actions::ActionData&)
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return;
    }

    trackeditInteraction()->deleteTracks(trackIds);
}

muse::Ret TrackeditActionsController::duplicateTracks()
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    return Ret(trackeditInteraction()->duplicateTracks(trackIds));
}

muse::Ret TrackeditActionsController::moveTracksUp()
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Up);
    return make_ok();
}

muse::Ret TrackeditActionsController::moveTracksDown()
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Down);
    return make_ok();
}

muse::Ret TrackeditActionsController::moveTracksToTop()
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Top);
    return make_ok();
}

muse::Ret TrackeditActionsController::moveTracksToBottom()
{
    TrackIdList trackIds = selectionController()->selectedTracks();

    if (trackIds.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    trackeditInteraction()->moveTracks(trackIds, TrackMoveDirection::Bottom);
    return make_ok();
}

muse::Ret TrackeditActionsController::swapStereoChannels()
{
    const TrackIdList trackIds = selectionController()->selectedTracks();
    if (trackIds.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    return Ret(trackeditInteraction()->swapStereoChannels(trackIds));
}

muse::Ret TrackeditActionsController::splitStereoToLR()
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported);
    }

    const TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    auto tracks = project->trackeditProject()->trackList();
    std::vector<TrackId> tracksIdsToSplit;
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        tracksIdsToSplit.push_back(track.id);
    }

    return Ret(trackeditInteraction()->splitStereoTracksToLRMono(tracksIdsToSplit));
}

muse::Ret TrackeditActionsController::splitStereoToCenter()
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported);
    }

    const TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    auto tracks = project->trackeditProject()->trackList();
    std::vector<TrackId> tracksIdsToSplit;
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        tracksIdsToSplit.push_back(track.id);
    }

    return Ret(trackeditInteraction()->splitStereoTracksToCenterMono(tracksIdsToSplit));
}

muse::Ret TrackeditActionsController::trimAudioOutsideSelection()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    if (selectedStartTime >= selectedEndTime) {
        return make_ret(Ret::Code::NotSupported);
    }

    std::vector<TrackId> tracksIdsToTrim;
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        tracksIdsToTrim.push_back(track.id);
    }

    if (tracksIdsToTrim.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    return Ret(trackeditInteraction()->trimTracksData(tracksIdsToTrim, selectedStartTime, selectedEndTime));
}

muse::Ret TrackeditActionsController::doGlobalSilence()
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        return silenceAudioSelection();
    }

    ClipKeyList selectedClips = selectionController()->selectedClips();
    if (selectedClips.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    return Ret(trackeditInteraction()->silenceClips(selectedClips));
}

muse::Ret TrackeditActionsController::silenceAudioSelection()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    auto selectedTracks = selectionController()->selectedTracks();
    auto selectedStartTime = selectionController()->dataSelectedStartTime();
    auto selectedEndTime = selectionController()->dataSelectedEndTime();
    auto tracks = project->trackeditProject()->trackList();

    std::vector<TrackId> tracksIdsToSilence;
    for (const auto& track : tracks) {
        if (std::find(selectedTracks.begin(), selectedTracks.end(), track.id) == selectedTracks.end()) {
            continue;
        }

        tracksIdsToSilence.push_back(track.id);
    }

    if (tracksIdsToSilence.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    return Ret(trackeditInteraction()->silenceTracksData(tracksIdsToSilence, selectedStartTime, selectedEndTime));
}

muse::Ret TrackeditActionsController::openClipPitchAndSpeed()
{
    auto selectedClips = clipsForInteraction();

    if (selectedClips.empty() || selectedClips.size() > 1) {
        return make_ret(Ret::Code::NotSupported);
    }

    dispatcher()->dispatch("clip-pitch-speed", ActionData::make_arg1<trackedit::ClipKey>(selectedClips.front()));
    return make_ok();
}

muse::Ret TrackeditActionsController::groupClips()
{
    const auto selectedClips = clipsForInteraction();

    trackeditInteraction()->groupClips(selectedClips);

    notifyActionEnabledChanged(GROUP_CLIPS_CODE);
    notifyActionEnabledChanged(UNGROUP_CLIPS_CODE);
    return make_ok();
}

muse::Ret TrackeditActionsController::ungroupClips()
{
    trackeditInteraction()->ungroupClips(clipsForInteraction());

    notifyActionEnabledChanged(GROUP_CLIPS_CODE);
    notifyActionEnabledChanged(UNGROUP_CLIPS_CODE);
    return make_ok();
}

muse::Ret TrackeditActionsController::selectNone()
{
    selectionController()->resetTimeSelection();
    selectionController()->resetDataSelection();
    selectionController()->resetSelectedClips();
    selectionController()->resetSelectedTracks();
    return make_ok();
}

muse::Ret TrackeditActionsController::selectAllTracks()
{
    auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return make_ret(Ret::Code::NotSupported);
    }

    selectionController()->setSelectedTracks(prj->trackIdList());
    return make_ok();
}

muse::Ret TrackeditActionsController::selectLeftOfPlaybackPos()
{
    RetVal<Val> rv = interactive()->openSync(makePlaybackPosUri());
    if (!rv.ret) {
        return rv.ret;
    }

    secs_t playbackTime = playbackState()->playbackPosition();

    if (muse::RealIsEqualOrMore(rv.val.toDouble(), playbackTime)) {
        return make_ret(Ret::Code::NotSupported);
    }

    selectionController()->setDataSelectedStartTime(rv.val.toDouble(), true);
    selectionController()->setDataSelectedEndTime(playbackTime, true);
    return make_ok();
}

muse::Ret TrackeditActionsController::selectRightOfPlaybackPos()
{
    RetVal<Val> rv = interactive()->openSync(makePlaybackPosUri());
    if (!rv.ret) {
        return rv.ret;
    }

    secs_t playbackTime = playbackState()->playbackPosition();
    secs_t rightOfPlaybackValue = rv.val.toDouble();
    if (muse::RealIsEqualOrLess(rightOfPlaybackValue, playbackTime)) {
        return make_ret(Ret::Code::NotSupported);
    }

    //! NOTE: clamp to 2x size of project
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return make_ret(Ret::Code::NotSupported);
    }

    secs_t maxTime = prj->trackeditProject()->totalTime().to_double() * 2;
    rightOfPlaybackValue = std::min(rightOfPlaybackValue, maxTime);

    selectionController()->setDataSelectedStartTime(playbackTime, true);
    selectionController()->setDataSelectedEndTime(rightOfPlaybackValue, true);
    return make_ok();
}

muse::Ret TrackeditActionsController::selectTrackStartToCursor()
{
    std::optional<secs_t> leftmostItemStartTime = selectionController()->selectedTracksStartTime();
    if (leftmostItemStartTime.has_value()) {
        selectionController()->setDataSelectedStartTime(leftmostItemStartTime.value(), true);
    } else {
        selectionController()->setDataSelectedStartTime(0.0, true);
    }

    selectionController()->setDataSelectedEndTime(playbackState()->playbackPosition(), true);
    return make_ok();
}

muse::Ret TrackeditActionsController::selectCursorToTrackEnd()
{
    std::optional<secs_t> rightmostItemEndTime = selectionController()->selectedTracksEndTime();
    if (!rightmostItemEndTime.has_value()) {
        //! NOTE: AU3 behavior
        return selectTrackStartToCursor();
    }

    selectionController()->setDataSelectedStartTime(playbackState()->playbackPosition(), true);
    selectionController()->setDataSelectedEndTime(rightmostItemEndTime.value(), true);
    return make_ok();
}

muse::Ret TrackeditActionsController::selectTrackStartToEnd()
{
    std::optional<secs_t> leftmostItemStartTime = selectionController()->selectedTracksStartTime();
    std::optional<secs_t> rightmostItemEndTime = selectionController()->selectedTracksEndTime();

    if (!leftmostItemStartTime.has_value() || !rightmostItemEndTime.has_value()) {
        return make_ret(Ret::Code::NotSupported);
    }

    selectionController()->setDataSelectedStartTime(leftmostItemStartTime.value(), true);
    selectionController()->setDataSelectedEndTime(rightmostItemEndTime.value(), true);
    return make_ok();
}

muse::Ret TrackeditActionsController::setSelection(const Params& params)
{
    if (!params.contains(TRACKEDIT_START_PARAM) || !params.contains(TRACKEDIT_END_PARAM)) {
        LOGE() << "set-selection missing required 'start' or 'end' param";
        return make_ret(Ret::Code::BadArgs);
    }

    const double startTime = params.at(TRACKEDIT_START_PARAM).toDouble();
    const double endTime = params.at(TRACKEDIT_END_PARAM).toDouble();
    if (startTime < 0.0 || endTime < startTime) {
        LOGE() << "set-selection invalid range: start=" << startTime << " end=" << endTime;
        return make_ret(Ret::Code::BadArgs);
    }

    selectionController()->setDataSelectedStartTime(startTime, true);
    selectionController()->setDataSelectedEndTime(endTime, true);
    return make_ok();
}

muse::Ret TrackeditActionsController::selectTrackByIndex(const Params& params)
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return make_ret(Ret::Code::NotSupported);
    }

    if (!params.contains(TRACKEDIT_TRACK_INDEX_PARAM)) {
        LOGE() << "select-track missing required 'trackIndex' param";
        return make_ret(Ret::Code::BadArgs);
    }

    const int trackIndex = params.at(TRACKEDIT_TRACK_INDEX_PARAM).toInt();
    auto ids = prj->trackIdList();
    if (trackIndex < 0 || trackIndex >= static_cast<int>(ids.size())) {
        return make_ret(Ret::Code::BadArgs);
    }

    selectionController()->setSelectedTracks({ ids[trackIndex] }, true);
    return make_ok();
}

muse::Ret TrackeditActionsController::moveCursorToClosestZeroCrossing()
{
    secs_t zeroCrossing = trackeditInteraction()->nearestZeroCrossing(playbackState()->playbackPosition());
    zeroCrossing = std::max(zeroCrossing.to_double(), 0.0);

    muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
    q.addParam("seekTime", muse::Val(zeroCrossing));
    q.addParam("triggerPlay", muse::Val(false));
    dispatcher()->dispatch(q);
    return make_ok();
}

muse::Ret TrackeditActionsController::setClipColor(const Params& params, const std::function<void()>& onSet)
{
    auto selectedClips = clipsForInteraction();
    if (selectedClips.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    const trackedit::ClipColorIndex colorIndex = params.at(TRACKEDIT_COLOR_INDEX_PARAM, Val(trackedit::CLIP_COLOR_INDEX_NONE)).toInt();

    auto clipKey = selectedClips.front();
    const bool changed = trackeditInteraction()->changeClipColor(clipKey, colorIndex);
    onSet();
    return Ret(changed);
}

muse::Ret TrackeditActionsController::setTrackColor(const Params& params)
{
    const auto tracks = selectionController()->selectedTracks();
    if (tracks.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    const trackedit::ClipColorIndex colorIndex = params.at(TRACKEDIT_COLOR_INDEX_PARAM, Val(trackedit::CLIP_COLOR_INDEX_NONE)).toInt();

    const bool changed = trackeditInteraction()->changeTracksColor(tracks, colorIndex);
    notifyActionCheckedChanged(legacyActionCode(TRACK_CHANGE_COLOR_QUERY, params));
    return Ret(changed);
}

muse::Ret TrackeditActionsController::setTrackFormat(const Params& params)
{
    const auto tracks = selectionController()->selectedTracks();
    if (tracks.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    if (!params.contains(TRACKEDIT_FORMAT_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    const int format = params.at(TRACKEDIT_FORMAT_PARAM).toInt();
    const bool changed = trackeditInteraction()->changeTracksFormat(tracks, static_cast<TrackFormat>(format));
    if (changed) {
        notifyActionCheckedChanged(legacyActionCode(TRACK_CHANGE_FORMAT_QUERY, params));
    }
    return Ret(changed);
}

muse::Ret TrackeditActionsController::setCustomTrackRate()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported);
    }

    const TrackIdList tracks = selectionController()->selectedTracks();
    if (tracks.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    const TrackId focusedTrackId = trackNavigationController()->focusedTrack();
    const std::optional<Track> focused = project->trackeditProject()->track(focusedTrackId);
    if (!focused) {
        return make_ret(Ret::Code::NotSupported);
    }

    muse::UriQuery customRateUri("audacity://trackedit/custom_rate");
    customRateUri.addParam("title", muse::Val(muse::trc("trackedit", "Set rate")));
    customRateUri.addParam("rate", muse::Val(static_cast<int>(focused.value().rate)));

    RetVal<Val> rv = interactive()->openSync(customRateUri);
    if (rv.ret.code() != static_cast<int>(Ret::Code::Ok)) {
        return rv.ret;
    }

    const auto customRate = rv.val.toInt();
    if (customRate <= 0) {
        return make_ret(Ret::Code::BadArgs);
    }

    return Ret(trackeditInteraction()->changeTracksRate(tracks, customRate));
}

muse::Ret TrackeditActionsController::setTrackRate(const Params& params)
{
    const auto tracks = selectionController()->selectedTracks();
    if (tracks.empty()) {
        return make_ret(Ret::Code::NotSupported);
    }

    if (!params.contains(TRACKEDIT_RATE_PARAM)) {
        return make_ret(Ret::Code::BadArgs);
    }

    const int rate = params.at(TRACKEDIT_RATE_PARAM).toInt();
    const bool changed = trackeditInteraction()->changeTracksRate(tracks, rate);
    if (changed) {
        notifyActionCheckedChanged(legacyActionCode(TRACK_CHANGE_RATE_QUERY, params));
    }
    return Ret(changed);
}

muse::Ret TrackeditActionsController::toggleGlobalSpectrogramView()
{
    const auto project = globalContext()->currentProject();
    IF_ASSERT_FAILED(project) {
        return make_ret(Ret::Code::NotSupported);
    }

    const auto viewState = project->viewState();
    IF_ASSERT_FAILED(viewState) {
        return make_ret(Ret::Code::NotSupported);
    }

    const bool enablingGlobalSpectrogram = !viewState->globalSpectrogramToggleIsOn();
    if (enablingGlobalSpectrogram && viewState->clipGainAutomationEnabled().val) {
        viewState->setClipGainAutomationEnabled(false);
    }

    viewState->toggleGlobalSpectrogramView();
    return make_ok();
}

muse::Ret TrackeditActionsController::changeTrackView(const TrackId& trackId, TrackViewType trackView)
{
    const auto prj = globalContext()->currentProject();
    IF_ASSERT_FAILED(prj) {
        return make_ret(Ret::Code::NotSupported);
    }
    prj->viewState()->setTrackViewType(trackId, trackView);
    switch (trackView) {
    case TrackViewType::Waveform:
        notifyActionCheckedChanged(SET_TRACK_VIEW_WAVEFORM.toString());
        break;
    case TrackViewType::Spectrogram:
        notifyActionCheckedChanged(SET_TRACK_VIEW_SPECTROGRAM.toString());
        break;
    case TrackViewType::WaveformAndSpectrogram:
        notifyActionCheckedChanged(SET_TRACK_VIEW_MULTI.toString());
        break;
    default:
        assert(false);
    }
    return make_ok();
}

muse::Ret TrackeditActionsController::addLabel()
{
    const muse::RetVal<LabelKey> newLabel = trackeditInteraction()->addLabelToSelection();
    if (!newLabel.ret) {
        return newLabel.ret;
    }

    tracksViewRequestsService()->requestLabelTitleEdit(newLabel.val);
    return make_ok();
}

muse::Ret TrackeditActionsController::renameSelectedItem()
{
    const LabelKeyList labels = labelsForInteraction();
    if (labels.size() != 1 || !labels.front().isValid()) {
        return make_ret(Ret::Code::NotSupported);
    }

    tracksViewRequestsService()->requestLabelTitleEdit(labels.front());
    return make_ok();
}

muse::Ret TrackeditActionsController::makeStereoTrack()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported);
    }

    const auto selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.size() != 1) {
        return make_ret(Ret::Code::NotSupported);
    }

    const std::optional<Track> selectedTrack = project->trackeditProject()->track(selectionController()->selectedTracks().front());
    if (!selectedTrack || selectedTrack->type != TrackType::Mono) {
        return make_ret(Ret::Code::NotSupported);
    }

    const TrackList tracks = project->trackeditProject()->trackList();
    const auto it = std::find_if(tracks.begin(), tracks.end(),
                                 [&selectedTrack](const Track& track) { return track.id == selectedTrack->id; });

    if (it == tracks.end()) {
        return make_ret(Ret::Code::NotSupported);
    }

    const auto nextTrack = std::next(it);
    if ((nextTrack == tracks.end()) || nextTrack->type != TrackType::Mono) {
        return make_ret(Ret::Code::NotSupported);
    }

    return Ret(trackeditInteraction()->makeStereoTrack(selectedTrack->id, nextTrack->id));
}

muse::Ret TrackeditActionsController::resampleTracks()
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return make_ret(Ret::Code::NotSupported);
    }

    const TrackIdList selectedTracks = selectionController()->selectedTracks();
    if (selectedTracks.size() == 0) {
        return make_ret(Ret::Code::NotSupported);
    }

    const TrackId focusedTrackId = trackNavigationController()->focusedTrack();
    const std::optional<Track> focused = project->trackeditProject()->track(focusedTrackId);
    if (!focused) {
        return make_ret(Ret::Code::NotSupported);
    }

    muse::UriQuery resampleUri("audacity://trackedit/custom_rate");

    muse::ValList availableSampleRates;
    for (const auto& rate : audioDriverController()->sampleRates()) {
        availableSampleRates.push_back(muse::Val(static_cast<int>(rate)));
    }
    resampleUri.addParam("availableRates", muse::Val(availableSampleRates));
    resampleUri.addParam("title", muse::Val(muse::trc("trackedit", "Resample track")));
    resampleUri.addParam("rate", muse::Val(static_cast<int>(focused.value().rate)));

    const RetVal<Val> rv = interactive()->openSync(resampleUri);
    if (rv.ret.code() != static_cast<int>(Ret::Code::Ok)) {
        return rv.ret;
    }

    const int customRate = rv.val.toInt();
    if (customRate <= 0) {
        return make_ret(Ret::Code::BadArgs);
    }

    return Ret(trackeditInteraction()->resampleTracks(selectedTracks, customRate));
}

bool TrackeditActionsController::actionChecked(const ActionCode&) const
{
    //! TODO AU4
    return false;
}

Channel<ActionCode> TrackeditActionsController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool TrackeditActionsController::canReceiveAction(const ActionCode& actionCode) const
{
    if (globalContext()->currentProject() == nullptr) {
        return false;
    } else if (globalContext()->isRecording() && muse::contains(actionsDisabledDuringRecording, actionCode)) {
        return false;
    } else if (actionCode == TRACKEDIT_UNDO) {
        return trackeditInteraction()->canUndo();
    } else if (actionCode == TRACKEDIT_REDO) {
        return trackeditInteraction()->canRedo();
    } else if (actionCode == GROUP_CLIPS_CODE) {
        return clipsForInteraction().size() > 1 && !selectionController()->isSelectionGrouped();
    } else if (actionCode == UNGROUP_CLIPS_CODE) {
        return clipsForInteraction().size() > 1 && selectionController()->selectionContainsGroup();
    } else if (actionCode == JOIN_CODE) {
        if (!selectionController()->timeSelectionIsEmpty()) {
            return rangeSelectionCoversMultipleClips();
        }
        return contiguousSelectedClipsSpan().has_value();
    } else if (actionCode == SILENCE_AUDIO_SELECTION) {
        return canSilenceAudio();
    } else if (actionCode == RENAME_ITEM_CODE) {
        return clipsForInteraction().size() + labelsForInteraction().size() == 1;
    }

    return true;
}

bool TrackeditActionsController::canSilenceAudio() const
{
    if (!selectionController()->timeSelectionIsEmpty()) {
        return !trackeditInteraction()->tracksDataIsSilent(selectionController()->selectedTracks(),
                                                           selectionController()->dataSelectedStartTime(),
                                                           selectionController()->dataSelectedEndTime());
    }

    for (const auto& clipKey : selectionController()->selectedClips()) {
        const secs_t begin = trackeditInteraction()->clipStartTime(clipKey);
        const secs_t end = trackeditInteraction()->clipEndTime(clipKey);
        if (!trackeditInteraction()->tracksDataIsSilent({ clipKey.trackId }, begin, end)) {
            return true;
        }
    }

    return false;
}

void TrackeditActionsController::moveFocusedItem(secs_t timePositionOffset, int trackPositionOffset)
{
    tracksViewRequestsService()->requestItemMove(timePositionOffset, trackPositionOffset);
}

muse::Ret TrackeditActionsController::extendFocusedItemBoundaryLeft()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (!labelsForInteraction().empty()) {
        trackeditInteraction()->stretchLabelsLeft(labelsForInteraction(), -stepSize, completed);
    } else if (!clipsForInteraction().empty()) {
        double minClipDuration = MIN_CLIP_WIDTH / zoomLevel();
        trackeditInteraction()->trimClipsLeft(clipsForInteraction(), -stepSize, minClipDuration, completed, UndoPushType::CONSOLIDATE);
    } else {
        dispatcher()->dispatch("sel-ext-left");
    }
    return make_ok();
}

muse::Ret TrackeditActionsController::extendFocusedItemBoundaryRight()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (!labelsForInteraction().empty()) {
        trackeditInteraction()->stretchLabelsRight(labelsForInteraction(), stepSize, completed);
    } else if (!clipsForInteraction().empty()) {
        double minClipDuration = MIN_CLIP_WIDTH / zoomLevel();
        trackeditInteraction()->trimClipsRight(clipsForInteraction(), -stepSize, minClipDuration, completed, UndoPushType::CONSOLIDATE);
    } else {
        dispatcher()->dispatch("sel-ext-right");
    }
    return make_ok();
}

muse::Ret TrackeditActionsController::reduceFocusedItemBoundaryLeft()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (!labelsForInteraction().empty()) {
        trackeditInteraction()->stretchLabelsRight(labelsForInteraction(), -stepSize, completed);
    } else if (!clipsForInteraction().empty()) {
        double minClipDuration = MIN_CLIP_WIDTH / zoomLevel();
        trackeditInteraction()->trimClipsRight(clipsForInteraction(), stepSize, minClipDuration, completed, UndoPushType::CONSOLIDATE);
    } else {
        dispatcher()->dispatch("sel-cntr-right");
    }
    return make_ok();
}

muse::Ret TrackeditActionsController::reduceFocusedItemBoundaryRight()
{
    const double stepSize = calculateStepSize();
    static bool completed = true;

    if (!labelsForInteraction().empty()) {
        trackeditInteraction()->stretchLabelsLeft(labelsForInteraction(), stepSize, completed);
    } else if (!clipsForInteraction().empty()) {
        double minClipDuration = MIN_CLIP_WIDTH / zoomLevel();
        trackeditInteraction()->trimClipsLeft(clipsForInteraction(), stepSize, minClipDuration, completed, UndoPushType::CONSOLIDATE);
    } else {
        dispatcher()->dispatch("sel-cntr-left");
    }
    return make_ok();
}

double TrackeditActionsController::zoomLevel() const
{
    auto project = globalContext()->currentProject();
    if (!project) {
        return 1.0;
    }

    auto viewState = project->viewState();
    if (!viewState) {
        return 1.0;
    }

    return viewState->zoomState().zoom;
}

double TrackeditActionsController::calculateStepSize() const
{
    double zoom = zoomLevel();
    if (zoom <= 0.0) {
        return 1.0;
    }
    return 10.0 / zoom;
}

au::trackedit::TrackId TrackeditActionsController::resolvePreviousTrackIdForMove(const TrackId& trackId) const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return INVALID_TRACK;
    }

    std::vector<Track> trackList = prj->trackList();
    TrackId lastLabelTrackId = INVALID_TRACK;
    TrackId lastWaveTrackId = INVALID_TRACK;

    for (const Track& track : trackList) {
        bool isLabelTrack = track.type == TrackType::Label;

        if (track.id == trackId) {
            return track.type == TrackType::Label ? lastLabelTrackId : lastWaveTrackId;
        }

        if (isLabelTrack) {
            lastLabelTrackId = track.id;
        } else {
            lastWaveTrackId = track.id;
        }
    }

    return INVALID_TRACK;
}

au::trackedit::TrackId TrackeditActionsController::resolveNextTrackIdForMove(const TrackId& trackId) const
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return INVALID_TRACK;
    }

    std::vector<Track> trackList = prj->trackList();

    for (size_t i = 0; i < trackList.size(); ++i) {
        const Track& track = trackList[i];

        if (track.id != trackId) {
            continue;
        }

        bool isLabelTrack = track.type == TrackType::Label;

        while (++i < trackList.size()) {
            const Track& nextTrack = trackList[i];
            if (isLabelTrack && nextTrack.type == TrackType::Label) {
                return nextTrack.id;
            } else if (!isLabelTrack && nextTrack.type != TrackType::Label) {
                return nextTrack.id;
            }
        }
    }

    return INVALID_TRACK;
}

au::context::IPlaybackStatePtr TrackeditActionsController::playbackState() const
{
    return globalContext()->playbackState();
}
