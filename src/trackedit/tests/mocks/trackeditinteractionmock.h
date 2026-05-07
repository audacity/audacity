/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "trackedit/itrackeditinteraction.h"

namespace au::trackedit {
class TrackeditInteractionMock : public ITrackeditInteraction
{
public:
    MOCK_METHOD(secs_t, clipStartTime, (const ClipKey&), (const, override));
    MOCK_METHOD(secs_t, clipEndTime, (const trackedit::ClipKey&), (const, override));

    MOCK_METHOD(bool, changeClipStartTime, (const ClipKey&, secs_t, bool), (override));
    MOCK_METHOD((muse::async::Channel<ClipKey, secs_t, bool>), clipStartTimeChanged, (), (const, override));

    MOCK_METHOD(bool, trimTracksData, (const std::vector<trackedit::TrackId>&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, silenceTracksData, (const std::vector<trackedit::TrackId>&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, changeTrackTitle, (const trackedit::TrackId, const muse::String&), (override));

    MOCK_METHOD(bool, changeClipTitle, (const ClipKey&, const muse::String&), (override));
    MOCK_METHOD(bool, changeClipPitch, (const ClipKey&, int), (override));
    MOCK_METHOD(bool, resetClipPitch, (const ClipKey&), (override));
    MOCK_METHOD(bool, changeClipSpeed, (const ClipKey&, double), (override));
    MOCK_METHOD(bool, resetClipSpeed, (const ClipKey&), (override));
    MOCK_METHOD(bool, changeClipColor, (const ClipKey&, ClipColorIndex), (override));
    MOCK_METHOD(bool, changeTracksColor, (const TrackIdList&, ClipColorIndex), (override));
    MOCK_METHOD(bool, changeClipOptimizeForVoice, (const ClipKey&, bool), (override));
    MOCK_METHOD(bool, renderClipPitchAndSpeed, (const ClipKey&), (override));
    MOCK_METHOD(bool, resetClipPitchAndSpeed, (const ClipKey&), (override));
    MOCK_METHOD(void, clearClipboard, (), (override));
    MOCK_METHOD(muse::Ret, pasteFromClipboard, (secs_t, bool, bool), (override));
    MOCK_METHOD(bool, cutClipIntoClipboard, (const ClipKey&), (override));
    MOCK_METHOD(bool, cutItemDataIntoClipboard, (const TrackIdList&, secs_t, secs_t, bool), (override));
    MOCK_METHOD(bool, copyClipIntoClipboard, (const ClipKey&), (override));
    MOCK_METHOD(bool, copyNonContinuousTrackDataIntoClipboard, (const TrackId, const TrackItemKeyList&, secs_t), (override));
    MOCK_METHOD(bool, copyContinuousTrackDataIntoClipboard, (const TrackId, secs_t, secs_t), (override));
    MOCK_METHOD(bool, removeClip, (const ClipKey&), (override));
    MOCK_METHOD(bool, removeClips, (const ClipKeyList&, bool), (override));
    MOCK_METHOD(bool, removeTracksData, (const TrackIdList&, secs_t, secs_t, bool), (override));
    MOCK_METHOD(muse::RetVal<ClipKeyList>, moveClips, (const ClipKeyList&, secs_t, int, bool, bool&), (override));
    MOCK_METHOD(bool, moveRangeSelection, (secs_t, bool), (override));
    MOCK_METHOD(void, cancelItemDragEdit, (), (override));
    MOCK_METHOD(bool, splitTracksAt, (const TrackIdList&, std::vector<secs_t>), (override));
    MOCK_METHOD(bool, splitClipsAtSilences, (const ClipKeyList&), (override));
    MOCK_METHOD(bool, splitRangeSelectionAtSilences, (const TrackIdList&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, splitRangeSelectionIntoNewTracks, (const TrackIdList&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, splitClipsIntoNewTracks, (const ClipKeyList&), (override));
    MOCK_METHOD(bool, mergeSelectedOnTracks, (const TrackIdList&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, duplicateSelectedOnTracks, (const TrackIdList&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, duplicateClip, (const ClipKey&), (override));
    MOCK_METHOD(bool, duplicateClips, (const ClipKeyList&), (override));
    MOCK_METHOD(bool, clipSplitCut, (const ClipKey&), (override));
    MOCK_METHOD(bool, clipSplitDelete, (const ClipKey&), (override));
    MOCK_METHOD(bool, splitCutSelectedOnTracks, (const TrackIdList, secs_t, secs_t), (override));
    MOCK_METHOD(bool, splitDeleteSelectedOnTracks, (const TrackIdList, secs_t, secs_t), (override));
    MOCK_METHOD(bool, trimClipsLeft, (const ClipKeyList&, secs_t, secs_t, bool, UndoPushType), (override));
    MOCK_METHOD(bool, trimClipsRight, (const ClipKeyList&, secs_t, secs_t, bool, UndoPushType), (override));

    MOCK_METHOD(bool, stretchClipsLeft, (const ClipKeyList&, secs_t, secs_t, bool, UndoPushType), (override));
    MOCK_METHOD(bool, stretchClipsRight, (const ClipKeyList&, secs_t, secs_t, bool, UndoPushType), (override));

    MOCK_METHOD(secs_t, clipDuration, (const ClipKey&), (const, override));
    MOCK_METHOD(double, nearestZeroCrossing, (double), (const, override));
    MOCK_METHOD(muse::Ret, makeRoomForClip, (const trackedit::ClipKey&), (override));

    MOCK_METHOD(bool, newMonoTrack, (), (override));
    MOCK_METHOD(bool, newStereoTrack, (), (override));
    MOCK_METHOD(muse::RetVal<TrackId>, newLabelTrack, (const muse::String&), (override));

    MOCK_METHOD(bool, deleteTracks, (const TrackIdList&), (override));
    MOCK_METHOD(bool, duplicateTracks, (const TrackIdList&), (override));
    MOCK_METHOD(void, moveTracks, (const TrackIdList&, TrackMoveDirection), (override));
    MOCK_METHOD(void, moveTracksTo, (const TrackIdList&, int), (override));
    MOCK_METHOD(ClipKeyList, clipsOnTrack, (const trackedit::TrackId), (override));

    MOCK_METHOD(bool, undo, (), (override));
    MOCK_METHOD(bool, canUndo, (), (override));
    MOCK_METHOD(bool, redo, (), (override));
    MOCK_METHOD(bool, canRedo, (), (override));
    MOCK_METHOD(bool, undoRedoToIndex, (size_t), (override));

    MOCK_METHOD(void, notifyAboutCancelDragEdit, (), (override));
    MOCK_METHOD(muse::async::Notification, cancelDragEditRequested, (), (const, override));

    MOCK_METHOD(bool, insertSilence, (const TrackIdList&, secs_t, secs_t, secs_t), (override));

    MOCK_METHOD(bool, toggleStretchToMatchProjectTempo, (const ClipKey&), (override));

    MOCK_METHOD(int64_t, clipGroupId, (const trackedit::ClipKey&), (const, override));
    MOCK_METHOD(void, setClipGroupId, (const trackedit::ClipKey&, int64_t), (override));
    MOCK_METHOD(void, groupClips, (const trackedit::ClipKeyList&), (override));
    MOCK_METHOD(void, ungroupClips, (const trackedit::ClipKeyList&), (override));
    MOCK_METHOD(ClipKeyList, clipsInGroup, (int64_t), (const, override));

    MOCK_METHOD(bool, changeTracksFormat, (const TrackIdList&, trackedit::TrackFormat), (override));
    MOCK_METHOD(bool, changeTracksRate, (const TrackIdList&, int), (override));

    MOCK_METHOD(bool, swapStereoChannels, (const TrackIdList&), (override));
    MOCK_METHOD(bool, splitStereoTracksToLRMono, (const TrackIdList&), (override));
    MOCK_METHOD(bool, splitStereoTracksToCenterMono, (const TrackIdList&), (override));
    MOCK_METHOD(bool, makeStereoTrack, (const TrackId, const TrackId), (override));
    MOCK_METHOD(bool, resampleTracks, (const TrackIdList&, int), (override));

    MOCK_METHOD(muse::RetVal<LabelKey>, addLabel, (const TrackId&), (override));
    MOCK_METHOD(bool, addLabelToSelection, (), (override));

    MOCK_METHOD(bool, changeLabelTitle, (const LabelKey&, const muse::String&), (override));
    MOCK_METHOD(bool, changeLabelLowFrequency, (const LabelKey&, double), (override));
    MOCK_METHOD(bool, changeLabelHighFrequency, (const LabelKey&, double), (override));

    MOCK_METHOD(bool, removeLabel, (const LabelKey&), (override));
    MOCK_METHOD(bool, removeLabels, (const LabelKeyList&, bool), (override));

    MOCK_METHOD(bool, cutLabel, (const LabelKey&), (override));
    MOCK_METHOD(bool, copyLabel, (const LabelKey&), (override));

    MOCK_METHOD(bool, moveLabels, (const LabelKeyList&, secs_t, bool), (override));
    MOCK_METHOD(muse::RetVal<LabelKeyList>, moveLabels, (const LabelKeyList&, secs_t, int, bool), (override));
    MOCK_METHOD(muse::RetVal<LabelKeyList>, moveLabelsToTrack, (const LabelKeyList&, const TrackId&, bool), (override));

    MOCK_METHOD(bool, stretchLabelLeft, (const LabelKey&, secs_t, bool), (override));
    MOCK_METHOD(bool, stretchLabelsLeft, (const LabelKeyList&, secs_t, bool), (override));

    MOCK_METHOD(bool, stretchLabelRight, (const LabelKey&, secs_t, bool), (override));
    MOCK_METHOD(bool, stretchLabelsRight, (const LabelKeyList&, secs_t, bool), (override));

    MOCK_METHOD(muse::Progress, progress, (), (const, override));
};
}
