/*
 * Audacity: A Digital Audio Editor
 */

#include "trackeditoperationcontroller.h"

#include "trackediterrors.h"

namespace au::trackedit {
TrackeditOperationController::TrackeditOperationController(const muse::modularity::ContextPtr& ctx,
                                                           std::unique_ptr<IUndoManager> undoManager)
    : muse::Contextable(ctx), m_undoManager{std::move(undoManager)} {}

secs_t TrackeditOperationController::clipStartTime(const ClipKey& clipKey) const
{
    return clipsInteraction()->clipStartTime(clipKey);
}

secs_t TrackeditOperationController::clipEndTime(const ClipKey& clipKey) const
{
    return clipsInteraction()->clipEndTime(clipKey);
}

bool TrackeditOperationController::changeClipStartTime(const ClipKey& clipKey, secs_t newStartTime, bool completed)
{
    return clipsInteraction()->changeClipStartTime(clipKey, newStartTime, completed);
}

muse::async::Channel<ClipKey, secs_t /*newStartTime*/, bool /*completed*/> TrackeditOperationController::clipStartTimeChanged() const
{
    return clipsInteraction()->clipStartTimeChanged();
}

bool TrackeditOperationController::trimTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->trimTracksData(tracksIds, begin, end)) {
        projectHistory()->pushHistoryState(
            //: History entry. %1 and %2 are positions in seconds
            muse::qtrc("trackedit", "Trim selected audio tracks from %1 seconds to %2 seconds")
            .arg(begin.to_double()).arg(end.to_double()).toStdString(),
            muse::trc("trackedit", "Trim Audio"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->silenceTracksData(tracksIds, begin, end)) {
        projectHistory()->pushHistoryState(
            //: History entry. %1 and %2 are positions in seconds
            muse::qtrc("trackedit", "Silenced selected tracks from %1 seconds to %2 seconds")
            .arg(begin.to_double()).arg(end.to_double()).toStdString(),
            muse::trc("trackedit", "Silence"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::silenceClips(const ClipKeyList& clipKeyList)
{
    bool anySilenced = false;
    for (const auto& clipKey : clipKeyList) {
        const secs_t begin = clipsInteraction()->clipStartTime(clipKey);
        const secs_t end = clipsInteraction()->clipEndTime(clipKey);
        if (tracksInteraction()->silenceTracksData({ clipKey.trackId }, begin, end)) {
            anySilenced = true;
        }
    }

    if (anySilenced) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Silenced selected clips"), muse::trc("trackedit", "Silence"));
    }
    return anySilenced;
}

bool TrackeditOperationController::tracksDataIsSilent(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) const
{
    return tracksInteraction()->tracksDataIsSilent(tracksIds, begin, end);
}

bool TrackeditOperationController::changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title)
{
    if (tracksInteraction()->changeTrackTitle(trackId, title)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Track Title"), muse::trc("trackedit", "Changed Track Title"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipTitle(const ClipKey& clipKey, const muse::String& newTitle)
{
    if (clipsInteraction()->changeClipTitle(clipKey, newTitle)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Clip Title"), muse::trc("trackedit", "Changed clip title"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipPitch(const ClipKey& clipKey, int pitch)
{
    if (clipsInteraction()->changeClipPitch(clipKey, pitch)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Pitch Shift"), muse::trc("trackedit", "Changed Pitch Shift"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::resetClipPitch(const ClipKey& clipKey)
{
    if (clipsInteraction()->resetClipPitch(clipKey)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Reset Clip Pitch"), muse::trc("trackedit", "Reset Clip Pitch"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipSpeed(const ClipKey& clipKey, double speed)
{
    if (clipsInteraction()->changeClipSpeed(clipKey, speed)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Changed Speed"), muse::trc("trackedit", "Changed Speed"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::resetClipSpeed(const ClipKey& clipKey)
{
    if (clipsInteraction()->resetClipSpeed(clipKey)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Reset Clip Speed"), muse::trc("trackedit", "Reset Clip Speed"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipColor(const ClipKey& clipKey, ClipColorIndex colorIndex)
{
    return clipsInteraction()->changeClipColor(clipKey, colorIndex);
}

bool TrackeditOperationController::changeTracksColor(const TrackIdList& tracksIds, ClipColorIndex colorIndex)
{
    if (tracksInteraction()->changeTracksColor(tracksIds, colorIndex)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Changed track color"), muse::trc("trackedit", "Changed track color"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize)
{
    return clipsInteraction()->changeClipOptimizeForVoice(clipKey, optimize);
}

bool TrackeditOperationController::resetClipPitchAndSpeed(const ClipKey& clipKey)
{
    const bool pitchOk = clipsInteraction()->resetClipPitch(clipKey);
    const bool speedOk = clipsInteraction()->resetClipSpeed(clipKey);
    if (pitchOk || speedOk) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Reset Clip Pitch and Speed"),
                                           muse::trc("trackedit", "Reset Clip Pitch and Speed"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::renderClipPitchAndSpeed(const ClipKey& clipKey)
{
    if (clipsInteraction()->renderClipPitchAndSpeed(clipKey)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Rendered time-stretched audio"), muse::trc("trackedit", "Render"));
        return true;
    }
    return false;
}

void TrackeditOperationController::clearClipboard()
{
    clipboard()->clearTrackData();
}

muse::Ret TrackeditOperationController::pasteFromClipboard(secs_t begin, bool moveClips, bool moveAllTracks)
{
    auto modifiedState = false;
    muse::Ret ret;
    const auto paths = clipboard()->systemClipboardFilePaths();
    const bool pastingFromSystemClipboard = !paths.empty();

    bool recreateRangeSelection = false;
    secs_t pastedDataEndTime = 0.0;

    if (pastingFromSystemClipboard) {
        ret = importer()->importFromSystemClipboard(paths, begin);
        dispatcher()->dispatch("center-view-on-playhead", muse::actions::ActionData::make_arg1<bool>(
                                   true /* center only if playhead is not visible */));
    } else {
        const std::vector<ITrackDataPtr> trackData = clipboard()->trackDataCopy();
        for (const ITrackDataPtr& data : trackData) {
            pastedDataEndTime = std::max(pastedDataEndTime, data->endTime());
        }
        recreateRangeSelection = clipboard()->isRangeSelectionCopy();

        ret = tracksInteraction()->paste(trackData, begin, moveClips, moveAllTracks,
                                         clipboard()->isMultiSelectionCopy(), modifiedState);
    }

    if (ret) {
        //! NOTE Importing files pushes its own "Import" history state
        if (!pastingFromSystemClipboard) {
            projectHistory()->pushHistoryState(muse::trc("trackedit", "Pasted from the clipboard"), muse::trc("trackedit", "Paste"));
        }

        if (recreateRangeSelection) {
            selectionController()->setDataSelectedStartTime(begin, true);
            selectionController()->setDataSelectedEndTime(begin + pastedDataEndTime, true);
        }
    } else if (modifiedState) {
        projectHistory()->rollbackState();
        globalContext()->currentTrackeditProject()->reload();
    }
    return ret;
}

bool TrackeditOperationController::cutClipIntoClipboard(const ClipKey& clipKey)
{
    clipboard()->clearSystemClipboard();
    ITrackDataPtr data = clipsInteraction()->cutClip(clipKey);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Cut to the clipboard"), muse::trc("trackedit", "Cut"));
    return true;
}

bool TrackeditOperationController::cutItemDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips,
                                                            bool isRangeSelection)
{
    clipboard()->clearSystemClipboard();
    std::vector<ITrackDataPtr> tracksData;
    for (const auto& trackId : tracksIds) {
        const auto data = tracksInteraction()->cutTrackData(trackId, begin, end, moveClips);
        if (!data) {
            return false;
        }
        tracksData.push_back(std::move(data));
    }
    for (auto& trackData : tracksData) {
        clipboard()->addTrackData(std::move(trackData));
    }
    clipboard()->setRangeSelectionCopy(isRangeSelection);
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Cut to the clipboard"), muse::trc("trackedit", "Cut"));
    return true;
}

bool TrackeditOperationController::copyClipIntoClipboard(const ClipKey& clipKey)
{
    clipboard()->clearSystemClipboard();
    ITrackDataPtr data = clipsInteraction()->copyClip(clipKey);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    return true;
}

bool TrackeditOperationController::copyNonContinuousTrackDataIntoClipboard(const TrackId trackId, const TrackItemKeyList& itemKeys,
                                                                           secs_t offset)
{
    clipboard()->clearSystemClipboard();
    ITrackDataPtr data = tracksInteraction()->copyNonContinuousTrackData(trackId, itemKeys, offset);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    if (itemKeys.size() > 1) {
        clipboard()->setMultiSelectionCopy(true);
    }
    return true;
}

bool TrackeditOperationController::copyContinuousTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end)
{
    clipboard()->clearSystemClipboard();
    ITrackDataPtr data = tracksInteraction()->copyContinuousTrackData(trackId, begin, end);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    clipboard()->setRangeSelectionCopy(true);
    return true;
}

bool TrackeditOperationController::removeClip(const ClipKey& clipKey)
{
    if (const std::optional<TimeSpan> span = clipsInteraction()->removeClip(clipKey)) {
        pushProjectHistoryDeleteState(span->start(), span->duration());
        return true;
    }
    return false;
}

bool TrackeditOperationController::removeClips(const ClipKeyList& clipKeyList, bool moveClips)
{
    if (clipsInteraction()->removeClips(clipKeyList, moveClips)) {
        bool hasLabels = isLabelsSelected();
        if (hasLabels) {
            labelsInteraction()->removeLabels(selectedLabels(), moveClips);
        }

        const std::string msg
            = hasLabels ? muse::trc("trackedit", "Remove multiple items") : muse::trc("trackedit", "Remove multiple clips");
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Remove"), msg);
        return true;
    }
    return false;
}

bool TrackeditOperationController::removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips)
{
    if (tracksInteraction()->removeTracksData(tracksIds, begin, end, moveClips)) {
        //: Undo history entry name; shown after Undo and Redo in the Edit menu
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Delete"), muse::trc("trackedit", "Delete and close gap"));
        return true;
    }
    return false;
}

muse::RetVal<ClipKeyList> TrackeditOperationController::moveClips(const ClipKeyList& clipKeyList, secs_t timePositionOffset,
                                                                  int trackPositionOffset,
                                                                  bool completed,
                                                                  bool& clipsMovedToOtherTrack)
{
    // Labels to move along with clips
    LabelKeyList selectedLabels = selectionController()->selectedLabels();
    if (!selectedLabels.empty()) {
        secs_t clampedOffset = timePositionOffset;

        for (const auto& clipKey : clipKeyList) {
            secs_t startTime = clipsInteraction()->clipStartTime(clipKey);
            if (startTime + clampedOffset < 0.0) {
                clampedOffset = -startTime;
            }
        }

        auto prj = globalContext()->currentTrackeditProject();
        for (const auto& labelKey : selectedLabels) {
            trackedit::Label label = prj->label(labelKey);
            if (label.isValid() && label.startTime + clampedOffset < 0.0) {
                clampedOffset = -label.startTime;
            }
        }

        labelsInteraction()->moveLabels(selectedLabels, clampedOffset, trackPositionOffset);
        timePositionOffset = clampedOffset;
    }

    muse::RetVal<ClipKeyList> result = clipsInteraction()->moveClips(clipKeyList, timePositionOffset, trackPositionOffset, completed,
                                                                     clipsMovedToOtherTrack);

    if (!result.ret) {
        if (completed) {
            clipsMovedToOtherTrack = false;
            projectHistory()->rollbackState();
            globalContext()->currentTrackeditProject()->reload();
        }
    } else if (completed) {
        const std::string msg = !selectedLabels.empty() ? muse::trc("trackedit", "Items moved") : muse::trc("trackedit", "Clip moved");
        projectHistory()->pushHistoryState(msg, muse::trc("trackedit", "Move clip"));
    }
    return result;
}

bool TrackeditOperationController::moveRangeSelection(secs_t timePositionOffset, bool completed)
{
    ClipKeyList clipsInRange = selectionController()->clipsIntersectingRangeSelection();
    LabelKeyList labelsInRange = selectionController()->labelsIntersectingRangeSelection();

    if (clipsInRange.empty() && labelsInRange.empty()) {
        return false;
    }

    secs_t clampedOffset = timePositionOffset;

    for (const auto& clipKey : clipsInRange) {
        secs_t startTime = clipsInteraction()->clipStartTime(clipKey);
        if (startTime + clampedOffset < 0.0) {
            clampedOffset = -startTime;
        }
    }

    auto prj = globalContext()->currentTrackeditProject();
    for (const auto& labelKey : labelsInRange) {
        trackedit::Label label = prj->label(labelKey);
        if (label.isValid() && label.startTime + clampedOffset < 0.0) {
            clampedOffset = -label.startTime;
        }
    }

    secs_t dataSelStart = selectionController()->dataSelectedStartTime();
    if (dataSelStart + clampedOffset < 0.0) {
        clampedOffset = -dataSelStart;
    }

    clipsInteraction()->changeClipsStartTime(clipsInRange, clampedOffset, completed);

    if (!labelsInRange.empty()) {
        labelsInteraction()->moveLabels(labelsInRange, clampedOffset, 0);
    }

    selectionController()->setDataSelectedStartTime(
        selectionController()->dataSelectedStartTime() + clampedOffset, false);
    selectionController()->setDataSelectedEndTime(
        selectionController()->dataSelectedEndTime() + clampedOffset, false);

    if (completed) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Items moved"), muse::trc("trackedit", "Move items"));
    }

    return true;
}

void TrackeditOperationController::cancelItemDragEdit()
{
    if (!projectHistory()->interactionOngoing()) {
        return;
    }
    clipsInteraction()->cancelClipDragEdit();
    labelsInteraction()->resetLabelStretchState();
    projectHistory()->rollbackState();
    globalContext()->currentTrackeditProject()->reload();
}

bool TrackeditOperationController::splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots)
{
    if (tracksInteraction()->splitTracksAt(tracksIds, pivots)) {
        //: Undo history entry name; shown after Undo and Redo in the Edit menu
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Split"), muse::trc("trackedit", "Split"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitClipsAtSilences(const ClipKeyList& clipKeyList)
{
    if (clipsInteraction()->splitClipsAtSilences(clipKeyList)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Split clips at silence"), muse::trc("trackedit", "Split at silence"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitRangeSelectionAtSilences(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->splitRangeSelectionAtSilences(tracksIds, begin, end)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Split clips at silence"), muse::trc("trackedit", "Split at silence"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitRangeSelectionIntoNewTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->splitRangeSelectionIntoNewTracks(tracksIds, begin, end)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Split into new track"), muse::trc("trackedit", "Split into new track"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitClipsIntoNewTracks(const ClipKeyList& clipKeyList)
{
    if (clipsInteraction()->splitClipsIntoNewTracks(clipKeyList)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Split into new track"), muse::trc("trackedit", "Split into new track"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->mergeSelectedOnTracks(tracksIds, begin, end)) {
        const secs_t duration = end - begin;
        pushProjectHistoryJoinState(begin, duration);
        return true;
    }
    return false;
}

bool TrackeditOperationController::duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->duplicateSelectedOnTracks(tracksIds, begin, end)) {
        pushProjectHistoryDuplicateState();
        return true;
    }
    return false;
}

bool TrackeditOperationController::duplicateClip(const ClipKey& clipKey)
{
    return clipsInteraction()->duplicateClip(clipKey);
}

bool TrackeditOperationController::duplicateClips(const ClipKeyList& clipKeyList)
{
    if (clipsInteraction()->duplicateClips(clipKeyList)) {
        pushProjectHistoryDuplicateState();
        return true;
    }
    return false;
}

bool TrackeditOperationController::clipSplitCut(const ClipKey& clipKey)
{
    ITrackDataPtr data = clipsInteraction()->clipSplitCut(clipKey);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Split-cut to the clipboard"), muse::trc("trackedit", "Split cut"));
    return true;
}

bool TrackeditOperationController::clipSplitDelete(const ClipKey& clipKey)
{
    if (clipsInteraction()->clipSplitDelete(clipKey)) {
        pushProjectHistorySplitDeleteState();
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    std::vector<ITrackDataPtr> tracksData = tracksInteraction()->splitCutSelectedOnTracks(tracksIds, begin, end);
    if (tracksData.empty()) {
        return false;
    }
    for (auto& trackData : tracksData) {
        clipboard()->addTrackData(std::move(trackData));
    }
    clipboard()->setRangeSelectionCopy(true);
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Split-cut to the clipboard"), muse::trc("trackedit", "Split cut"));
    return true;
}

bool TrackeditOperationController::splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->splitDeleteSelectedOnTracks(tracksIds, begin, end)) {
        pushProjectHistorySplitDeleteState();
        return true;
    }
    return false;
}

bool TrackeditOperationController::trimClipsLeft(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                 UndoPushType type)
{
    const auto labelKeys = selectedLabels();
    deltaSec = clampBoundaryDeltaToSelectedItems(deltaSec, minClipDuration, labelKeys);

    const auto success = clipsInteraction()->trimClipsLeft(clipKeyList, deltaSec, minClipDuration, completed);
    if (!success) {
        return success;
    }

    bool hasLabels = isLabelsSelected();
    if (hasLabels) {
        labelsInteraction()->stretchLabelsLeft(labelKeys, deltaSec, completed);
    }

    if (completed) {
        std::string msg = hasLabels ? muse::trc("trackedit", "Trim items left") : muse::trc("trackedit", "Trim clip left");
        //: Undo history entry name; shown after Undo and Redo in the Edit menu
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Trim"), msg, type);
    }
    return success;
}

bool TrackeditOperationController::trimClipsRight(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                  UndoPushType type)
{
    const auto labelKeys = selectedLabels();
    deltaSec = clampBoundaryDeltaToSelectedItems(deltaSec, minClipDuration, labelKeys);

    const auto success = clipsInteraction()->trimClipsRight(clipKeyList, deltaSec, minClipDuration, completed);
    if (!success) {
        return success;
    }

    bool hasLabels = isLabelsSelected();
    if (hasLabels) {
        labelsInteraction()->stretchLabelsRight(labelKeys, -deltaSec, completed);
    }
    if (completed) {
        std::string msg = hasLabels ? muse::trc("trackedit", "Trim items right") : muse::trc("trackedit", "Trim clip right");
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Trim"), msg, type);
    }
    return success;
}

bool TrackeditOperationController::stretchClipsLeft(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                    UndoPushType type)
{
    const auto labelKeys = selectedLabels();
    deltaSec = clampBoundaryDeltaToSelectedItems(deltaSec, minClipDuration, labelKeys);

    const auto success = clipsInteraction()->stretchClipsLeft(clipKeyList, deltaSec, minClipDuration, completed);
    if (!success) {
        return success;
    }

    bool hasLabels = isLabelsSelected();
    if (hasLabels) {
        labelsInteraction()->stretchLabelsLeft(labelKeys, deltaSec, completed);
    }

    if (completed) {
        auto [longDesc, msg] = stretchHistoryDescriptions(clipKeyList, hasLabels, true);
        projectHistory()->pushHistoryState(longDesc, msg, type);
    }

    return success;
}

bool TrackeditOperationController::stretchClipsRight(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration,
                                                     bool completed,
                                                     UndoPushType type)
{
    const auto labelKeys = selectedLabels();
    deltaSec = clampBoundaryDeltaToSelectedItems(deltaSec, minClipDuration, labelKeys);

    const auto success = clipsInteraction()->stretchClipsRight(clipKeyList, deltaSec, minClipDuration, completed);
    if (!success) {
        return success;
    }

    bool hasLabels = isLabelsSelected();
    if (hasLabels) {
        labelsInteraction()->stretchLabelsRight(labelKeys, -deltaSec, completed);
    }

    if (completed) {
        auto [longDesc, msg] = stretchHistoryDescriptions(clipKeyList, hasLabels, false);
        projectHistory()->pushHistoryState(longDesc, msg, type);
    }

    return success;
}

std::pair<std::string, std::string> TrackeditOperationController::stretchHistoryDescriptions(
    const ClipKeyList& clipKeyList, bool hasLabels, bool isLeft) const
{
    if (!hasLabels && clipKeyList.size() == 1) {
        const double speed = globalContext()->currentTrackeditProject()->clip(clipKeyList[0]).speed;
        const int speedPct = static_cast<int>(100.0 / speed + 0.5);
        return { muse::trc("trackedit", "Changed Speed"),
                 muse::qtrc("trackedit", "Changed speed to: %1%").arg(speedPct).toStdString() };
    }
    if (isLeft) {
        return { muse::trc("trackedit", "Stretch Left"),
                 hasLabels ? muse::trc("trackedit", "Stretch items left") : muse::trc("trackedit", "Stretch clips left") };
    }
    return { muse::trc("trackedit", "Stretch Right"),
             hasLabels ? muse::trc("trackedit", "Stretch items right") : muse::trc("trackedit", "Stretch clips right") };
}

secs_t TrackeditOperationController::clipDuration(const ClipKey& clipKey) const
{
    return clipsInteraction()->clipDuration(clipKey);
}

double TrackeditOperationController::nearestZeroCrossing(double t0) const
{
    return tracksInteraction()->nearestZeroCrossing(t0);
}

muse::Ret TrackeditOperationController::makeRoomForClip(const ClipKey& clipKey)
{
    return clipsInteraction()->makeRoomForClip(clipKey);
}

bool TrackeditOperationController::newMonoTrack()
{
    if (tracksInteraction()->newMonoTrack()) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Created new mono track"), muse::trc("trackedit", "New mono track"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::newStereoTrack()
{
    if (tracksInteraction()->newStereoTrack()) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Created new stereo track"), muse::trc("trackedit", "New stereo track"));
        return true;
    }
    return false;
}

muse::RetVal<TrackId> TrackeditOperationController::newLabelTrack(const muse::String& title)
{
    auto track = tracksInteraction()->newLabelTrack(title);
    if (track.ret.success()) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Created label track"), muse::trc("trackedit", "New label track"));
    }
    return track;
}

bool TrackeditOperationController::deleteTracks(const TrackIdList& trackIds)
{
    if (tracksInteraction()->deleteTracks(trackIds)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Delete track"), muse::trc("trackedit", "Delete track"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::duplicateTracks(const TrackIdList& trackIds)
{
    if (tracksInteraction()->duplicateTracks(trackIds)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Duplicate track"), muse::trc("trackedit", "Duplicate track"));
        return true;
    }
    return false;
}

void TrackeditOperationController::moveTracks(const TrackIdList& trackIds, TrackMoveDirection direction)
{
    if (tracksInteraction()->moveTracks(trackIds, direction)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Move track"), muse::trc("trackedit", "Move track"));
    }
}

void TrackeditOperationController::moveTracksTo(const TrackIdList& trackIds, int pos)
{
    if (tracksInteraction()->moveTracksTo(trackIds, pos)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Move track"), muse::trc("trackedit", "Move track"));
    }
}

ClipKeyList TrackeditOperationController::clipsOnTrack(const TrackId trackId)
{
    return clipsInteraction()->clipsOnTrack(trackId);
}

bool TrackeditOperationController::undo()
{
    return m_undoManager->undo();
}

bool TrackeditOperationController::canUndo()
{
    return m_undoManager->canUndo();
}

bool TrackeditOperationController::redo()
{
    return m_undoManager->redo();
}

bool TrackeditOperationController::canRedo()
{
    return m_undoManager->canRedo();
}

bool TrackeditOperationController::undoRedoToIndex(size_t index)
{
    return m_undoManager->undoRedoToIndex(index);
}

void TrackeditOperationController::notifyAboutCancelDragEdit()
{
    m_cancelDragEditRequested.notify();
}

muse::async::Notification TrackeditOperationController::cancelDragEditRequested() const
{
    return m_cancelDragEditRequested;
}

bool TrackeditOperationController::insertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration)
{
    if (tracksInteraction()->insertSilence(trackIds, begin, end, duration)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Insert silence"), muse::trc("trackedit", "Insert silence"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::toggleStretchToMatchProjectTempo(const ClipKey& clipKey)
{
    return clipsInteraction()->toggleStretchToMatchProjectTempo(clipKey);
}

int64_t TrackeditOperationController::clipGroupId(const trackedit::ClipKey& clipKey) const
{
    return clipsInteraction()->clipGroupId(clipKey);
}

void TrackeditOperationController::setClipGroupId(const trackedit::ClipKey& clipKey, int64_t id)
{
    clipsInteraction()->setClipGroupId(clipKey, id);
}

void TrackeditOperationController::groupClips(const trackedit::ClipKeyList& clipKeyList)
{
    clipsInteraction()->groupClips(clipKeyList);
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Clips grouped"), muse::trc("trackedit", "Clips grouped"));
}

void TrackeditOperationController::ungroupClips(const trackedit::ClipKeyList& clipKeyList)
{
    clipsInteraction()->ungroupClips(clipKeyList);
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Clips ungrouped"), muse::trc("trackedit", "Clips ungrouped"));
}

ClipKeyList TrackeditOperationController::clipsInGroup(int64_t id) const
{
    return clipsInteraction()->clipsInGroup(id);
}

bool TrackeditOperationController::changeTracksFormat(const TrackIdList& tracksIds, trackedit::TrackFormat format)
{
    if (tracksInteraction()->changeTracksFormat(tracksIds, format)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Changed track format"), muse::trc("trackedit", "Changed track format"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeTracksRate(const TrackIdList& tracksIds, int rate)
{
    if (tracksInteraction()->changeTracksRate(tracksIds, rate)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Changed track rate"), muse::trc("trackedit", "Changed track rate"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::swapStereoChannels(const TrackIdList& tracksIds)
{
    if (tracksInteraction()->swapStereoChannels(tracksIds)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Swapped stereo channels"),
                                           muse::trc("trackedit", "Swapped stereo channels"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitStereoTracksToLRMono(const TrackIdList& tracksIds)
{
    if (tracksInteraction()->splitStereoTracksToLRMono(tracksIds)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Split stereo tracks to L/R mono"),
                                           muse::trc("trackedit", "Split stereo tracks to L/R mono"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitStereoTracksToCenterMono(const TrackIdList& tracksIds)
{
    if (tracksInteraction()->splitStereoTracksToCenterMono(tracksIds)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Split stereo tracks to center mono"),
                                           muse::trc("trackedit", "Split stereo tracks to center mono"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::makeStereoTrack(const TrackId left, const TrackId right)
{
    if (tracksInteraction()->makeStereoTrack(left, right)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Make stereo track"), muse::trc("trackedit", "Make stereo track"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::resampleTracks(const TrackIdList& tracksIds, int rate)
{
    if (tracksInteraction()->resampleTracks(tracksIds, rate)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Resampled audio track(s)"), muse::trc("trackedit", "Resample track"));
        return true;
    }
    return false;
}

muse::RetVal<LabelKey> TrackeditOperationController::addLabel(const TrackId& toTrackId)
{
    muse::RetVal<LabelKey> retVal = labelsInteraction()->addLabel(toTrackId);
    if (retVal.ret) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Label added"), muse::trc("trackedit", "Add label"));
    }

    return retVal;
}

bool TrackeditOperationController::addLabelToSelection()
{
    if (labelsInteraction()->addLabelToSelection()) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Label added"), muse::trc("trackedit", "Add label"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeLabelTitle(const LabelKey& labelKey, const muse::String& title)
{
    if (labelsInteraction()->changeLabelTitle(labelKey, title)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Label title changed"), muse::trc("trackedit", "Changed label title"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeLabelLowFrequency(const LabelKey& labelKey, double frequency)
{
    if (labelsInteraction()->changeLabelLowFrequency(labelKey, frequency)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Label low frequency changed"),
                                           muse::trc("trackedit", "Change label low frequency"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeLabelHighFrequency(const LabelKey& labelKey, double frequency)
{
    if (labelsInteraction()->changeLabelHighFrequency(labelKey, frequency)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Label high frequency changed"),
                                           muse::trc("trackedit", "Change label high frequency"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::cutLabel(const LabelKey& labelKey)
{
    ITrackDataPtr data = labelsInteraction()->cutLabel(labelKey);
    if (!data) {
        return false;
    }

    clipboard()->addTrackData(std::move(data));
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Cut"), muse::trc("trackedit", "Cut label"));
    return true;
}

bool TrackeditOperationController::copyLabel(const LabelKey& labelKey)
{
    ITrackDataPtr data = labelsInteraction()->copyLabel(labelKey);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    return true;
}

bool TrackeditOperationController::removeLabel(const LabelKey& labelKey)
{
    if (labelsInteraction()->removeLabel(labelKey)) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Remove"), muse::trc("trackedit", "Remove label"));
        return true;
    }
    return false;
}

bool TrackeditOperationController::removeLabels(const LabelKeyList& labelKeys, bool moveLabels)
{
    if (labelsInteraction()->removeLabels(labelKeys, moveLabels)) {
        bool hasClips = isClipsSelected();
        if (hasClips) {
            clipsInteraction()->removeClips(selectedClips(), moveLabels);
        }

        if (hasClips) {
            projectHistory()->pushHistoryState(muse::trc("trackedit", "Remove"), muse::trc("trackedit", "Remove multiple items"));
        } else {
            projectHistory()->pushHistoryState(muse::trc("trackedit", "Remove"), muse::trc("trackedit", "Remove multiple labels"));
        }

        return true;
    }
    return false;
}

bool TrackeditOperationController::moveLabels(const LabelKeyList& labelKeys, secs_t timePositionOffset, bool completed)
{
    ClipKeyList selectedClips = selectionController()->selectedClips();
    if (!selectedClips.empty()) {
        secs_t clampedOffset = timePositionOffset;

        for (const auto& clipKey : selectedClips) {
            secs_t startTime = clipsInteraction()->clipStartTime(clipKey);
            if (startTime + clampedOffset < 0.0) {
                clampedOffset = -startTime;
            }
        }

        auto prj = globalContext()->currentTrackeditProject();
        for (const auto& labelKey : labelKeys) {
            trackedit::Label label = prj->label(labelKey);
            if (label.isValid() && label.startTime + clampedOffset < 0.0) {
                clampedOffset = -label.startTime;
            }
        }

        clipsInteraction()->changeClipsStartTime(selectedClips, clampedOffset, completed);

        timePositionOffset = clampedOffset;
    }

    muse::RetVal<LabelKeyList> retVal = labelsInteraction()->moveLabels(labelKeys, timePositionOffset, 0);
    if (retVal.ret && completed) {
        const std::string msg = !selectedClips.empty() ? muse::trc("trackedit", "Move items") : muse::trc("trackedit", "Move labels");
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Move"), msg);
    }
    return retVal.ret;
}

muse::RetVal<LabelKeyList> TrackeditOperationController::moveLabels(const LabelKeyList& labelKeys, secs_t timePositionOffset,
                                                                    int trackPositionOffset,
                                                                    bool completed)
{
    muse::RetVal<LabelKeyList> retVal = labelsInteraction()->moveLabels(labelKeys, timePositionOffset, trackPositionOffset);
    if (!retVal.ret) {
        return retVal;
    }

    bool clipsSelected = isClipsSelected();
    if (isClipsSelected()) {
        bool clipsMovedToOtherTracks = false;
        clipsInteraction()->moveClips(selectedClips(), timePositionOffset, trackPositionOffset, completed, clipsMovedToOtherTracks);
    }

    if (completed) {
        const std::string msg = clipsSelected ? muse::trc("trackedit", "Move items") : muse::trc("trackedit", "Move labels");
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Move"), msg);
    }

    return retVal;
}

muse::RetVal<LabelKeyList> TrackeditOperationController::moveLabelsToTrack(const LabelKeyList& labelKeys, const TrackId& toTrackId,
                                                                           bool completed)
{
    muse::RetVal<LabelKeyList> retVal = labelsInteraction()->moveLabelsToTrack(labelKeys, toTrackId);
    if (retVal.ret && completed) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Labels moved"), muse::trc("trackedit", "Move labels"));
    }
    return retVal;
}

bool TrackeditOperationController::stretchLabelLeft(const LabelKey& labelKey, secs_t newStartTime, bool completed)
{
    bool success = labelsInteraction()->stretchLabelLeft(labelKey, newStartTime, completed);
    if (success && completed) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Label stretched"), muse::trc("trackedit", "Stretch label left"));
    }
    return success;
}

bool TrackeditOperationController::stretchLabelsLeft(const LabelKeyList& labelKeyList, secs_t deltaSec,
                                                     bool completed)
{
    bool success = labelsInteraction()->stretchLabelsLeft(labelKeyList, deltaSec, completed);
    if (!success) {
        return success;
    }

    bool clipsSelected = isClipsSelected();
    if (clipsSelected) {
        constexpr double MIN_CLIP_WIDTH = 3.0;
        clipsInteraction()->stretchClipsLeft(selectedClips(), deltaSec, MIN_CLIP_WIDTH, completed);
    }

    if (completed) {
        const std::string msg
            = clipsSelected ? muse::trc("trackedit", "Stretch items left") : muse::trc("trackedit", "Stretch labels left");
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Stretch"), msg);
    }

    return success;
}

bool TrackeditOperationController::stretchLabelRight(const LabelKey& labelKey, secs_t newEndTime, bool completed)
{
    bool success = labelsInteraction()->stretchLabelRight(labelKey, newEndTime, completed);
    if (success && completed) {
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Label stretched"), muse::trc("trackedit", "Stretch label right"));
    }
    return success;
}

bool TrackeditOperationController::stretchLabelsRight(const LabelKeyList& labelKeyList, secs_t deltaSec,
                                                      bool completed)
{
    bool success = labelsInteraction()->stretchLabelsRight(labelKeyList, deltaSec, completed);
    if (!success) {
        return success;
    }

    bool clipsSelected = isClipsSelected();
    if (clipsSelected) {
        constexpr double MIN_CLIP_WIDTH = 3.0;
        clipsInteraction()->stretchClipsRight(selectedClips(), -deltaSec, MIN_CLIP_WIDTH, completed);
    }

    if (completed) {
        const std::string msg = clipsSelected ? muse::trc("trackedit", "Stretch items right") : muse::trc("trackedit",
                                                                                                          "Stretch labels right");
        projectHistory()->pushHistoryState(muse::trc("trackedit", "Stretch"), msg);
    }
    return success;
}

void TrackeditOperationController::resetLabelStretchState()
{
    labelsInteraction()->resetLabelStretchState();
}

muse::Progress TrackeditOperationController::progress() const
{
    return tracksInteraction()->progress();
}

void TrackeditOperationController::pushProjectHistoryJoinState(secs_t start, secs_t duration)
{
    projectHistory()->pushHistoryState(
        //: History entry. %1 is a duration in seconds,
        //: %2 is the position in seconds it starts at
        muse::qtrc("trackedit", "Joined %1 seconds at %2")
        .arg(duration.to_double()).arg(start.to_double()).toStdString(),
        //: Undo history entry name; shown after Undo and Redo in the Edit menu
        muse::trc("trackedit", "Join"));
}

void TrackeditOperationController::pushProjectHistoryDuplicateState()
{
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Duplicated"),
                                       /*: Undo history entry name; shown after Undo and Redo in the Edit menu */
                                       muse::trc("trackedit", "Duplicate"));
}

void TrackeditOperationController::pushProjectHistorySplitDeleteState()
{
    projectHistory()->pushHistoryState(muse::trc("trackedit", "Split-deleted clips"), muse::trc("trackedit", "Split delete"));
}

void TrackeditOperationController::pushProjectHistoryDeleteState(secs_t start, secs_t duration)
{
    projectHistory()->pushHistoryState(
        //: History entry. %1 is a duration in seconds,
        //: %2 is the position in seconds it starts at
        muse::qtrc("trackedit", "Delete %1 seconds at %2")
        .arg(duration.to_double()).arg(start.to_double()).toStdString(),
        muse::trc("trackedit", "Delete"));
}

std::optional<secs_t> TrackeditOperationController::shortestLabelDuration(const LabelKeyList& labelKeys) const
{
    const auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return std::nullopt;
    }

    std::optional<secs_t> shortestDuration;
    for (const auto& labelKey : labelKeys) {
        const auto label = prj->label(labelKey);
        if (!label.isValid()) {
            continue;
        }

        const secs_t duration = label.endTime - label.startTime;
        if (!shortestDuration.has_value() || duration < shortestDuration.value()) {
            shortestDuration = duration;
        }
    }

    return shortestDuration;
}

secs_t TrackeditOperationController::clampBoundaryDeltaToSelectedItems(secs_t deltaSec,
                                                                       secs_t minClipDuration,
                                                                       const LabelKeyList& labelKeys) const
{
    if (!muse::RealIsEqualOrMore(deltaSec, 0.0) || labelKeys.empty()) {
        return deltaSec;
    }

    std::optional<secs_t> maxShrinkDelta;
    for (const auto& clipKey : selectedClips()) {
        const secs_t duration = clipDuration(clipKey);
        const secs_t clipShrinkDelta = std::max(0.0, (duration - minClipDuration).to_double());
        if (!maxShrinkDelta.has_value() || clipShrinkDelta < maxShrinkDelta.value()) {
            maxShrinkDelta = clipShrinkDelta;
        }
    }

    if (const auto labelDuration = shortestLabelDuration(labelKeys); labelDuration.has_value()) {
        const secs_t labelShrinkDelta = std::max(0.0, labelDuration.value().to_double());
        maxShrinkDelta = std::min(labelShrinkDelta, maxShrinkDelta.value_or(labelShrinkDelta));
    }

    if (!maxShrinkDelta.has_value() || muse::RealIsEqualOrLess(deltaSec, maxShrinkDelta.value())) {
        return deltaSec;
    }

    return maxShrinkDelta.value();
}

bool TrackeditOperationController::isClipsSelected() const
{
    return selectionController()->hasSelectedClips();
}

ClipKeyList TrackeditOperationController::selectedClips() const
{
    return selectionController()->selectedClips();
}

bool TrackeditOperationController::isLabelsSelected() const
{
    return selectionController()->hasSelectedLabels();
}

LabelKeyList TrackeditOperationController::selectedLabels() const
{
    return selectionController()->selectedLabels();
}
}
