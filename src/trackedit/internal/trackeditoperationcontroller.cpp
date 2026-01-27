/*
 * Audacity: A Digital Audio Editor
 */
#include "trackeditoperationcontroller.h"
#include "trackediterrors.h"

namespace au::trackedit {
TrackeditOperationController::TrackeditOperationController(const muse::modularity::ContextPtr& ctx,
                                                           std::unique_ptr<IUndoManager> undoManager)
    : muse::Injectable(ctx), m_undoManager{std::move(undoManager)} {}

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
        std::stringstream ss;
        ss << "Trim selected audio tracks from " << begin << " seconds to " << end << " seconds";
        projectHistory()->pushHistoryState(ss.str(), "Trim Audio");
        return true;
    }
    return false;
}

bool TrackeditOperationController::silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->silenceTracksData(tracksIds, begin, end)) {
        std::stringstream ss;
        ss << "Silenced selected tracks for " << begin << " seconds at " << end << "seconts";
        projectHistory()->pushHistoryState(ss.str(), "Silence");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title)
{
    if (tracksInteraction()->changeTrackTitle(trackId, title)) {
        projectHistory()->pushHistoryState("Track Title", "Changed Track Title");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipTitle(const ClipKey& clipKey, const muse::String& newTitle)
{
    if (clipsInteraction()->changeClipTitle(clipKey, newTitle)) {
        projectHistory()->pushHistoryState("Clip Title", "Changed Clip Title");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipPitch(const ClipKey& clipKey, int pitch)
{
    if (clipsInteraction()->changeClipPitch(clipKey, pitch)) {
        projectHistory()->pushHistoryState("Pitch Shift", "Changed Pitch Shift");
        return true;
    }
    return false;
}

bool TrackeditOperationController::resetClipPitch(const ClipKey& clipKey)
{
    if (clipsInteraction()->resetClipPitch(clipKey)) {
        projectHistory()->pushHistoryState("Reset Clip Pitch", "Reset Clip Pitch");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipSpeed(const ClipKey& clipKey, double speed)
{
    if (clipsInteraction()->changeClipSpeed(clipKey, speed)) {
        projectHistory()->pushHistoryState("Changed Speed", "Changed Speed");
        return true;
    }
    return false;
}

bool TrackeditOperationController::resetClipSpeed(const ClipKey& clipKey)
{
    if (clipsInteraction()->resetClipSpeed(clipKey)) {
        projectHistory()->pushHistoryState("Reset Clip Speed", "Reset Clip Speed");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipColor(const ClipKey& clipKey, const std::string& color)
{
    return clipsInteraction()->changeClipColor(clipKey, color);
}

bool TrackeditOperationController::changeTracksColor(const TrackIdList& tracksIds, const std::string& color)
{
    if (tracksInteraction()->changeTracksColor(tracksIds, color)) {
        projectHistory()->pushHistoryState("Changed track color", "Changed track color");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize)
{
    return clipsInteraction()->changeClipOptimizeForVoice(clipKey, optimize);
}

bool TrackeditOperationController::renderClipPitchAndSpeed(const ClipKey& clipKey)
{
    if (clipsInteraction()->renderClipPitchAndSpeed(clipKey)) {
        projectHistory()->pushHistoryState("Rendered time-stretched audio", "Render");
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
    const auto ret = tracksInteraction()->paste(clipboard()->trackDataCopy(), begin, moveClips, moveAllTracks,
                                                clipboard()->isMultiSelectionCopy(), modifiedState);
    if (ret) {
        projectHistory()->pushHistoryState("Pasted from the clipboard", "Paste");
    } else if (modifiedState) {
        projectHistory()->rollbackState();
        globalContext()->currentTrackeditProject()->reload();
    }
    return ret;
}

bool TrackeditOperationController::cutClipIntoClipboard(const ClipKey& clipKey)
{
    ITrackDataPtr data = clipsInteraction()->cutClip(clipKey);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    projectHistory()->pushHistoryState("Cut to the clipboard", "Cut");
    return true;
}

bool TrackeditOperationController::cutItemDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips)
{
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
    projectHistory()->pushHistoryState("Cut to the clipboard", "Cut");
    return true;
}

bool TrackeditOperationController::copyClipIntoClipboard(const ClipKey& clipKey)
{
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
    ITrackDataPtr data = tracksInteraction()->copyContinuousTrackData(trackId, begin, end);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
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
        projectHistory()->pushHistoryState("Delete", "Delete multiple clips");
        return true;
    }
    return false;
}

bool TrackeditOperationController::removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips)
{
    if (tracksInteraction()->removeTracksData(tracksIds, begin, end, moveClips)) {
        projectHistory()->pushHistoryState("Delete", "Delete and close gap");
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

        labelsInteraction()->moveLabels(selectedLabels, clampedOffset);
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
        const std::string msg = !selectedLabels.empty() ? "Items moved" : "Clip moved";
        projectHistory()->pushHistoryState(msg, "Move clip");
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

    for (const auto& clipKey : clipsInRange) {
        secs_t currentStart = clipsInteraction()->clipStartTime(clipKey);
        clipsInteraction()->changeClipStartTime(clipKey, currentStart + clampedOffset, completed);
    }

    if (!labelsInRange.empty()) {
        labelsInteraction()->moveLabels(labelsInRange, clampedOffset);
    }

    selectionController()->setDataSelectedStartTime(
        selectionController()->dataSelectedStartTime() + clampedOffset, false);
    selectionController()->setDataSelectedEndTime(
        selectionController()->dataSelectedEndTime() + clampedOffset, false);

    if (completed) {
        projectHistory()->pushHistoryState("Items moved", "Move items");
    }

    return true;
}

void TrackeditOperationController::cancelItemDragEdit()
{
    if (!projectHistory()->interactionOngoing()) {
        return;
    }
    clipsInteraction()->cancelClipDragEdit();
    projectHistory()->rollbackState();
    globalContext()->currentTrackeditProject()->reload();
}

bool TrackeditOperationController::splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots)
{
    if (tracksInteraction()->splitTracksAt(tracksIds, pivots)) {
        projectHistory()->pushHistoryState("Split", "Split");
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitClipsAtSilences(const ClipKeyList& clipKeyList)
{
    if (clipsInteraction()->splitClipsAtSilences(clipKeyList)) {
        projectHistory()->pushHistoryState("Split clips at silence", "Split at silence");
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitRangeSelectionAtSilences(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->splitRangeSelectionAtSilences(tracksIds, begin, end)) {
        projectHistory()->pushHistoryState("Split clips at silence", "Split at silence");
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitRangeSelectionIntoNewTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    if (tracksInteraction()->splitRangeSelectionIntoNewTracks(tracksIds, begin, end)) {
        projectHistory()->pushHistoryState("Split into new track", "Split into new track");
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitClipsIntoNewTracks(const ClipKeyList& clipKeyList)
{
    if (clipsInteraction()->splitClipsIntoNewTracks(clipKeyList)) {
        projectHistory()->pushHistoryState("Split into new track", "Split into new track");
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
    projectHistory()->pushHistoryState("Split-cut to the clipboard", "Split cut");
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
    projectHistory()->pushHistoryState("Split-cut to the clipboard", "Split cut");
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

bool TrackeditOperationController::trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                UndoPushType type)
{
    const auto success = clipsInteraction()->trimClipLeft(clipKey, deltaSec, minClipDuration, completed);
    if (success && completed) {
        projectHistory()->pushHistoryState("Clip left trimmed", "Trim clip left", type);
    }
    return success;
}

bool TrackeditOperationController::trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                 UndoPushType type)
{
    const auto success = clipsInteraction()->trimClipRight(clipKey, deltaSec, minClipDuration, completed);
    if (success && completed) {
        projectHistory()->pushHistoryState("Clip right trimmed", "Trim clip right", type);
    }
    return success;
}

bool TrackeditOperationController::stretchClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                   UndoPushType type)
{
    const auto success = clipsInteraction()->stretchClipLeft(clipKey, deltaSec, minClipDuration, completed);
    if (success && completed) {
        projectHistory()->pushHistoryState("Clip left stretched", "Stretch clip left", type);
    }
    return success;
}

bool TrackeditOperationController::stretchClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                    UndoPushType type)
{
    const auto success = clipsInteraction()->stretchClipRight(clipKey, deltaSec, minClipDuration, completed);
    if (success && completed) {
        projectHistory()->pushHistoryState("Clip right stretched", "Stretch clip right", type);
    }
    return success;
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
        projectHistory()->pushHistoryState("Created new audio track", "New track");
        return true;
    }
    return false;
}

bool TrackeditOperationController::newStereoTrack()
{
    if (tracksInteraction()->newStereoTrack()) {
        projectHistory()->pushHistoryState("Created new audio track", "New track");
        return true;
    }
    return false;
}

muse::RetVal<TrackId> TrackeditOperationController::newLabelTrack(const muse::String& title)
{
    return tracksInteraction()->newLabelTrack(title);
}

bool TrackeditOperationController::deleteTracks(const TrackIdList& trackIds)
{
    if (tracksInteraction()->deleteTracks(trackIds)) {
        projectHistory()->pushHistoryState("Delete track", "Delete track");
        return true;
    }
    return false;
}

bool TrackeditOperationController::duplicateTracks(const TrackIdList& trackIds)
{
    if (tracksInteraction()->duplicateTracks(trackIds)) {
        projectHistory()->pushHistoryState("Duplicate track", "Duplicate track");
        return true;
    }
    return false;
}

void TrackeditOperationController::moveTracks(const TrackIdList& trackIds, TrackMoveDirection direction)
{
    if (tracksInteraction()->moveTracks(trackIds, direction)) {
        projectHistory()->pushHistoryState("Move track", "Move track");
    }
}

void TrackeditOperationController::moveTracksTo(const TrackIdList& trackIds, int pos)
{
    if (tracksInteraction()->moveTracksTo(trackIds, pos)) {
        projectHistory()->pushHistoryState("Move track", "Move track");
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
    projectHistory()->pushHistoryState("Clips grouped", "Clips grouped");
}

void TrackeditOperationController::ungroupClips(const trackedit::ClipKeyList& clipKeyList)
{
    clipsInteraction()->ungroupClips(clipKeyList);
    projectHistory()->pushHistoryState("Clips ungrouped", "Clips ungrouped");
}

ClipKeyList TrackeditOperationController::clipsInGroup(int64_t id) const
{
    return clipsInteraction()->clipsInGroup(id);
}

bool TrackeditOperationController::changeTracksFormat(const TrackIdList& tracksIds, trackedit::TrackFormat format)
{
    if (tracksInteraction()->changeTracksFormat(tracksIds, format)) {
        projectHistory()->pushHistoryState("Changed track format", "Changed track format");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeTracksRate(const TrackIdList& tracksIds, int rate)
{
    if (tracksInteraction()->changeTracksRate(tracksIds, rate)) {
        projectHistory()->pushHistoryState("Changed track rate", "Changed track rate");
        return true;
    }
    return false;
}

bool TrackeditOperationController::swapStereoChannels(const TrackIdList& tracksIds)
{
    if (tracksInteraction()->swapStereoChannels(tracksIds)) {
        projectHistory()->pushHistoryState("Swapped stereo channels", "Swapped stereo channels");
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitStereoTracksToLRMono(const TrackIdList& tracksIds)
{
    if (tracksInteraction()->splitStereoTracksToLRMono(tracksIds)) {
        projectHistory()->pushHistoryState("Split stereo tracks to L/R mono", "Split stereo tracks to L/R mono");
        return true;
    }
    return false;
}

bool TrackeditOperationController::splitStereoTracksToCenterMono(const TrackIdList& tracksIds)
{
    if (tracksInteraction()->splitStereoTracksToCenterMono(tracksIds)) {
        projectHistory()->pushHistoryState("Split stereo tracks to center mono", "Split stereo tracks to center mono");
        return true;
    }
    return false;
}

bool TrackeditOperationController::makeStereoTrack(const TrackId left, const TrackId right)
{
    if (tracksInteraction()->makeStereoTrack(left, right)) {
        projectHistory()->pushHistoryState("Make stereo track", "Make stereo track");
        return true;
    }
    return false;
}

bool TrackeditOperationController::resampleTracks(const TrackIdList& tracksIds, int rate)
{
    if (tracksInteraction()->resampleTracks(tracksIds, rate)) {
        projectHistory()->pushHistoryState("Resampled audio track(s)", "Resample track");
        return true;
    }
    return false;
}

muse::RetVal<LabelKey> TrackeditOperationController::addLabel(const TrackId& toTrackId)
{
    muse::RetVal<LabelKey> retVal = labelsInteraction()->addLabel(toTrackId);
    if (retVal.ret) {
        projectHistory()->pushHistoryState("Label added", "Add label");
    }

    return retVal;
}

bool TrackeditOperationController::addLabelToSelection()
{
    if (labelsInteraction()->addLabelToSelection()) {
        projectHistory()->pushHistoryState("Label added", "Add label");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeLabelTitle(const LabelKey& labelKey, const muse::String& title)
{
    if (labelsInteraction()->changeLabelTitle(labelKey, title)) {
        projectHistory()->pushHistoryState("Label title changed", "Change label title");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeLabelLowFrequency(const LabelKey& labelKey, double frequency)
{
    if (labelsInteraction()->changeLabelLowFrequency(labelKey, frequency)) {
        projectHistory()->pushHistoryState("Label low frequency changed", "Change label low frequency");
        return true;
    }
    return false;
}

bool TrackeditOperationController::changeLabelHighFrequency(const LabelKey& labelKey, double frequency)
{
    if (labelsInteraction()->changeLabelHighFrequency(labelKey, frequency)) {
        projectHistory()->pushHistoryState("Label high frequency changed", "Change label high frequency");
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
    projectHistory()->pushHistoryState("Label cut", "Cut label");
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
        projectHistory()->pushHistoryState("Label removed", "Remove label");
        return true;
    }
    return false;
}

bool TrackeditOperationController::removeLabels(const LabelKeyList& labelKeys, bool moveLabels)
{
    if (labelsInteraction()->removeLabels(labelKeys, moveLabels)) {
        projectHistory()->pushHistoryState("Labels removed", "Remove labels");
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

        for (const auto& clipKey : selectedClips) {
            secs_t currentStart = clipsInteraction()->clipStartTime(clipKey);
            clipsInteraction()->changeClipStartTime(clipKey, currentStart + clampedOffset, completed);
        }

        timePositionOffset = clampedOffset;
    }

    bool success = labelsInteraction()->moveLabels(labelKeys, timePositionOffset);
    if (success && completed) {
        const std::string msg = !selectedClips.empty() ? "Items moved" : "Labels moved";
        projectHistory()->pushHistoryState(msg, "Move labels");
    }
    return success;
}

muse::RetVal<LabelKeyList> TrackeditOperationController::moveLabelsToTrack(const LabelKeyList& labelKeys, const TrackId& toTrackId,
                                                                           bool completed)
{
    muse::RetVal<LabelKeyList> retVal = labelsInteraction()->moveLabelsToTrack(labelKeys, toTrackId);
    if (retVal.ret && completed) {
        projectHistory()->pushHistoryState("Labels moved", "Move labels");
    }
    return retVal;
}

bool TrackeditOperationController::stretchLabelLeft(const LabelKey& labelKey, secs_t newStartTime, bool completed)
{
    bool success = labelsInteraction()->stretchLabelLeft(labelKey, newStartTime, completed);
    if (success && completed) {
        projectHistory()->pushHistoryState("Label stretched", "Stretch label left");
    }
    return success;
}

bool TrackeditOperationController::stretchLabelRight(const LabelKey& labelKey, secs_t newEndTime, bool completed)
{
    bool success = labelsInteraction()->stretchLabelRight(labelKey, newEndTime, completed);
    if (success && completed) {
        projectHistory()->pushHistoryState("Label stretched", "Stretch label right");
    }
    return success;
}

muse::Progress TrackeditOperationController::progress() const
{
    return tracksInteraction()->progress();
}

void TrackeditOperationController::pushProjectHistoryJoinState(secs_t start, secs_t duration)
{
    std::stringstream ss;
    ss << "Joined " << duration << " seconds at " << start;
    projectHistory()->pushHistoryState(ss.str(), "Join");
}

void TrackeditOperationController::pushProjectHistoryDuplicateState()
{
    projectHistory()->pushHistoryState("Duplicated", "Duplicate");
}

void TrackeditOperationController::pushProjectHistorySplitDeleteState()
{
    projectHistory()->pushHistoryState("Split-deleted clips", "Split delete");
}

void TrackeditOperationController::pushProjectHistoryDeleteState(secs_t start, secs_t duration)
{
    std::stringstream ss;
    ss << "Delete " << duration << " seconds at " << start;
    projectHistory()->pushHistoryState(ss.str(), "Delete");
}
}
