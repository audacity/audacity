/*
 * Audacity: A Digital Audio Editor
 */
#include "trackeditoperationcontroller.h"

namespace au::trackedit {
TrackeditOperationController::TrackeditOperationController(std::unique_ptr<IUndoManager> undoManager)
    : m_undoManager{std::move(undoManager)} {}

secs_t TrackeditOperationController::clipStartTime(const ClipKey& clipKey) const
{
    return trackAndClipOperations()->clipStartTime(clipKey);
}

bool TrackeditOperationController::changeClipStartTime(const ClipKey& clipKey, secs_t newStartTime, bool completed)
{
    return trackAndClipOperations()->changeClipStartTime(clipKey, newStartTime, completed);
}

muse::async::Channel<ClipKey, secs_t /*newStartTime*/, bool /*completed*/> TrackeditOperationController::clipStartTimeChanged() const
{
    return trackAndClipOperations()->clipStartTimeChanged();
}

bool TrackeditOperationController::trimTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->trimTracksData(tracksIds, begin, end);
}

bool TrackeditOperationController::silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->silenceTracksData(tracksIds, begin, end);
}

bool TrackeditOperationController::changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title)
{
    return trackAndClipOperations()->changeTrackTitle(trackId, title);
}

bool TrackeditOperationController::changeClipTitle(const ClipKey& clipKey, const muse::String& newTitle)
{
    return trackAndClipOperations()->changeClipTitle(clipKey, newTitle);
}

bool TrackeditOperationController::changeClipPitch(const ClipKey& clipKey, int pitch)
{
    return trackAndClipOperations()->changeClipPitch(clipKey, pitch);
}

bool TrackeditOperationController::resetClipPitch(const ClipKey& clipKey)
{
    return trackAndClipOperations()->resetClipPitch(clipKey);
}

bool TrackeditOperationController::changeClipSpeed(const ClipKey& clipKey, double speed)
{
    return trackAndClipOperations()->changeClipSpeed(clipKey, speed);
}

bool TrackeditOperationController::resetClipSpeed(const ClipKey& clipKey)
{
    return trackAndClipOperations()->resetClipSpeed(clipKey);
}

bool TrackeditOperationController::changeClipColor(const ClipKey& clipKey, const std::string& color)
{
    return trackAndClipOperations()->changeClipColor(clipKey, color);
}

bool TrackeditOperationController::changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize)
{
    return trackAndClipOperations()->changeClipOptimizeForVoice(clipKey, optimize);
}

bool TrackeditOperationController::renderClipPitchAndSpeed(const ClipKey& clipKey)
{
    return trackAndClipOperations()->renderClipPitchAndSpeed(clipKey);
}

void TrackeditOperationController::clearClipboard()
{
    trackAndClipOperations()->clearClipboard();
}

muse::Ret TrackeditOperationController::pasteFromClipboard(secs_t begin, bool moveClips, bool moveAllTracks)
{
    return trackAndClipOperations()->pasteFromClipboard(begin, moveClips, moveAllTracks);
}

bool TrackeditOperationController::cutClipIntoClipboard(const ClipKey& clipKey)
{
    return trackAndClipOperations()->cutClipIntoClipboard(clipKey);
}

bool TrackeditOperationController::cutClipDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips)
{
    return trackAndClipOperations()->cutClipDataIntoClipboard(tracksIds, begin, end, moveClips);
}

bool TrackeditOperationController::copyClipIntoClipboard(const ClipKey& clipKey)
{
    return trackAndClipOperations()->copyClipIntoClipboard(clipKey);
}

bool TrackeditOperationController::copyNonContinuousTrackDataIntoClipboard(const TrackId trackId, const ClipKeyList& clipKeys,
                                                                           secs_t offset)
{
    return trackAndClipOperations()->copyNonContinuousTrackDataIntoClipboard(trackId, clipKeys, offset);
}

bool TrackeditOperationController::copyContinuousTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->copyContinuousTrackDataIntoClipboard(trackId, begin, end);
}

bool TrackeditOperationController::removeClip(const ClipKey& clipKey)
{
    return trackAndClipOperations()->removeClip(clipKey);
}

bool TrackeditOperationController::removeClips(const ClipKeyList& clipKeyList, bool moveClips)
{
    return trackAndClipOperations()->removeClips(clipKeyList, moveClips);
}

bool TrackeditOperationController::removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips)
{
    return trackAndClipOperations()->removeTracksData(tracksIds, begin, end, moveClips);
}

bool TrackeditOperationController::moveClips(secs_t timePositionOffset, double pixelsPerSecond, int trackPositionOffset, bool completed)
{
    return trackAndClipOperations()->moveClips(timePositionOffset, pixelsPerSecond, trackPositionOffset, completed);
}

bool TrackeditOperationController::splitTracksAt(const TrackIdList& tracksIds, secs_t pivot)
{
    return trackAndClipOperations()->splitTracksAt(tracksIds, pivot);
}

bool TrackeditOperationController::splitClipsAtSilences(const ClipKeyList& clipKeyList)
{
    return trackAndClipOperations()->splitClipsAtSilences(clipKeyList);
}

bool TrackeditOperationController::splitRangeSelectionAtSilences(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->splitRangeSelectionAtSilences(tracksIds, begin, end);
}

bool TrackeditOperationController::splitRangeSelectionIntoNewTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->splitRangeSelectionIntoNewTracks(tracksIds, begin, end);
}

bool TrackeditOperationController::splitClipsIntoNewTracks(const ClipKeyList& clipKeyList)
{
    return trackAndClipOperations()->splitClipsIntoNewTracks(clipKeyList);
}

bool TrackeditOperationController::mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->mergeSelectedOnTracks(tracksIds, begin, end);
}

bool TrackeditOperationController::duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->duplicateSelectedOnTracks(tracksIds, begin, end);
}

bool TrackeditOperationController::duplicateClip(const ClipKey& clipKey)
{
    return trackAndClipOperations()->duplicateClip(clipKey);
}

bool TrackeditOperationController::duplicateClips(const ClipKeyList& clipKeyList)
{
    return trackAndClipOperations()->duplicateClips(clipKeyList);
}

bool TrackeditOperationController::clipSplitCut(const ClipKey& clipKey)
{
    return trackAndClipOperations()->clipSplitCut(clipKey);
}

bool TrackeditOperationController::clipSplitDelete(const ClipKey& clipKey)
{
    return trackAndClipOperations()->clipSplitDelete(clipKey);
}

bool TrackeditOperationController::splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->splitCutSelectedOnTracks(tracksIds, begin, end);
}

bool TrackeditOperationController::splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    return trackAndClipOperations()->splitDeleteSelectedOnTracks(tracksIds, begin, end);
}

bool TrackeditOperationController::trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    return trackAndClipOperations()->trimClipLeft(clipKey, deltaSec, minClipDuration, completed);
}

bool TrackeditOperationController::trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    return trackAndClipOperations()->trimClipRight(clipKey, deltaSec, minClipDuration, completed);
}

bool TrackeditOperationController::stretchClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    return trackAndClipOperations()->stretchClipLeft(clipKey, deltaSec, minClipDuration, completed);
}

bool TrackeditOperationController::stretchClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed)
{
    return trackAndClipOperations()->stretchClipRight(clipKey, deltaSec, minClipDuration, completed);
}

secs_t TrackeditOperationController::clipDuration(const ClipKey& clipKey) const
{
    return trackAndClipOperations()->clipDuration(clipKey);
}

std::optional<secs_t> TrackeditOperationController::getLeftmostClipStartTime(const ClipKeyList& clipKeys) const
{
    return trackAndClipOperations()->getLeftmostClipStartTime(clipKeys);
}

bool TrackeditOperationController::newMonoTrack()
{
    return trackAndClipOperations()->newMonoTrack();
}

bool TrackeditOperationController::newStereoTrack()
{
    return trackAndClipOperations()->newStereoTrack();
}

bool TrackeditOperationController::newLabelTrack()
{
    return trackAndClipOperations()->newLabelTrack();
}

bool TrackeditOperationController::deleteTracks(const TrackIdList& trackIds)
{
    return trackAndClipOperations()->deleteTracks(trackIds);
}

bool TrackeditOperationController::duplicateTracks(const TrackIdList& trackIds)
{
    return trackAndClipOperations()->duplicateTracks(trackIds);
}

void TrackeditOperationController::moveTracks(const TrackIdList& trackIds, TrackMoveDirection direction)
{
    trackAndClipOperations()->moveTracks(trackIds, direction);
}

void TrackeditOperationController::moveTracksTo(const TrackIdList& trackIds, int pos)
{
    trackAndClipOperations()->moveTracksTo(trackIds, pos);
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

bool TrackeditOperationController::insertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration)
{
    return trackAndClipOperations()->insertSilence(trackIds, begin, end, duration);
}

bool TrackeditOperationController::toggleStretchToMatchProjectTempo(const ClipKey& clipKey)
{
    return trackAndClipOperations()->toggleStretchToMatchProjectTempo(clipKey);
}

int64_t TrackeditOperationController::clipGroupId(const trackedit::ClipKey& clipKey) const
{
    return trackAndClipOperations()->clipGroupId(clipKey);
}

void TrackeditOperationController::setClipGroupId(const trackedit::ClipKey& clipKey, int64_t id)
{
    trackAndClipOperations()->setClipGroupId(clipKey, id);
}

void TrackeditOperationController::groupClips(const trackedit::ClipKeyList& clipKeyList)
{
    trackAndClipOperations()->groupClips(clipKeyList);
}

void TrackeditOperationController::ungroupClips(const trackedit::ClipKeyList& clipKeyList)
{
    trackAndClipOperations()->ungroupClips(clipKeyList);
}

ClipKeyList TrackeditOperationController::clipsInGroup(int64_t id) const
{
    return trackAndClipOperations()->clipsInGroup(id);
}

muse::ProgressPtr TrackeditOperationController::progress() const
{
    return trackAndClipOperations()->progress();
}
}
