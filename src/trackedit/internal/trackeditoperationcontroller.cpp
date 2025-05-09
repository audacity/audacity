/*
 * Audacity: A Digital Audio Editor
 */
#include "trackeditoperationcontroller.h"
#include "trackediterrors.h"

namespace au::trackedit {
TrackeditOperationController::TrackeditOperationController(std::unique_ptr<IUndoManager> undoManager)
    : m_undoManager{std::move(undoManager)} {}

secs_t TrackeditOperationController::clipStartTime(const ClipKey& clipKey) const
{
    return trackAndClipOperations()->clipStartTime(clipKey);
}

secs_t TrackeditOperationController::clipEndTime(const ClipKey& clipKey) const
{
    return trackAndClipOperations()->clipEndTime(clipKey);
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

bool TrackeditOperationController::changeTrackColor(const TrackId trackId, const std::string& color)
{
    return trackAndClipOperations()->changeTrackColor(trackId, color);
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
    clipboard()->clearTrackData();
}

muse::Ret TrackeditOperationController::pasteFromClipboard(secs_t begin, bool moveClips, bool moveAllTracks)
{
    return trackAndClipOperations()->paste(clipboard()->trackDataCopy(), begin, moveClips, moveAllTracks,
                                           clipboard()->isMultiSelectionCopy());
}

bool TrackeditOperationController::cutClipIntoClipboard(const ClipKey& clipKey)
{
    ITrackDataPtr data = trackAndClipOperations()->cutClip(clipKey);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    projectHistory()->pushHistoryState("Cut to the clipboard", "Cut");
    return true;
}

bool TrackeditOperationController::cutClipDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips)
{
    std::vector<ITrackDataPtr> tracksData(tracksIds.size());
    for (const auto& trackId : tracksIds) {
        const auto data = trackAndClipOperations()->cutTrackData(trackId, begin, end, moveClips);
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
    ITrackDataPtr data = trackAndClipOperations()->copyClip(clipKey);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    return true;
}

bool TrackeditOperationController::copyNonContinuousTrackDataIntoClipboard(const TrackId trackId, const ClipKeyList& clipKeys,
                                                                           secs_t offset)
{
    ITrackDataPtr data = trackAndClipOperations()->copyNonContinuousTrackData(trackId, clipKeys, offset);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    if (clipKeys.size() > 1) {
        clipboard()->setMultiSelectionCopy(true);
    }
    return true;
}

bool TrackeditOperationController::copyContinuousTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end)
{
    ITrackDataPtr data = trackAndClipOperations()->copyContinuousTrackData(trackId, begin, end);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    return true;
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

bool TrackeditOperationController::moveClips(secs_t timePositionOffset, int trackPositionOffset, bool completed)
{
    return trackAndClipOperations()->moveClips(timePositionOffset, trackPositionOffset, completed);
}

bool TrackeditOperationController::splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots)
{
    return trackAndClipOperations()->splitTracksAt(tracksIds, pivots);
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
    ITrackDataPtr data = trackAndClipOperations()->clipSplitCut(clipKey);
    if (!data) {
        return false;
    }
    clipboard()->addTrackData(std::move(data));
    projectHistory()->pushHistoryState("Split-cut to the clipboard", "Split cut");
    return true;
}

bool TrackeditOperationController::clipSplitDelete(const ClipKey& clipKey)
{
    return trackAndClipOperations()->clipSplitDelete(clipKey);
}

bool TrackeditOperationController::splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    std::vector<ITrackDataPtr> tracksData = trackAndClipOperations()->splitCutSelectedOnTracks(tracksIds, begin, end);
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
    return trackAndClipOperations()->splitDeleteSelectedOnTracks(tracksIds, begin, end);
}

bool TrackeditOperationController::trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                UndoPushType type)
{
    return trackAndClipOperations()->trimClipLeft(clipKey, deltaSec, minClipDuration, completed, type);
}

bool TrackeditOperationController::trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                 UndoPushType type)
{
    return trackAndClipOperations()->trimClipRight(clipKey, deltaSec, minClipDuration, completed, type);
}

bool TrackeditOperationController::stretchClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                   UndoPushType type)
{
    return trackAndClipOperations()->stretchClipLeft(clipKey, deltaSec, minClipDuration, completed, type);
}

bool TrackeditOperationController::stretchClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed,
                                                    UndoPushType type)
{
    return trackAndClipOperations()->stretchClipRight(clipKey, deltaSec, minClipDuration, completed, type);
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
