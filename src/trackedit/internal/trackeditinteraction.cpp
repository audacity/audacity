#include "trackeditinteraction.h"

namespace au::trackedit {
TrackeditInteraction::TrackeditInteraction(std::unique_ptr<ITrackeditInteraction> interaction)
    : m_interaction(std::move(interaction))
{
}

muse::secs_t TrackeditInteraction::clipStartTime(const trackedit::ClipKey& clipKey) const
{
    return m_interaction->clipStartTime(clipKey);
}

bool TrackeditInteraction::changeClipStartTime(const trackedit::ClipKey& clipKey, secs_t newStartTime, bool completed)
{
    return withPlaybackStop(&ITrackeditInteraction::changeClipStartTime, clipKey, newStartTime, completed);
}

muse::async::Channel<trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/> TrackeditInteraction::clipStartTimeChanged() const
{
    return m_interaction->clipStartTimeChanged();
}

bool TrackeditInteraction::trimTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
{
    return withPlaybackStop(&ITrackeditInteraction::trimTracksData, tracksIds, begin, end);
}

bool TrackeditInteraction::silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end)
{
    return withPlaybackStop(&ITrackeditInteraction::silenceTracksData, tracksIds, begin, end);
}

bool TrackeditInteraction::changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title)
{
    return withPlaybackStop(&ITrackeditInteraction::changeTrackTitle, trackId, title);
}

bool TrackeditInteraction::changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle)
{
    return withPlaybackStop(&ITrackeditInteraction::changeClipTitle, clipKey, newTitle);
}

bool TrackeditInteraction::changeClipPitch(const ClipKey& clipKey, int pitch)
{
    return withPlaybackStop(&ITrackeditInteraction::changeClipPitch, clipKey, pitch);
}

bool TrackeditInteraction::resetClipPitch(const ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::resetClipPitch, clipKey);
}

bool TrackeditInteraction::changeClipSpeed(const ClipKey& clipKey, double speed)
{
    return withPlaybackStop(&ITrackeditInteraction::changeClipSpeed, clipKey, speed);
}

bool TrackeditInteraction::resetClipSpeed(const ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::resetClipSpeed, clipKey);
}

bool TrackeditInteraction::changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize)
{
    return withPlaybackStop(&ITrackeditInteraction::changeClipOptimizeForVoice, clipKey, optimize);
}

void TrackeditInteraction::renderClipPitchAndSpeed(const ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::renderClipPitchAndSpeed, clipKey);
}

void TrackeditInteraction::clearClipboard()
{
    return m_interaction->clearClipboard();
}

muse::Ret TrackeditInteraction::pasteFromClipboard(secs_t begin)
{
    return withPlaybackStop(&ITrackeditInteraction::pasteFromClipboard, begin);
}

bool TrackeditInteraction::cutClipIntoClipboard(const ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::cutClipIntoClipboard, clipKey);
}

bool TrackeditInteraction::cutClipDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    return withPlaybackStop(&ITrackeditInteraction::cutClipDataIntoClipboard, tracksIds, begin, end);
}

bool TrackeditInteraction::copyClipIntoClipboard(const trackedit::ClipKey& clipKey)
{
    return m_interaction->copyClipIntoClipboard(clipKey);
}

bool TrackeditInteraction::copyClipDataIntoClipboard(const trackedit::ClipKey& clipKey, secs_t begin, secs_t end)
{
    return m_interaction->copyClipDataIntoClipboard(clipKey, begin, end);
}

bool TrackeditInteraction::copyTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end)
{
    return m_interaction->copyTrackDataIntoClipboard(trackId, begin, end);
}

bool TrackeditInteraction::removeClip(const trackedit::ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::removeClip, clipKey);
}

bool TrackeditInteraction::removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    return withPlaybackStop(&ITrackeditInteraction::removeTracksData, tracksIds, begin, end);
}

bool TrackeditInteraction::moveClips(secs_t offset, bool completed)
{
    return withPlaybackStop(&ITrackeditInteraction::moveClips, offset, completed);
}

bool TrackeditInteraction::splitTracksAt(const TrackIdList& tracksIds, secs_t pivot)
{
    return withPlaybackStop(&ITrackeditInteraction::splitTracksAt, tracksIds, pivot);
}

bool TrackeditInteraction::mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    return withPlaybackStop(&ITrackeditInteraction::mergeSelectedOnTracks, tracksIds, begin, end);
}

bool TrackeditInteraction::duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end)
{
    return withPlaybackStop(&ITrackeditInteraction::duplicateSelectedOnTracks, tracksIds, begin, end);
}

bool TrackeditInteraction::duplicateClip(const ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::duplicateClip, clipKey);
}

bool TrackeditInteraction::clipSplitCut(const ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::clipSplitCut, clipKey);
}

bool TrackeditInteraction::clipSplitDelete(const ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::clipSplitDelete, clipKey);
}

bool TrackeditInteraction::splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    return withPlaybackStop(&ITrackeditInteraction::splitCutSelectedOnTracks, tracksIds, begin, end);
}

bool TrackeditInteraction::splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end)
{
    return withPlaybackStop(&ITrackeditInteraction::splitDeleteSelectedOnTracks, tracksIds, begin, end);
}

bool TrackeditInteraction::trimClipLeft(const trackedit::ClipKey& clipKey, secs_t deltaSec, bool completed)
{
    return withPlaybackStop(&ITrackeditInteraction::trimClipLeft, clipKey, deltaSec, completed);
}

bool TrackeditInteraction::trimClipRight(const trackedit::ClipKey& clipKey, secs_t deltaSec, bool completed)
{
    return withPlaybackStop(&ITrackeditInteraction::trimClipRight, clipKey, deltaSec, completed);
}

muse::secs_t TrackeditInteraction::clipDuration(const trackedit::ClipKey& clipKey) const
{
    return m_interaction->clipDuration(clipKey);
}

void TrackeditInteraction::newMonoTrack()
{
    return withPlaybackStop(&ITrackeditInteraction::newMonoTrack);
}

void TrackeditInteraction::newStereoTrack()
{
    return withPlaybackStop(&ITrackeditInteraction::newStereoTrack);
}

void TrackeditInteraction::newLabelTrack()
{
    return withPlaybackStop(&ITrackeditInteraction::newLabelTrack);
}

void TrackeditInteraction::deleteTracks(const TrackIdList& trackIds)
{
    return withPlaybackStop(&ITrackeditInteraction::deleteTracks, trackIds);
}

void TrackeditInteraction::duplicateTracks(const TrackIdList& trackIds)
{
    return withPlaybackStop(&ITrackeditInteraction::duplicateTracks, trackIds);
}

void TrackeditInteraction::moveTracks(const TrackIdList& trackIds, const TrackMoveDirection direction)
{
    return m_interaction->moveTracks(trackIds, direction);
}

void TrackeditInteraction::moveTracksTo(const TrackIdList& trackIds, int to)
{
    return m_interaction->moveTracksTo(trackIds, to);
}

void TrackeditInteraction::undo()
{
    return withPlaybackStop(&ITrackeditInteraction::undo);
}

bool TrackeditInteraction::canUndo()
{
    return m_interaction->canUndo();
}

void TrackeditInteraction::redo()
{
    return withPlaybackStop(&ITrackeditInteraction::redo);
}

bool TrackeditInteraction::canRedo()
{
    return m_interaction->canRedo();
}

void TrackeditInteraction::toggleStretchToMatchProjectTempo(const ClipKey& clipKey)
{
    return withPlaybackStop(&ITrackeditInteraction::toggleStretchToMatchProjectTempo, clipKey);
}

muse::ProgressPtr TrackeditInteraction::progress() const
{
    return m_interaction->progress();
}
}
