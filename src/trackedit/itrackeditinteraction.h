#pragma once

#include "dom/track.h"
#include "modularity/imoduleinterface.h"

#include "global/types/string.h"
#include "global/async/channel.h"
#include "global/progress.h"

#include "trackedittypes.h"
#include "types/ret.h"

namespace au::trackedit {
enum class TrackMoveDirection {
    Up,
    Down,
    Top,
    Bottom
};

//! NOTE Interface for interacting with the project
//! When it gets big, maybe we’ll divide it into several
//! Currently implemented in the au3wrap module
class ITrackeditInteraction : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackeditInteraction)

public:
    virtual ~ITrackeditInteraction() = default;

    virtual secs_t clipStartTime(const ClipKey& clipKey) const = 0;

    //! NOTE Can be called by moving a clip
    //! if the changes is completed, then it is necessary to pass: `completed = true`
    virtual bool changeClipStartTime(const ClipKey& clipKey, secs_t newStartTime, bool completed) = 0;
    virtual muse::async::Channel<ClipKey, secs_t /*newStartTime*/, bool /*completed*/> clipStartTimeChanged() const = 0;

    virtual bool moveClipToTrack(const trackedit::ClipKey& clipKey, TrackId trackId, bool completed) = 0;

    virtual bool trimTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title) = 0;

    virtual bool changeClipTitle(const ClipKey& clipKey, const muse::String& newTitle) = 0;
    virtual bool changeClipPitch(const ClipKey& clipKey, int pitch) = 0;
    virtual bool resetClipPitch(const ClipKey& clipKey) = 0;
    virtual bool changeClipSpeed(const ClipKey& clipKey, double speed) = 0;
    virtual bool resetClipSpeed(const ClipKey& clipKey) = 0;
    virtual bool changeClipColor(const ClipKey& clipKey, const std::string& color) = 0;
    virtual bool changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize) = 0;
    virtual bool renderClipPitchAndSpeed(const ClipKey& clipKey) = 0;
    virtual void clearClipboard() = 0;
    virtual muse::Ret pasteFromClipboard(secs_t begin) = 0;
    virtual bool cutClipIntoClipboard(const ClipKey& clipKey) = 0;
    virtual bool cutClipDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool copyClipIntoClipboard(const ClipKey& clipKey) = 0;
    virtual bool copyClipDataIntoClipboard(const ClipKey& clipKey, secs_t begin, secs_t end) = 0;
    virtual bool copyNonContinuousTrackDataIntoClipboard(const TrackId trackId, const ClipKeyList& clipKeys, secs_t offset) = 0;
    virtual bool copyContinuousTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end) = 0;
    virtual bool removeClip(const ClipKey& clipKey) = 0;
    virtual bool removeClips(const ClipKeyList& clipKeyList) = 0;
    virtual bool removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool moveClips(secs_t timePositionOffset, int trackPositionOffset, bool completed) = 0;
    virtual bool splitTracksAt(const TrackIdList& tracksIds, secs_t pivot) = 0;
    virtual bool mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool duplicateClip(const ClipKey& clipKey) = 0;
    virtual bool duplicateClips(const ClipKeyList& clipKeyList) = 0;
    virtual bool clipSplitCut(const ClipKey& clipKey) = 0;
    virtual bool clipSplitDelete(const ClipKey& clipKey) = 0;
    virtual bool splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;
    virtual bool trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;
    virtual bool stretchClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;
    virtual bool stretchClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;
    virtual secs_t clipDuration(const ClipKey& clipKey) const = 0;
    virtual std::optional<secs_t> getLeftmostClipStartTime(const ClipKeyList& clipKeys) const = 0;

    virtual bool newMonoTrack() = 0;
    virtual bool newStereoTrack() = 0;
    virtual bool newLabelTrack() = 0;
    virtual bool deleteTracks(const TrackIdList& trackIds) = 0;
    virtual bool duplicateTracks(const TrackIdList& trackIds) = 0;
    virtual void moveTracks(const TrackIdList& trackIds, TrackMoveDirection direction) = 0;
    virtual void moveTracksTo(const TrackIdList& trackIds, int pos) = 0;
    virtual bool undo() = 0;
    virtual bool canUndo() = 0;
    virtual bool redo() = 0;
    virtual bool canRedo() = 0;

    virtual bool insertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration) = 0;

    virtual bool toggleStretchToMatchProjectTempo(const ClipKey& clipKey) = 0;

    virtual int64_t clipGroupId(const trackedit::ClipKey& clipKey) const = 0;
    virtual void setClipGroupId(const trackedit::ClipKey& clipKey, int64_t id) = 0;
    virtual void groupClips(const trackedit::ClipKeyList& clipKeyList) = 0;
    virtual void ungroupClips(const trackedit::ClipKeyList& clipKeyList) = 0;
    virtual int64_t determineGroupId(const ClipKeyList& clipKeyList) const = 0;
    virtual ClipKeyList clipsInGroup(int64_t id) const = 0;

    virtual muse::ProgressPtr progress() const = 0;
};
}
