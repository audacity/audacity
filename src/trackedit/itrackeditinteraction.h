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

    virtual bool trimTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title) = 0;

    virtual bool changeClipTitle(const ClipKey& clipKey, const muse::String& newTitle) = 0;
    virtual bool changeClipPitch(const ClipKey& clipKey, int pitch) = 0;
    virtual bool resetClipPitch(const ClipKey& clipKey) = 0;
    virtual bool changeClipSpeed(const ClipKey& clipKey, double speed) = 0;
    virtual bool resetClipSpeed(const ClipKey& clipKey) = 0;
    virtual bool changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize) = 0;
    virtual void renderClipPitchAndSpeed(const ClipKey& clipKey) = 0;
    virtual void clearClipboard() = 0;
    virtual muse::Ret pasteFromClipboard(secs_t begin, TrackId trackId) = 0;
    virtual bool cutClipIntoClipboard(const ClipKey& clipKey) = 0;
    virtual bool cutClipDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool copyClipIntoClipboard(const ClipKey& clipKey) = 0;
    virtual bool copyClipDataIntoClipboard(const ClipKey& clipKey, secs_t begin, secs_t end) = 0;
    virtual bool copyTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end) = 0;
    virtual bool removeClip(const ClipKey& clipKey) = 0;
    virtual bool removeClipsData(const std::vector<trackedit::ClipKey>& clipsKeys, secs_t begin, secs_t end) = 0;
    virtual bool splitTracksAt(const TrackIdList& tracksIds, secs_t pivot) = 0;
    virtual bool mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool duplicateClip(const ClipKey& clipKey) = 0;
    virtual bool clipSplitCut(const ClipKey& clipKey) = 0;
    virtual bool clipSplitDelete(const ClipKey& clipKey) = 0;
    virtual bool splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, bool completed) = 0;
    virtual bool trimClipRight(const ClipKey& clipKey, secs_t deltaSec, bool completed) = 0;
    virtual void newMonoTrack() = 0;
    virtual void newStereoTrack() = 0;
    virtual void newLabelTrack() = 0;
    virtual void deleteTracks(const TrackIdList& trackIds) = 0;
    virtual void duplicateTracks(const TrackIdList& trackIds) = 0;
    virtual void moveTracks(const TrackIdList& trackIds, TrackMoveDirection direction) = 0;
    virtual void moveTracksTo(const TrackIdList& trackIds, int pos) = 0;
    virtual secs_t clipDuration(const ClipKey& clipKey) const = 0;
    virtual void undo() = 0;
    virtual bool canUndo() = 0;
    virtual void redo() = 0;
    virtual bool canRedo() = 0;

    virtual void toggleStretchEnabled(const ClipKey& clipKey) = 0;

    virtual muse::ProgressPtr progress() const = 0;
};
}
