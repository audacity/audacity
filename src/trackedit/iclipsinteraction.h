/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "global/types/string.h"
#include "global/types/secs.h"
#include "global/types/ret.h"
#include "global/async/channel.h"
#include "global/progress.h"

#include "trackedittypes.h"
#include "timespan.h"
#include "itrackdata.h"

#include "modularity/imoduleinterface.h"

namespace au::trackedit {
class IClipsInteraction : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IClipsInteraction)

public:
    virtual ~IClipsInteraction() = default;

    virtual secs_t clipStartTime(const ClipKey& clipKey) const = 0;
    virtual secs_t clipEndTime(const trackedit::ClipKey& clipKey) const = 0;
    virtual secs_t clipDuration(const ClipKey& clipKey) const = 0;

    virtual bool changeClipStartTime(const ClipKey& clipKey, secs_t newStartTime, bool completed) = 0;
    virtual muse::async::Channel<ClipKey, secs_t /*newStartTime*/, bool /*completed*/> clipStartTimeChanged() const = 0;

    virtual bool changeClipTitle(const ClipKey& clipKey, const muse::String& newTitle) = 0;
    virtual bool changeClipPitch(const ClipKey& clipKey, int pitch) = 0;
    virtual bool resetClipPitch(const ClipKey& clipKey) = 0;
    virtual bool changeClipSpeed(const ClipKey& clipKey, double speed) = 0;
    virtual bool resetClipSpeed(const ClipKey& clipKey) = 0;
    virtual bool changeClipColor(const ClipKey& clipKey, const std::string& color) = 0;
    virtual bool changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize) = 0;
    virtual bool renderClipPitchAndSpeed(const ClipKey& clipKey) = 0;

    virtual ITrackDataPtr cutClip(const ClipKey& clipKey) = 0;
    virtual ITrackDataPtr copyClip(const ClipKey& clipKey) = 0;
    virtual std::optional<TimeSpan> removeClip(const ClipKey& clipKey) = 0;
    virtual bool removeClips(const ClipKeyList& clipKeyList, bool moveClips) = 0;
    virtual muse::RetVal<ClipKeyList> moveClips(const ClipKeyList& clipKeyList, secs_t timePositionOffset, int trackPositionOffset,
                                                bool completed, bool& clipsMovedToOtherTracks) = 0;
    virtual void cancelClipDragEdit() = 0;

    virtual bool splitClipsAtSilences(const ClipKeyList& clipKeyList) = 0;
    virtual bool splitClipsIntoNewTracks(const ClipKeyList& clipKeyList) = 0;

    virtual bool duplicateClip(const ClipKey& clipKey) = 0;
    virtual bool duplicateClips(const ClipKeyList& clipKeyList) = 0;
    virtual ITrackDataPtr clipSplitCut(const ClipKey& clipKey) = 0;
    virtual bool clipSplitDelete(const ClipKey& clipKey) = 0;

    virtual bool trimClipsLeft(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;
    virtual bool trimClipsRight(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;

    virtual bool stretchClipsLeft(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;
    virtual bool stretchClipsRight(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;

    virtual muse::Ret makeRoomForClip(const trackedit::ClipKey& clipKey) = 0;

    virtual ClipKeyList clipsOnTrack(const trackedit::TrackId trackId) = 0;

    virtual bool toggleStretchToMatchProjectTempo(const ClipKey& clipKey) = 0;

    virtual int64_t clipGroupId(const trackedit::ClipKey& clipKey) const = 0;
    virtual void setClipGroupId(const trackedit::ClipKey& clipKey, int64_t id) = 0;
    virtual void groupClips(const trackedit::ClipKeyList& clipKeyList) = 0;
    virtual void ungroupClips(const trackedit::ClipKeyList& clipKeyList) = 0;
    virtual ClipKeyList clipsInGroup(int64_t id) const = 0;

    virtual muse::Progress progress() const = 0;

    //! TODO
    virtual bool clipTransferNeedsDownmixing(const std::vector<ITrackDataPtr>& srcTracks, const TrackIdList& dstTracks) const = 0;
    virtual bool userIsOkWithDownmixing() const = 0;
    virtual muse::Ret makeRoomForClipsOnTracks(const std::vector<TrackId>& tracksIds, const std::vector<ITrackDataPtr>& trackData,
                                               muse::secs_t begin) = 0;
    virtual muse::Ret makeRoomForDataOnTrack(const TrackId trackId, muse::secs_t begin, muse::secs_t end) = 0;
    virtual bool singleClipOnTrack(const TrackId trackId) const = 0;
};
}
