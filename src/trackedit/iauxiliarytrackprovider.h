/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <vector>

#include "global/async/notifylist.h"
#include "global/modularity/imoduleinterface.h"
#include "global/types/retval.h"

#include "dom/track.h"
#include "timespan.h"
#include "trackedittypes.h"

namespace au::trackedit {
class IAuxiliaryTrackProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAuxiliaryTrackProvider)

public:
    ~IAuxiliaryTrackProvider() override = default;

    virtual TrackIdList trackIdList() const = 0;
    virtual TrackList trackList() const = 0;
    virtual std::optional<Track> track(TrackId trackId) const = 0;
    virtual bool hasTrack(TrackId trackId) const = 0;

    virtual Clip clip(const ClipKey& key) const = 0;
    virtual muse::async::NotifyList<Clip> clipList(TrackId trackId) const = 0;
    virtual bool hasClip(const ClipKey& key) const = 0;
    virtual ClipKeyList clipsOnTrack(TrackId trackId) const = 0;

    virtual std::vector<int64_t> groupsIdsList() const = 0;
    virtual int64_t clipGroupId(const ClipKey& clipKey) const = 0;
    virtual void setClipGroupId(const ClipKey& clipKey, int64_t id) = 0;
    virtual ClipKeyList clipsInGroup(int64_t id) const = 0;

    virtual secs_t totalTime() const = 0;
    virtual secs_t clipStartTime(const ClipKey& clipKey) const = 0;
    virtual secs_t clipEndTime(const ClipKey& clipKey) const = 0;
    virtual secs_t clipDuration(const ClipKey& clipKey) const = 0;
    virtual bool changeClipStartTime(const ClipKey& clipKey, secs_t newStartTime, bool completed) = 0;
    virtual bool changeClipTitle(const ClipKey& clipKey, const muse::String& newTitle) = 0;
    virtual bool changeClipColor(const ClipKey& clipKey, ClipColorIndex colorIndex) = 0;

    virtual std::optional<TimeSpan> removeClip(const ClipKey& clipKey) = 0;
    virtual bool removeClips(const ClipKeyList& clipKeyList, bool moveClips) = 0;
    virtual muse::RetVal<ClipKeyList> moveClips(const ClipKeyList& clipKeyList, secs_t timePositionOffset, int trackPositionOffset,
                                                bool completed, bool& clipsMovedToOtherTracks) = 0;
    virtual bool trimClipsLeft(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;
    virtual bool trimClipsRight(const ClipKeyList& clipKeyList, secs_t deltaSec, secs_t minClipDuration, bool completed) = 0;
    virtual bool singleClipOnTrack(TrackId trackId) const = 0;

    virtual bool removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips) = 0;
    virtual bool splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots) = 0;
    virtual bool deleteTracks(const TrackIdList& trackIds) = 0;
    virtual bool changeTrackTitle(TrackId trackId, const muse::String& title) = 0;
};
}
