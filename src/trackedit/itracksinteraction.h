/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "dom/track.h"

#include "global/types/string.h"
#include "global/types/ret.h"
#include "global/progress.h"

#include "trackedittypes.h"
#include "itrackdata.h"

#include "modularity/imoduleinterface.h"

namespace au::trackedit {
class ITracksInteraction : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITracksInteraction)

public:
    virtual ~ITracksInteraction() = default;

    virtual bool trimTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title) = 0;

    virtual bool changeTracksColor(const TrackIdList& trackId, const std::string& color) = 0;

    virtual muse::Ret paste(const std::vector<ITrackDataPtr>& data, secs_t begin, bool moveClips, bool moveAllTracks,
                            bool isMultiSelectionCopy, bool& modifiedState) = 0;

    virtual ITrackDataPtr cutTrackData(const TrackId trackId, secs_t begin, secs_t end, bool moveClips) = 0;
    virtual ITrackDataPtr copyNonContinuousTrackData(const TrackId trackId, const ClipKeyList& clipKeys, secs_t offset) = 0;
    virtual ITrackDataPtr copyContinuousTrackData(const TrackId trackId, secs_t begin, secs_t end) = 0;
    virtual bool removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips) = 0;

    virtual bool splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots) = 0;
    virtual bool splitRangeSelectionAtSilences(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool splitRangeSelectionIntoNewTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) = 0;
    virtual std::vector<ITrackDataPtr> splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) = 0;
    virtual bool splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) = 0;

    virtual bool newMonoTrack() = 0;
    virtual bool newStereoTrack() = 0;
    virtual bool newLabelTrack() = 0;
    virtual bool deleteTracks(const TrackIdList& trackIds) = 0;
    virtual bool duplicateTracks(const TrackIdList& trackIds) = 0;
    virtual bool moveTracks(const TrackIdList& trackIds, TrackMoveDirection direction) = 0;
    virtual bool moveTracksTo(const TrackIdList& trackIds, int pos) = 0;

    virtual bool insertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration) = 0;

    virtual bool changeTracksFormat(const TrackIdList& tracksIds, trackedit::TrackFormat format) = 0;
    virtual bool changeTracksRate(const TrackIdList& tracksIds, int rate) = 0;

    virtual bool swapStereoChannels(const TrackIdList& tracksIds) = 0;
    virtual bool splitStereoTracksToLRMono(const TrackIdList& tracksIds) = 0;
    virtual bool splitStereoTracksToCenterMono(const TrackIdList& tracksIds) = 0;
    virtual bool makeStereoTrack(const TrackId left, const TrackId right) = 0;
    virtual bool resampleTracks(const TrackIdList& tracksIds, int rate) = 0;

    virtual double nearestZeroCrossing(double time) const = 0;

    virtual muse::Progress progress() const = 0;
};
}
