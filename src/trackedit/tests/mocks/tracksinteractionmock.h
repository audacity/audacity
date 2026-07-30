/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "trackedit/itracksinteraction.h"

namespace au::trackedit {
class TracksInteractionMock : public ITracksInteraction
{
public:
    MOCK_METHOD(bool, trimTracksData, (const std::vector<TrackId>&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, silenceTracksData, (const std::vector<TrackId>&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, tracksDataIsSilent, (const std::vector<TrackId>&, secs_t, secs_t), (const, override));
    MOCK_METHOD(bool, changeTrackTitle, (const TrackId, const muse::String&), (override));

    MOCK_METHOD(bool, changeTracksColor, (const TrackIdList&, ClipColorIndex), (override));

    MOCK_METHOD(muse::Ret, paste, (const std::vector<ITrackDataPtr>&, secs_t, bool, bool, bool, bool&), (override));

    MOCK_METHOD(ITrackDataPtr, cutTrackData, (const TrackId, secs_t, secs_t, bool), (override));
    MOCK_METHOD(ITrackDataPtr, copyNonContinuousTrackData, (const TrackId, const TrackItemKeyList&, secs_t), (override));
    MOCK_METHOD(ITrackDataPtr, copyContinuousTrackData, (const TrackId, secs_t, secs_t), (override));
    MOCK_METHOD(bool, removeTracksData, (const TrackIdList&, secs_t, secs_t, bool), (override));

    MOCK_METHOD(bool, splitTracksAt, (const TrackIdList&, std::vector<secs_t>), (override));
    MOCK_METHOD(bool, splitRangeSelectionAtSilences, (const TrackIdList&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, splitRangeSelectionIntoNewTracks, (const TrackIdList&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, mergeSelectedOnTracks, (const TrackIdList&, secs_t, secs_t), (override));
    MOCK_METHOD(bool, duplicateSelectedOnTracks, (const TrackIdList&, secs_t, secs_t), (override));
    MOCK_METHOD(std::vector<ITrackDataPtr>, splitCutSelectedOnTracks, (const TrackIdList, secs_t, secs_t), (override));
    MOCK_METHOD(bool, splitDeleteSelectedOnTracks, (const TrackIdList, secs_t, secs_t), (override));

    MOCK_METHOD(bool, newMonoTrack, (), (override));
    MOCK_METHOD(bool, newStereoTrack, (), (override));
    MOCK_METHOD(muse::RetVal<TrackId>, newLabelTrack, (const muse::String&), (override));

    MOCK_METHOD(bool, deleteTracks, (const TrackIdList&), (override));
    MOCK_METHOD(bool, duplicateTracks, (const TrackIdList&), (override));
    MOCK_METHOD(bool, moveTracks, (const TrackIdList&, TrackMoveDirection), (override));
    MOCK_METHOD(bool, moveTracksTo, (const TrackIdList&, int), (override));

    MOCK_METHOD(bool, insertSilence, (const TrackIdList&, secs_t, secs_t, secs_t), (override));

    MOCK_METHOD(bool, changeTracksFormat, (const TrackIdList&, TrackFormat), (override));
    MOCK_METHOD(bool, changeTracksRate, (const TrackIdList&, int), (override));

    MOCK_METHOD(bool, swapStereoChannels, (const TrackIdList&), (override));
    MOCK_METHOD(bool, splitStereoTracksToLRMono, (const TrackIdList&), (override));
    MOCK_METHOD(bool, splitStereoTracksToCenterMono, (const TrackIdList&), (override));
    MOCK_METHOD(bool, makeStereoTrack, (const TrackId, const TrackId), (override));
    MOCK_METHOD(bool, resampleTracks, (const TrackIdList&, int), (override));

    MOCK_METHOD(double, nearestZeroCrossing, (double), (const, override));

    MOCK_METHOD(TrackId, addWaveTrack, (int), (override));
    MOCK_METHOD(void, removeDragAddedTracks, (size_t, bool), (override));

    MOCK_METHOD(muse::Progress, progress, (), (const, override));
};
}
