/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>
#include <vector>

#include "au3wrap/au3types.h"
#include "trackedit/trackedittypes.h"

class WritableSampleTrack;

namespace au::record {
constexpr int RATE_NOT_SELECTED = -1;
using WritableSampleTrackArray = std::vector< std::shared_ptr< WritableSampleTrack > >;

struct PropertiesOfSelected
{
    bool allSameRate = false;
    int rateOfSelected = RATE_NOT_SELECTED;
    bool anySelected = false;
};

PropertiesOfSelected GetPropertiesOfSelected(const au3::Au3Project& proj);

WritableSampleTrackArray ChooseExistingRecordingTracks(au3::Au3Project& proj, bool selectedOnly, double targetRate, int recordingChannels);

//! The tracks a recording goes into: the selected tracks, or the focused track when no track is selected.
//! With more input channels than the focused track has, the extra channels are dropped.
WritableSampleTrackArray ChooseRecordingTracks(au3::Au3Project& proj, trackedit::TrackId focusedTrackId, int recordingChannels);
}
