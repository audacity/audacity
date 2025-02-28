/**********************************************************************

Audacity: A Digital Audio Editor

@file WaveTrackLocation.cpp
@brief implements WaveTrackLocations

Paul Licameli -- split from WaveTrack.h

**********************************************************************/
#include "WaveTrackLocation.h"
#include "WaveTrack.h"
#include "WaveClip.h"

#include <cmath>

WaveTrackLocations FindWaveTrackLocations(const WaveTrack& track)
{
    WaveTrackLocations locations;

    auto clips = track.SortedIntervalArray();

    // Count number of display locations
    int num = 0;
    for (const auto clip : clips) {
        num += clip->NumCutLines();
    }

    if (num == 0) {
        return locations;
    }

    // Alloc necessary number of display locations
    locations.reserve(num);

    // Add all display locations to cache
    int curpos = 0;

    for (const auto clip: clips) {
        for (const auto& cc : clip->GetCutLines()) {
            auto cutlinePosition
                =clip->GetSequenceStartTime() + cc->GetSequenceStartTime();
            if (clip->WithinPlayRegion(cutlinePosition)) {
                // Add cut line expander point
                locations.emplace_back(cutlinePosition);
            }
            // If cutline is skipped, we still need to count it
            // so that curpos matches num at the end
            curpos++;
        }
    }

    assert(curpos == num);

    return locations;
}
