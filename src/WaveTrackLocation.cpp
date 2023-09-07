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

WaveTrackLocations FindWaveTrackLocations(const WaveTrack &track)
{
   WaveTrackLocations locations;

   auto clips = track.SortedClipArray();

   // Count number of display locations
   int num = 0;
   {
      const WaveClip *prev = nullptr;
      for (const auto clip : clips) {
         num += clip->NumCutLines();

         if (prev && fabs(prev->GetPlayEndTime() -
            clip->GetPlayStartTime()) < WAVETRACK_MERGE_POINT_TOLERANCE)
            ++num;

         prev = clip;
      }
   }

   if (num == 0)
      return locations;

   // Alloc necessary number of display locations
   locations.reserve(num);

   // Add all display locations to cache
   int curpos = 0;

   const WaveClip *previousClip = nullptr;
   for (const auto clip: clips) {
      for (const auto &cc : clip->GetCutLines()) {
         auto cutlinePosition =
            clip->GetSequenceStartTime() + cc->GetSequenceStartTime();
         if (clip->WithinPlayRegion(cutlinePosition)) {
             // Add cut line expander point
             locations.emplace_back(cutlinePosition,
                WaveTrackLocation::locationCutLine);
         }
         // If cutline is skipped, we still need to count it
         // so that curpos matches num at the end
         curpos++;
      }

      if (previousClip) {
         if (fabs(previousClip->GetPlayEndTime() - clip->GetPlayStartTime())
            < WAVETRACK_MERGE_POINT_TOLERANCE
         ) {
            // Add merge point
            locations.emplace_back(previousClip->GetPlayEndTime(),
               WaveTrackLocation::locationMergePoint,
               track.GetClipIndex(previousClip),
               track.GetClipIndex(clip)
            );
            curpos++;
         }
      }

      previousClip = clip;
   }

   assert(curpos == num);

   return locations;
}

