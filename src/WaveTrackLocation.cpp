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

WaveTrackLocations::~WaveTrackLocations() = default;

auto WaveTrackLocations::Clone() const -> PointerType
{
   return std::make_unique<WaveTrackLocations>(*this);
}

void WaveTrackLocations::Update( const WaveTrack &track )
{
   auto clips = track.SortedClipArray();

   mDisplayLocationsCache.clear();

   // Count number of display locations
   int num = 0;
   {
      const WaveClip *prev = nullptr;
      for (const auto clip : clips)
      {
         num += clip->NumCutLines();

         if (prev && fabs(prev->GetPlayEndTime() -
            clip->GetPlayStartTime()) < WAVETRACK_MERGE_POINT_TOLERANCE)
            ++num;

         prev = clip;
      }
   }

   if (num == 0)
      return;

   // Alloc necessary number of display locations
   mDisplayLocationsCache.reserve(num);

   // Add all display locations to cache
   int curpos = 0;

   const WaveClip *previousClip = nullptr;
   for (const auto clip: clips)
   {
      for (const auto &cc : clip->GetCutLines())
      {
         auto cutlinePosition = clip->GetSequenceStartTime() + cc->GetSequenceStartTime();
         if (clip->WithinPlayRegion(cutlinePosition))
         {
             // Add cut line expander point
             mDisplayLocationsCache.push_back(WaveTrackLocation{
                cutlinePosition,
                WaveTrackLocation::locationCutLine
             });
         }
         // If cutline is skipped, we still need to count it
         // so that curpos match num at the end
         curpos++;
      }

     if (previousClip)
      {
         if (fabs(previousClip->GetPlayEndTime() - clip->GetPlayStartTime())
                                          < WAVETRACK_MERGE_POINT_TOLERANCE)
         {
            // Add merge point
            mDisplayLocationsCache.push_back(WaveTrackLocation{
               previousClip->GetPlayEndTime(),
               WaveTrackLocation::locationMergePoint,
               track.GetClipIndex(previousClip),
               track.GetClipIndex(clip)
            });
            curpos++;
         }
      }

      previousClip = clip;
   }

   wxASSERT(curpos == num);
}

static WaveTrack::Attachments::RegisteredFactory sKey{ []( SampleTrack& ){
   return std::make_unique< WaveTrackLocations >();
} };

WaveTrackLocations &WaveTrackLocations::Get( const WaveTrack &track )
{
   // const_cast the track to get this cache of mutable attached display data
   return const_cast< WaveTrack& >( track )
      .Attachments::Get< WaveTrackLocations >( sKey );
}

