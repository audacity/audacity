/**********************************************************************

Audacity: A Digital Audio Editor

@file WaveTrackLocation.h
@brief finds clip and cutline boundaries attached to WaveTrack

Paul Licameli -- split from WaveTrack.h

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_LOCATION__
#define __AUDACITY_WAVE_TRACK_LOCATION__

#include <vector>

class WaveTrack;

struct WaveTrackLocation {

   enum LocationType {
      locationCutLine = 1,
      locationMergePoint
   };

   WaveTrackLocation() = default;

   WaveTrackLocation(double pos, LocationType typ,
    int clipidx1 = -1, int clipidx2 = -1
   )  : pos{ pos }, typ{ typ }, clipidx1{ clipidx1 }, clipidx2{ clipidx2 }
   {}

   // Position of track location
   double pos{ 0.0 };

   // Type of track location
   LocationType typ{ locationCutLine };

   // Only for typ==locationMergePoint
   int clipidx1{ -1 }; // first clip (left one)
   int clipidx2{ -1 }; // second clip (right one)
};

inline
bool operator == (const WaveTrackLocation &a, const WaveTrackLocation &b)
{
   return a.pos == b.pos &&
   a.typ == b.typ &&
   a.clipidx1 == b.clipidx1 &&
   a.clipidx2 == b.clipidx2;
}

inline
bool operator != (const WaveTrackLocation &a, const WaveTrackLocation &b)
{
   return !( a == b );
}

using WaveTrackLocations = std::vector<WaveTrackLocation>;

AUDACITY_DLL_API
WaveTrackLocations FindWaveTrackLocations(const WaveTrack &track);

#endif
