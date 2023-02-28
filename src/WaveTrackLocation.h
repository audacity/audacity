/**********************************************************************

Audacity: A Digital Audio Editor

@file WaveTrackLocation.h
@brief data cache for clip boundaries attached to WaveTrack

Paul Licameli -- split from WaveTrack.h

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_LOCATION__
#define __AUDACITY_WAVE_TRACK_LOCATION__

#include "ClientData.h"

class WaveTrack;

struct WaveTrackLocation {

   enum LocationType {
      locationCutLine = 1,
      locationMergePoint
   };

   explicit
   WaveTrackLocation
   (double pos_ = 0.0, LocationType typ_ = locationCutLine,
    int clipidx1_ = -1, int clipidx2_ = -1)
    : pos(pos_), typ(typ_), clipidx1(clipidx1_), clipidx2(clipidx2_)
   {}

   // Position of track location
   double pos;

   // Type of track location
   LocationType typ;

   // Only for typ==locationMergePoint
   int clipidx1; // first clip (left one)
   int clipidx2; // second clip (right one)
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

class AUDACITY_DLL_API WaveTrackLocations final
   : public ClientData::Cloneable<>
{
   using Location = WaveTrackLocation;
   std::vector <Location> mDisplayLocationsCache;
 
public:
   static WaveTrackLocations &Get( const WaveTrack &track );

   ~WaveTrackLocations() override;
   PointerType Clone() const override;

   // Cache special locations (e.g. cut lines) for later speedy access
   void Update( const WaveTrack &track );

   // Get cached locations
   const std::vector<Location> &Get() const
   { return mDisplayLocationsCache; }
};

#endif
