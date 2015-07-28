/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackLocation.h

Paul Licameli -- split from WaveTrack.h

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_LOCATION__
#define __AUDACITY_WAVE_TRACK_LOCATION__

struct WaveTrackLocation {

   enum LocationType {
      locationCutLine = 1,
      locationMergePoint
   };

   // Position of track location
   double pos;

   // Type of track location
   LocationType typ;

   // Only for typ==locationMergePoint
   int clipidx1; // first clip (left one)
   int clipidx2; // second clip (right one)
};

#endif
