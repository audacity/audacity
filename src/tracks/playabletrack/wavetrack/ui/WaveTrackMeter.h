/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file WaveTrackMeter.cpp
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_WAVE_TRACK_METER__
#define __AUDACITY_WAVE_TRACK_METER__

#include "TrackAttachment.h"
#include <memory>

class WaveTrack;
class wxDC;
struct wxRect;

class WaveTrackMeter final
   : public TrackAttachment
   , public std::enable_shared_from_this<WaveTrackMeter>
{
public:
   static WaveTrackMeter &Get(WaveTrack &track);

   WaveTrackMeter();
   ~WaveTrackMeter();

   void Draw(wxDC &dc, wxRect rect, bool selected);

private:
   struct Impl;
   const std::unique_ptr<Impl> mpImpl;
   int mLastWidth{ -1 }, mLastHeight{ -1 };
   bool mLastSelected{ false };
};

#endif

