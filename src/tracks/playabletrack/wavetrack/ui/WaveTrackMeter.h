/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file WaveTrackMeter.cpp
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_WAVE_TRACK_METER__
#define __AUDACITY_WAVE_TRACK_METER__

#include "Meter.h"
#include "TrackAttachment.h"
#include <memory>

class WaveTrack;
class wxDC;
struct wxRect;

class WaveTrackMeter final
   : public Meter
   , public TrackAttachment
   , public std::enable_shared_from_this<WaveTrackMeter>
{
public:
   static WaveTrackMeter &Get(WaveTrack &track);

   WaveTrackMeter();
   ~WaveTrackMeter() override;

   void Draw(wxDC &dc, wxRect rect, bool selected);

   void Update(unsigned numChannels,
      unsigned long numFrames, const float *sampleData, bool interleaved)
   override;
   bool IsDisabled() const override;
   void Clear() override;
   void Reset(double sampleRate, bool resetClipping) override;

private:
   struct Impl;
   const std::unique_ptr<Impl> mpImpl;
   int mLastWidth{ -1 }, mLastHeight{ -1 };
   bool mLastSelected{ false };
};

#endif

