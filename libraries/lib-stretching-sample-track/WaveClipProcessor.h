#pragma once

#include "AudioSegment.h"
#include "SampleCount.h"
#include "TimeAndPitchInterface.h"
#include "WaveClip.h"

class WaveClipProcessor :
    public AudioSegmentProcessor,
    public TimeAndPitchSource
{
public:
   WaveClipProcessor(const WaveClip& clip);
   static WaveClipProcessor& Get(const WaveClip& clip);

   // AudioSegmentProcessor
   void SetOffsetFromPlayStartTime(double) override;
   size_t Process(
      float* const* buffer, size_t numChannels,
      size_t samplesPerChannel) override;
   bool SamplesRemaining() const override;

   // TimeAndPitchSource
   size_t
   Pull(float* const*, size_t numChannels, size_t samplesPerChannel) override;
   bool Empty() const override;

private:
   void StretcherSampleGetterCb(float* const*, size_t samplesPerChannel);
   sampleCount GetRemainingSamplesInClip() const;
   const WaveClip& mClip;
   sampleCount mReadPos = 0;
   std::unique_ptr<TimeAndPitchInterface> mStretcher;
};
