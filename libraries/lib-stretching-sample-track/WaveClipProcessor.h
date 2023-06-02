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
   WaveClipProcessor(const WaveClip& clip, double offsetFromPlayStartTime);
   static WaveClipProcessor& Get(const WaveClip& clip);

   // AudioSegmentProcessor
   size_t Process(
      float* const* buffer, size_t numChannels,
      size_t samplesPerChannel) override;
   bool SamplesRemaining() const override;

   // TimeAndPitchSource
   void
   Pull(float* const*, size_t numChannels, size_t samplesPerChannel) override;

private:
   void StretcherSampleGetterCb(float* const*, size_t samplesPerChannel);
   sampleCount GetRemainingSamplesInClip() const;
   const WaveClip& mClip;
   const sampleCount mTotalNumSamplesToProduce;
   sampleCount mTotalNumSamplesProduced = 0;
   sampleCount mReadPos = 0;
   const std::unique_ptr<TimeAndPitchInterface> mStretcher;
};
