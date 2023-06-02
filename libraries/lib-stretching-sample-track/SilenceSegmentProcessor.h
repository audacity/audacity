#pragma once

#include "AudioSegment.h"
#include "SampleCount.h"

class SilenceSegment;

class SilenceSegmentProcessor : public AudioSegmentProcessor
{
public:
   SilenceSegmentProcessor(const SilenceSegment& segment);

   size_t Process(
      float* const* buffer, size_t numChannels,
      size_t samplesPerChannel) override;

   bool SamplesRemaining() const override;

   static SilenceSegmentProcessor& Get(const SilenceSegment&);

private:
   const SilenceSegment& mSilenceSegment;
   sampleCount mReadPos = 0;
};
