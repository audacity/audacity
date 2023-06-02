#pragma once

#include "ClientData.h"
#include "SampleCount.h"

class AudioSegment;

class AudioSegmentProcessor
{
public:
   virtual ~AudioSegmentProcessor() = default;
   virtual size_t Process(
      float* const* buffers, size_t numChannels, size_t samplesPerChannel) = 0;
   virtual bool SamplesRemaining() const = 0;
};

class AudioSegment :
    public ClientData::Site<AudioSegment, AudioSegmentProcessor>
{
public:
   using Processor = ClientData::Site<AudioSegment, AudioSegmentProcessor>;
   virtual AudioSegmentProcessor& GetProcessor() const = 0;
   virtual sampleCount GetNumSamples() const = 0;
};
