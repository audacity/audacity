#include "SilenceSegment.h"
#include "SilenceSegmentProcessor.h"

AudioSegmentProcessor& SilenceSegment::GetProcessor() const
{
   return SilenceSegmentProcessor::Get(*this);
}

SilenceSegment::SilenceSegment(sampleCount numSamples)
    : mNumSamples(numSamples)
{
}

sampleCount SilenceSegment::GetNumSamples() const
{
   return mNumSamples;
}
