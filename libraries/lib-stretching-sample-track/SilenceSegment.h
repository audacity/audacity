#pragma once

#include "AudioSegment.h"

class SilenceSegment : public AudioSegment
{
public:
   SilenceSegment(sampleCount numSamples);
   AudioSegmentProcessor& GetProcessor() const override;
   sampleCount GetNumSamples() const override;

private:
   const sampleCount mNumSamples;
};
