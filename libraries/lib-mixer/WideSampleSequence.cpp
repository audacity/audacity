/**********************************************************************

Audacity: A Digital Audio Editor

WideSampleSequence.cpp

Paul Licameli split from SampleFrame.cpp

**********************************************************************/
#include "WideSampleSequence.h"
#include <cmath>

WideSampleSequence::~WideSampleSequence() = default;

sampleCount WideSampleSequence::TimeToLongSamples(double t0) const
{
   return sampleCount(floor(t0 * GetRate() + 0.5));
}

double WideSampleSequence::LongSamplesToTime(sampleCount pos) const
{
   return pos.as_double() / GetRate();
}

double WideSampleSequence::SnapToSample(double t) const
{
   return LongSamplesToTime(TimeToLongSamples(t));
}
