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

const WideSampleSequence& WideSampleSequence::GetDecorated() const
{
   const WideSampleSequence* innermost = this;
   while (const auto newP = innermost->DoGetDecorated())
      innermost = newP;
   return *innermost;
}

const WideSampleSequence* WideSampleSequence::DoGetDecorated() const
{
   return nullptr;
}
