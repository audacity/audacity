/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrack.cpp

Paul Licameli split from WaveTrack.cpp

**********************************************************************/

#include "SampleTrack.h"

SampleTrack::~SampleTrack() = default;

static const Track::TypeInfo &typeInfo()
{
   static const Track::TypeInfo info{
      { "sample", "sample", XO("Sample Track") },
      true, &PlayableTrack::ClassTypeInfo() };
   return info;
}

auto SampleTrack::ClassTypeInfo() -> const TypeInfo &
{
   return typeInfo();
}

auto SampleTrack::GetTypeInfo() const -> const TypeInfo &
{
   return typeInfo();
}

sampleCount SampleTrack::TimeToLongSamples(double t0) const
{
   return sampleCount( floor(t0 * GetRate() + 0.5) );
}

double SampleTrack::LongSamplesToTime(sampleCount pos) const
{
   return pos.as_double() / GetRate();
}
