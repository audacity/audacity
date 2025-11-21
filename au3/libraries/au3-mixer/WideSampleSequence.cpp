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

bool WideSampleSequence::GetFloats(size_t iChannel, size_t nBuffers,
                                   float* const buffers[], sampleCount start, size_t len,
                                   bool backwards, fillFormat fill,
                                   bool mayThrow, sampleCount* pNumWithinClips) const
{
    // Cast the pointers to pass them to DoGet() which handles multiple
    // destination formats
    const auto castBuffers = reinterpret_cast<const samplePtr*>(buffers);
    const auto result = DoGet(
        iChannel, nBuffers, castBuffers,
        floatSample, start, len, backwards, fill, mayThrow, pNumWithinClips);
    if (!result) {
        while (nBuffers--) {
            ClearSamples(castBuffers[nBuffers], floatSample, 0, len);
        }
    }
    return result;
}
