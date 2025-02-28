/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveClipUtilities.cpp

  Paul Licameli split from WaveClip.cpp

**********************************************************************/
#include "WaveClipUtilities.h"
#include "WaveClip.h"
#include <cmath>

bool WaveClipUtilities::GetFloatAtTime(const WaveClip& clip,
                                       double t, size_t iChannel, float& value, bool mayThrow)
{
    if (!clip.WithinPlayRegion(t + clip.GetPlayStartTime())) {
        return false;
    }
    const auto start = clip.TimeToSamples(t);
    return clip.GetSamples(
        iChannel, reinterpret_cast<samplePtr>(&value), floatSample, start, 1u,
        mayThrow);
}

void WaveClipUtilities::SetFloatsFromTime(WaveClip& clip,
                                          double t, size_t iChannel, const float* buffer, size_t numFloats,
                                          sampleFormat effectiveFormat)
{
    const auto maybeNegativeStart = clip.TimeToSamples(t);
    const auto maybeOutOfBoundEnd = maybeNegativeStart + numFloats;
    const auto effectiveStart = std::max(sampleCount { 0 }, maybeNegativeStart);
    const auto effectiveEnd
        =std::min(clip.GetVisibleSampleCount(), maybeOutOfBoundEnd);
    if (effectiveStart >= effectiveEnd) {
        return;
    }
    // Cannot be greater than `numFloats` -> safe cast
    const auto effectiveLen = (effectiveEnd - effectiveStart).as_size_t();
    // Cannot be greater than `numFloats` -> safe cast
    const auto numLeadingZeros
        =(effectiveStart - maybeNegativeStart).as_size_t();
    const auto offsetBuffer
        =reinterpret_cast<const char*>(buffer + numLeadingZeros);
    clip.SetSamples(
        iChannel, offsetBuffer, floatSample, effectiveStart, effectiveLen,
        effectiveFormat);
}

bool WaveClipUtilities::SharesBoundaryWithNextClip(
    const WaveTrack::Interval& prev, const WaveTrack::Interval& next)
{
    double endThis = prev.GetRate() * prev.GetPlayStartTime()
                     + prev.GetVisibleSampleCount().as_double() * prev.GetStretchRatio();
    double startNext = next.GetRate() * next.GetPlayStartTime();

    // given that a double has about 15 significant digits, using a criterion
    // of half a sample should be safe in all normal usage.
    return fabs(startNext - endThis) < 0.5;
}
