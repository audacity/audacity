/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SilenceSegment.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "SilenceSegment.h"

#include <algorithm>
#include <cassert>

SilenceSegment::SilenceSegment(size_t numChannels, sampleCount numSamples)
    : mNumChannels{numChannels}
    , mNumRemainingSamples{numSamples}
{
}

size_t
SilenceSegment::GetFloats(float* const* buffers, size_t numSamples)
{
    const size_t numSamplesToProduce
        =std::min<long long>(mNumRemainingSamples.as_long_long(), numSamples);
    for (auto i = 0u; i < mNumChannels; ++i) {
        auto buffer = buffers[i];
        std::fill(buffer, buffer + numSamplesToProduce, 0.f);
    }
    mNumRemainingSamples -= numSamplesToProduce;
    return numSamplesToProduce;
}

bool SilenceSegment::Empty() const
{
    return mNumRemainingSamples == 0u;
}

size_t SilenceSegment::NChannels() const
{
    return mNumChannels;
}
