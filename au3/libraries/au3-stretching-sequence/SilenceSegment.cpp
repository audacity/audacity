/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SilenceSegment.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "SilenceSegment.h"

#include <algorithm>
#include <cassert>

SilenceSegment::SilenceSegment(
    size_t numChannels, sampleCount unitySamples,
    PlaybackTempoScale::Ptr tempoScale)
    : mNumChannels{numChannels}
    , mTempoScale{tempoScale ? std::move(tempoScale) : PlaybackTempoScale::Create()}
    , mLastTempoScale{std::max(0.01, PlaybackTempoScale::Get(mTempoScale))}
    , mNumRemainingSamples{sampleCount {
                                std::max(0.0, unitySamples.as_double() / mLastTempoScale) + .5 }}
{
}

size_t
SilenceSegment::GetFloats(float* const* buffers, size_t numSamples)
{
    // Preserve-pitch Play-at-Speed: rescale remaining silence when tempo changes
    // while the playhead is in a gap between clips.
    const double tempoScale = std::max(0.01, PlaybackTempoScale::Get(mTempoScale));
    if (tempoScale != mLastTempoScale) {
        mNumRemainingSamples = sampleCount {
            std::max(0.0, mNumRemainingSamples.as_double() * mLastTempoScale / tempoScale)
            + .5
        };
        mLastTempoScale = tempoScale;
    }

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
