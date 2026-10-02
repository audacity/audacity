/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SilenceSegment.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioSegment.h"
#include "PlaybackTempoScale.h"

#include "au3-math/SampleCount.h"

class STRETCHING_SEQUENCE_API SilenceSegment final : public AudioSegment
{
public:
    //! @param unitySamples Gap length at tempo scale 1.0 (project timeline samples).
    SilenceSegment(
        size_t numChannels, sampleCount unitySamples,
        PlaybackTempoScale::Ptr tempoScale = PlaybackTempoScale::Create());
    size_t GetFloats(float* const* buffers, size_t numSamples) override;
    bool Empty() const override;
    size_t NChannels() const override;

private:
    const size_t mNumChannels;
    PlaybackTempoScale::Ptr mTempoScale;
    double mLastTempoScale = 1.0;
    sampleCount mNumRemainingSamples;
};
