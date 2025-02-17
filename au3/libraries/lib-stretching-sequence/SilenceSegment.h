/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SilenceSegment.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioSegment.h"

#include "SampleCount.h"

class STRETCHING_SEQUENCE_API SilenceSegment final : public AudioSegment
{
public:
    SilenceSegment(size_t numChannels, sampleCount numSamples);
    size_t GetFloats(float* const* buffers, size_t numSamples) override;
    bool Empty() const override;
    size_t NChannels() const override;

private:
    const size_t mNumChannels;
    sampleCount mNumRemainingSamples;
};
