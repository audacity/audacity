/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegment.h

  Matthieu Hodgkinson

**********************************************************************/

#pragma once

#include "SampleCount.h"

#include <vector>

/**
 * @brief A generalization for audio segments, whether clips or silence between
 * clips.
 */
class STRETCHING_SEQUENCE_API AudioSegment
{
public:
    virtual ~AudioSegment();

    /**
     * @brief Fills `buffers` with as many as `numSamples` or the number of
     * remaining samples, whichever is smaller.
     * @param buffers Pointers to buffers, one for each channel.
     * @param numSamples The max. number of samples to write to each buffer.
     * @return The number of samples actually provided in each buffer.
     */
    virtual size_t
    GetFloats(float* const* buffers, size_t numSamples) = 0;

    /**
     * @brief The number of channels in the segment.
     */
    virtual size_t NChannels() const = 0;

    /**
     * @brief Whether the segment has no more samples to provide.
     */
    virtual bool Empty() const = 0;
};
