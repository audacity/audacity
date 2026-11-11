/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-math/SampleCount.h"

#include <cstddef>

//! A node of the graph processing a sequence as it is played, i.e. all its clips together
/*!
 * Positions are in samples, from the sequence's time 0, at its rate.
 */
class ISequenceProcessingNode
{
public:
    virtual ~ISequenceProcessingNode() = default;

    //! Fills `buffers`, one per channel, with the `len` samples from `start`
    virtual void GetSamples(float* const* buffers, sampleCount start, size_t len) = 0;
};
