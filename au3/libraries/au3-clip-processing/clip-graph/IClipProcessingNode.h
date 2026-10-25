/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-math/SampleCount.h"

#include <cstddef>

//! A node of the graph processing a clip as it is played
/*!
 * Positions are in play samples, from the clip's play start, at the rate of the sequence the clip is in.
 */
class IClipProcessingNode
{
public:
    virtual ~IClipProcessingNode() = default;

    //! Fills `buffers`, one per channel, with the `len` samples from `start`
    virtual void GetSamples(float* const* buffers, sampleCount start, size_t len) = 0;

    virtual sampleCount NumSamples() const = 0;
    virtual size_t NChannels() const = 0;
};
