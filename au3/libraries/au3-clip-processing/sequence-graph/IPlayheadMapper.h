/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-math/SampleCount.h"

#include <cstddef>

/**
 * @brief Maps the linear indices of a stream, e.g. the samples of an audio device, to the positions the playhead reads
 *
 * @details The index of a sample is when it is played, its position what is played then. Seeks and loops make the
 * playhead jump: the positions of contiguous indices are contiguous only within a segment.
 */
class IPlayheadMapper
{
public:
    virtual ~IPlayheadMapper() = default;

    struct Segment {
        //! Where the playhead reads
        sampleCount position;
        size_t len = 0;
    };

    //! Where the playhead reads from `index` on, and for how many samples contiguously, at most `maxLen` and at least one
    virtual Segment Map(sampleCount index, size_t maxLen) const = 0;
};
