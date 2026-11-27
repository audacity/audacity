/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

#include "au3-clip-processing/sequence-graph/ISequenceProcessingNode.h"

#include "stereo.h"

namespace au::au4audio {
//! Its input, rendered once for the several nodes that read it: in stereo
/*!
 Its readers may read it at different positions in a block, as their effects read ahead by their latencies. It renders its
 input in order, keeping what it rendered last; a read of what it doesn't keep, before it or too far after, renders the
 input again from there.
 */
class SharedOutputNode final : public ISequenceProcessingNode
{
public:
    //! @param capacity the most it keeps: at least what a read reads, and as much more as its readers read apart
    //! @param maxFrames the most it renders of its input at once
    SharedOutputNode(std::shared_ptr<ISequenceProcessingNode> input, size_t capacity, size_t maxFrames);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    void render(int64_t end);

    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const size_t m_capacity;
    const size_t m_maxFrames;
    //! What it keeps, by sample index modulo the capacity
    std::vector<float> m_kept[numChannels];
    std::vector<float> m_rendered[numChannels];
    float* m_renderedPointers[numChannels] {};
    //! The indices of what it keeps
    int64_t m_begin = 0;
    int64_t m_end = 0;
};
}
