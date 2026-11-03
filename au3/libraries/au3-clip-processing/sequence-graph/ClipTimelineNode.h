/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "clip-graph/IClipProcessingNode.h"
#include "ISequenceProcessingNode.h"

#include <memory>
#include <vector>

class ClipInterface;

//! The clips of a sequence at their places in time, and silence between them
class ClipTimelineNode final : public ISequenceProcessingNode
{
public:
    //! A clip, and the end node of the graph processing it
    struct ClipAndGraph {
        std::shared_ptr<const ClipInterface> clip;
        std::unique_ptr<IClipProcessingNode> graph;
    };

    ClipTimelineNode(std::vector<ClipAndGraph> clips, size_t numChannels, double rate);

    // ISequenceProcessingNode
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::vector<ClipAndGraph> mClips;
    const double mRate;
    const size_t mNumChannels;
    std::vector<float*> mOffsetBuffers;
};
