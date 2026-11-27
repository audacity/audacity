/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>
#include <memory>
#include <vector>

#include "au3-clip-processing/sequence-graph/ISequenceProcessingNode.h"

#include "stereo.h"

namespace au::au4audio {
//! The sum of its tracks' chains, rendered one after the other; in stereo, at most `maxFrames` at once
class SequentialMixNode final : public ISequenceProcessingNode
{
public:
    SequentialMixNode(std::vector<std::shared_ptr<ISequenceProcessingNode> > tracks, size_t maxFrames);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::vector<std::shared_ptr<ISequenceProcessingNode> > m_tracks;
    const size_t m_maxFrames;
    std::vector<float> m_track[numChannels];
    float* m_trackPointers[numChannels] {};
};
}
