/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>
#include <memory>
#include <vector>

#include "au3-mixer/ISequenceProcessingNode.h"

#include "tracktiongraphincludes.h"

namespace au::au4tracktion {
//! Tracktion's graph as one of our nodes: the sum of its tracks' chains, rendered in parallel on tracktion_graph's thread
//! pool
/*!
 * Runs a LockFreeMultiThreadedNodePlayer over a SummingNode of AudacityToTracktionNodes, by stream index: the reading
 * thread renders with the pool, and returns once the pool is done. In stereo, at most `maxFrames` at once.
 */
class TracktionToAudacityNode final : public ISequenceProcessingNode
{
public:
    TracktionToAudacityNode(std::vector<std::shared_ptr<ISequenceProcessingNode> > tracks, double sampleRate, size_t maxFrames);
    ~TracktionToAudacityNode() override;

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const size_t m_maxFrames;
    std::unique_ptr<tracktion::graph::LockFreeMultiThreadedNodePlayer> m_player;
    choc::buffer::ChannelArrayBuffer<float> m_buffer;
    tracktion_engine::MidiMessageArray m_midi;
};
}
