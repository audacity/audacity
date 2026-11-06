/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "au3-mixer/ISequenceProcessingNode.h"

namespace au::au4juce {
class MeterSink;
class MeterTapNode;

//! Its input, unchanged; once it has rendered, sends what the meters' taps kept of it to a MeterSink
/*!
 * On top of the master's chain, so that every tap has kept the block when it sends; on the thread that renders it.
 */
class MeterSendNode final : public ISequenceProcessingNode
{
public:
    struct Tap {
        std::shared_ptr<MeterTapNode> node;
        //! How far ahead of the stream index it keeps what is heard there: the tracks', by the master effects' latency
        int64_t lead = 0;
    };

    //! @param taps those of the sink's meters, in the same order
    MeterSendNode(std::shared_ptr<ISequenceProcessingNode> input, std::vector<Tap> taps, MeterSink& sink);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const std::vector<Tap> m_taps;
    MeterSink& m_sink;
    std::vector<const float*> m_channels;
};
}
