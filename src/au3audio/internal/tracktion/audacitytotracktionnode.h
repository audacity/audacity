/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "tracktiongraphincludes.h"

class ISequenceProcessingNode;

namespace au::au4tracktion {
//! One of our nodes as a node of tracktion's graph: it renders, by stream index, what its input renders
/*!
 * Reports no latency: our nodes compensate their own, so that tracktion adds no latency nodes, which would keep state
 * across jumps. In stereo.
 */
class AudacityToTracktionNode final : public tracktion::graph::Node
{
public:
    AudacityToTracktionNode(std::shared_ptr<ISequenceProcessingNode> input, size_t nodeId);
    ~AudacityToTracktionNode() override;

    tracktion::graph::NodeProperties getNodeProperties() override;
    void prepareToPlay(const tracktion::graph::PlaybackInitialisationInfo&) override {}
    bool isReadyToProcess() override { return true; }
    void process(tracktion::graph::Node::ProcessContext& pc) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const size_t m_nodeId;
};
}
