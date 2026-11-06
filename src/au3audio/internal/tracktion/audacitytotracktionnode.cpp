/*
* Audacity: A Digital Audio Editor
*/
#include "audacitytotracktionnode.h"

#include "au3-mixer/ISequenceProcessingNode.h"

#include "../juce/stereo.h"

using namespace au::au4tracktion;

AudacityToTracktionNode::AudacityToTracktionNode(std::shared_ptr<ISequenceProcessingNode> input, size_t nodeId)
    : m_input{std::move(input)}, m_nodeId{nodeId}
{
}

AudacityToTracktionNode::~AudacityToTracktionNode() = default;

tracktion::graph::NodeProperties AudacityToTracktionNode::getNodeProperties()
{
    tracktion::graph::NodeProperties props;
    props.hasAudio = true;
    props.numberOfChannels = au4juce::numChannels;
    props.latencyNumSamples = 0;
    props.nodeID = m_nodeId;
    return props;
}

void AudacityToTracktionNode::process(tracktion::graph::Node::ProcessContext& pc)
{
    float* const buffers[au4juce::numChannels] { channelPointer(pc.buffers.audio, 0), channelPointer(pc.buffers.audio, 1) };
    m_input->GetSamples(buffers, pc.referenceSampleRange.getStart(), pc.numSamples);
}
