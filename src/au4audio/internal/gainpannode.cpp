/*
* Audacity: A Digital Audio Editor
*/
#include "gainpannode.h"

#include "au3-mixer/AudioIOSequences.h"

#include "stereo.h"

using namespace au::au4audio;

GainPanNode::GainPanNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numInputChannels,
                         std::function<float(int)> channelVolume)
    : m_input{std::move(input)}, m_numInputChannels{numInputChannels}, m_channelVolume{std::move(channelVolume)}
{
}

GainPanNode::GainPanNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const PlayableSequence> track)
    : GainPanNode{std::move(input), track->NChannels(), [track](int channel) { return track->GetChannelVolume(channel); }}
{
}

void GainPanNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    m_input->GetSamples(buffers, start, len);
    // Read once per call: they may change on another thread
    const float left = m_channelVolume(0);
    const float right = m_channelVolume(1);
    if (m_numInputChannels < numChannels) {
        // Mono, in the first buffer only so far
        for (size_t i = 0; i < len; ++i) {
            buffers[1][i] = buffers[0][i] * right;
            buffers[0][i] *= left;
        }
        return;
    }
    for (size_t i = 0; i < len; ++i) {
        buffers[0][i] *= left;
        buffers[1][i] *= right;
    }
}
