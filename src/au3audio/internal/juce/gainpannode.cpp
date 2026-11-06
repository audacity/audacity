/*
* Audacity: A Digital Audio Editor
*/
#include "gainpannode.h"

#include "au3-mixer/AudioIOSequences.h"

#include "stereo.h"

using namespace au::au4juce;

GainPanNode::GainPanNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const PlayableSequence> track)
    : m_input{std::move(input)}, m_track{std::move(track)}
{
}

void GainPanNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    m_input->GetSamples(buffers, start, len);
    // Read once per call: they may change on another thread
    const float left = m_track->GetChannelVolume(0);
    const float right = m_track->GetChannelVolume(1);
    if (m_track->NChannels() < numChannels) {
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
