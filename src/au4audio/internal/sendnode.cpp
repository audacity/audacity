/*
 * Audacity: A Digital Audio Editor
 */
#include "sendnode.h"

#include "stereo.h"

using namespace au::au4audio;

SendNode::SendNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numInputChannels, std::function<float()> level)
    : m_input{std::move(input)}, m_numInputChannels{numInputChannels}, m_level{std::move(level)}
{
}

void SendNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    m_input->GetSamples(buffers, start, len);
    // Read once per call: it may change on another thread
    const float level = m_level();
    for (size_t i = 0; i < len; ++i) {
        buffers[0][i] *= level;
    }
    if (m_numInputChannels < numChannels) {
        for (size_t i = 0; i < len; ++i) {
            buffers[1][i] = buffers[0][i];
        }
        return;
    }
    for (size_t i = 0; i < len; ++i) {
        buffers[1][i] *= level;
    }
}
