/*
* Audacity: A Digital Audio Editor
*/
#include "effectsnode.h"

#include <algorithm>

#include "ieffectscontext.h"

using namespace au::au4juce;

EffectsNode::EffectsNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numInputChannels, const ChannelGroup* group,
                         IEffectsContext& context, size_t maxFrames)
    : m_input{std::move(input)}, m_numInputChannels{std::min<size_t>(numInputChannels, numChannels)}, m_group{group},
    m_context{context}, m_latency{std::max(0, context.latency(group))}, m_maxFrames{maxFrames}
{
    m_scratch.allocate(maxFrames);
    for (int ch = 0; ch < numChannels; ++ch) {
        m_discarded[ch].assign(maxFrames, 0.f);
        m_discardedPointers[ch] = m_discarded[ch].data();
    }
}

void EffectsNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    const auto index = start.as_long_long();
    for (size_t done = 0; done < len;) {
        const auto frames = std::min(len - done, m_maxFrames);
        float* const output[numChannels] { buffers[0] + done, buffers[1] + done };
        render(output, index + static_cast<int64_t>(done), frames);
        done += frames;
    }
}

void EffectsNode::render(float* const* output, int64_t index, size_t frames)
{
    if (m_next != index) {
        // What the effects output for the samples before `index` reaches here, delayed by their latency
        for (int64_t done = 0; done < m_latency;) {
            const auto count = static_cast<size_t>(std::min<int64_t>(m_latency - done, static_cast<int64_t>(m_maxFrames)));
            process(m_discardedPointers, index + done, count);
            done += static_cast<int64_t>(count);
        }
    }
    process(output, index + m_latency, frames);
    m_next = index + static_cast<int64_t>(frames);
}

void EffectsNode::process(float* const* output, int64_t index, size_t frames)
{
    // Effects may crash on null buffers, even for channels they don't use
    for (size_t ch = m_numInputChannels; ch < numChannels; ++ch) {
        std::fill_n(output[ch], frames, 0.f);
    }
    m_input->GetSamples(output, index, frames);
    m_context.process(m_group, output, m_scratch, frames);
}
