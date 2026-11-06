/*
* Audacity: A Digital Audio Editor
*/
#include "metersendnode.h"

#include "metersink.h"
#include "metertapnode.h"

using namespace au::au4juce;

MeterSendNode::MeterSendNode(std::shared_ptr<ISequenceProcessingNode> input, std::vector<Tap> taps, MeterSink& sink)
    : m_input{std::move(input)}, m_taps{std::move(taps)}, m_sink{sink}
{
    size_t numChannels = 0;
    for (const auto& meter : m_sink.meters()) {
        numChannels += meter.numChannels;
    }
    m_channels.resize(numChannels);
}

void MeterSendNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    // Before rendering, so that what renders while the sink is invalidated is dropped
    const auto generation = m_sink.generation();
    m_input->GetSamples(buffers, start, len);

    const auto index = start.as_long_long();
    size_t channel = 0;
    for (size_t i = 0; i < m_taps.size(); ++i) {
        const auto& tap = m_taps[i];
        const auto capture = tap.node->capture();
        // What is heard from `index`, if it kept all of it
        const auto from = index + tap.lead;
        const bool kept = capture.index <= from && capture.index + static_cast<int64_t>(capture.frames) >= from + static_cast<int64_t>(len);
        const auto numChannels = m_sink.meters()[i].numChannels;
        for (size_t ch = 0; ch < numChannels; ++ch, ++channel) {
            m_channels[channel] = kept && ch < capture.numChannels ? capture.channels[ch] + (from - capture.index) : nullptr;
        }
        tap.node->reset();
    }
    m_sink.write(generation, index, len, m_channels.data());
}
