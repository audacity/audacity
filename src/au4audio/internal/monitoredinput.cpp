/*
* Audacity: A Digital Audio Editor
*/
#include "monitoredinput.h"

#include <algorithm>

using namespace au::au4audio;

MonitoredInput::MonitoredInput(int numInputChannels, size_t maxFrames, const std::atomic<float>& inputVolume)
    : m_numInputChannels{numInputChannels}, m_inputVolume{inputVolume}
{
    for (auto& channel : m_input) {
        channel.assign(maxFrames, 0.f);
    }
}

void MonitoredInput::read(const float* const* input, int offset, int frames, int64_t index)
{
    const float volume = m_inputVolume.load(std::memory_order_relaxed);
    const auto len = std::min(static_cast<size_t>(std::max(0, frames)), m_input[0].size());
    for (int ch = 0; ch < numChannels; ++ch) {
        const auto source = std::min(ch, m_numInputChannels - 1);
        const float* const samples = input && source >= 0 && input[source] ? input[source] + offset : nullptr;
        for (size_t i = 0; i < len; ++i) {
            m_input[ch][i] = samples ? samples[i] * volume : 0.f;
        }
    }
    m_index = index;
    m_frames = len;
}

bool MonitoredInput::get(float* const* buffers, size_t numChannels, int64_t index, size_t len) const
{
    if (index < m_index || index + static_cast<int64_t>(len) > m_index + static_cast<int64_t>(m_frames)) {
        return false;
    }
    const auto offset = static_cast<size_t>(index - m_index);
    for (size_t ch = 0; ch < std::min<size_t>(numChannels, au4audio::numChannels); ++ch) {
        std::copy_n(m_input[ch].data() + offset, len, buffers[ch]);
    }
    return true;
}

MonitoredInputNode::MonitoredInputNode(std::shared_ptr<const MonitoredInput> input, size_t numChannels, int64_t lead)
    : m_input{std::move(input)}, m_numChannels{std::min<size_t>(numChannels, au4audio::numChannels)}, m_lead{lead}
{
}

void MonitoredInputNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    if (!m_input->get(buffers, m_numChannels, start.as_long_long() - m_lead, len)) {
        for (size_t ch = 0; ch < m_numChannels; ++ch) {
            std::fill_n(buffers[ch], len, 0.f);
        }
    }
}
