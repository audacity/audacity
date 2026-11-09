/*
* Audacity: A Digital Audio Editor
*/
#include "metertapnode.h"

#include <algorithm>
#include <cmath>

using namespace au::au4audio;

MeterTapNode::MeterTapNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numMeterChannels, size_t maxFrames)
    : m_input{std::move(input)}, m_numMeterChannels{std::clamp<size_t>(numMeterChannels, 1, numChannels)}
{
    for (auto& kept : m_kept) {
        kept.assign(maxFrames, 0.f);
    }
}

void MeterTapNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    m_input->GetSamples(buffers, start, len);
    const auto index = start.as_long_long();
    const auto capacity = m_kept[0].size();
    // Unless contiguous with what it keeps and fitting, it keeps this read only, as much of it as fits
    if (m_frames == 0 || index != m_index + static_cast<int64_t>(m_frames) || m_frames + len > capacity) {
        m_index = index;
        m_frames = 0;
    }
    len = std::min(len, capacity);
    if (m_numMeterChannels == 1) {
        auto* const kept = m_kept[0].data() + m_frames;
        for (size_t i = 0; i < len; ++i) {
            kept[i] = std::abs(buffers[0][i]) >= std::abs(buffers[1][i]) ? buffers[0][i] : buffers[1][i];
        }
    } else {
        for (int ch = 0; ch < numChannels; ++ch) {
            std::copy_n(buffers[ch], len, m_kept[ch].data() + m_frames);
        }
    }
    m_frames += len;
}

MeterTapNode::Capture MeterTapNode::capture() const
{
    Capture capture;
    capture.index = m_index;
    capture.frames = m_frames;
    capture.numChannels = m_numMeterChannels;
    for (size_t ch = 0; ch < m_numMeterChannels; ++ch) {
        capture.channels[ch] = m_kept[ch].data();
    }
    return capture;
}
