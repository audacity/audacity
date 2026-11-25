/*
 * Audacity: A Digital Audio Editor
 */
#include "sharedoutputnode.h"

#include <algorithm>

using namespace au::au4audio;

namespace {
//! Where `index` is kept, if it ever is negative
size_t slot(int64_t index, int64_t capacity)
{
    return static_cast<size_t>(((index % capacity) + capacity) % capacity);
}
}

SharedOutputNode::SharedOutputNode(std::shared_ptr<ISequenceProcessingNode> input, size_t capacity, size_t maxFrames)
    : m_input{std::move(input)}, m_capacity{std::max(capacity, maxFrames)}, m_maxFrames{maxFrames}
{
    for (int ch = 0; ch < numChannels; ++ch) {
        m_kept[ch].assign(m_capacity, 0.f);
        m_rendered[ch].assign(maxFrames, 0.f);
        m_renderedPointers[ch] = m_rendered[ch].data();
    }
}

void SharedOutputNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    const auto capacity = static_cast<int64_t>(m_capacity);
    for (size_t done = 0; done < len;) {
        const auto from = start.as_long_long() + static_cast<int64_t>(done);
        const auto frames = static_cast<int64_t>(std::min(len - done, m_capacity));
        // Rendering on from what it keeps would forget `from` before reaching it
        if (from < m_begin || from + frames - capacity > m_end) {
            m_begin = from;
            m_end = from;
        }
        render(from + frames);
        for (int ch = 0; ch < numChannels; ++ch) {
            for (int64_t i = 0; i < frames; ++i) {
                buffers[ch][done + static_cast<size_t>(i)] = m_kept[ch][slot(from + i, capacity)];
            }
        }
        done += static_cast<size_t>(frames);
    }
}

void SharedOutputNode::render(int64_t end)
{
    const auto capacity = static_cast<int64_t>(m_capacity);
    while (m_end < end) {
        const auto frames = static_cast<size_t>(std::min<int64_t>(end - m_end, static_cast<int64_t>(m_maxFrames)));
        for (int ch = 0; ch < numChannels; ++ch) {
            std::fill_n(m_renderedPointers[ch], frames, 0.f);
        }
        m_input->GetSamples(m_renderedPointers, m_end, frames);
        for (int ch = 0; ch < numChannels; ++ch) {
            for (size_t i = 0; i < frames; ++i) {
                m_kept[ch][slot(m_end + static_cast<int64_t>(i), capacity)] = m_rendered[ch][i];
            }
        }
        m_end += static_cast<int64_t>(frames);
        m_begin = std::max(m_begin, m_end - capacity);
    }
}
