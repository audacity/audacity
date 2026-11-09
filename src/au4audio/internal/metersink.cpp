/*
* Audacity: A Digital Audio Editor
*/
#include "metersink.h"

#include <algorithm>
#include <chrono>

using namespace au::au4audio;

MeterSink::MeterSink(std::vector<Meter> meters, size_t maxFrames, size_t numBlocks)
    : m_meters{std::move(meters)}, m_maxFrames{maxFrames}, m_blocks(numBlocks)
{
    for (const auto& meter : m_meters) {
        m_numChannels += meter.numChannels;
    }
    for (auto& block : m_blocks) {
        block.samples.assign(m_numChannels * m_maxFrames, 0.f);
    }
}

void MeterSink::write(uint32_t generation, int64_t start, size_t frames, const float* const* channels)
{
    const auto numWritten = m_numWritten.load(std::memory_order_relaxed);
    if (numWritten - m_numRead.load(std::memory_order_acquire) == m_blocks.size()) {
        return;
    }
    auto& block = m_blocks[numWritten % m_blocks.size()];
    block.generation = generation;
    block.start = start;
    block.frames = std::min(frames, m_maxFrames);
    for (size_t ch = 0; ch < m_numChannels; ++ch) {
        auto* const samples = block.samples.data() + ch * m_maxFrames;
        if (channels[ch]) {
            std::copy_n(channels[ch], block.frames, samples);
        } else {
            std::fill_n(samples, block.frames, 0.f);
        }
    }
    m_numWritten.store(numWritten + 1, std::memory_order_release);
}

void MeterSink::forward(audio::IAudioMeter& meter, int64_t index, size_t frames, audio::TimePoint time, double sampleRate)
{
    const auto generation = this->generation();
    const auto end = index + static_cast<int64_t>(frames);
    const auto numWritten = m_numWritten.load(std::memory_order_acquire);
    for (auto numRead = m_numRead.load(std::memory_order_relaxed); numRead != numWritten;) {
        const auto& block = m_blocks[numRead % m_blocks.size()];
        const auto blockEnd = block.start + static_cast<int64_t>(block.frames);
        // Dropped, or already played
        if (block.generation != generation || blockEnd <= index) {
            m_numRead.store(++numRead, std::memory_order_release);
            continue;
        }
        if (block.start >= end) {
            break;
        }
        const auto from = std::max(index, block.start);
        const auto to = std::min(end, blockEnd);
        const auto heard = time + std::chrono::duration_cast<audio::TimePoint::duration>(
            std::chrono::duration<double>((from - index) / sampleRate));
        size_t channel = 0;
        for (const auto& target : m_meters) {
            for (size_t ch = 0; ch < target.numChannels; ++ch, ++channel) {
                const auto* const samples = block.samples.data() + channel * m_maxFrames + (from - block.start);
                meter.push(static_cast<uint8_t>(ch), { samples, static_cast<size_t>(to - from), 1, heard }, target.trackId);
            }
        }
        if (blockEnd > end) {
            // Its rest plays next
            break;
        }
        m_numRead.store(++numRead, std::memory_order_release);
    }
}
