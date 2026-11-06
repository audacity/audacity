/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <vector>

#include "audio/iaudiometer.h"

namespace au::au4juce {
//! What the meters show, on its way from the thread that renders to the device callback, which forwards it as it plays
/*!
 * Written by one thread, block by block, by stream index; read by another, which sends to the meters what it outputs,
 * stamped with when it's heard. What it holds when invalidated, e.g. after a seek, is never shown, as what the queue
 * rendered then is never played.
 */
class MeterSink
{
public:
    //! A meter the sink feeds: a track's, or the main one
    struct Meter {
        std::optional<audio::IAudioMeter::TrackId> trackId;
        size_t numChannels = 0;
    };

    //! @param maxFrames the most a block has
    //! @param numBlocks how many blocks it holds, written and not yet read
    MeterSink(std::vector<Meter> meters, size_t maxFrames, size_t numBlocks);

    const std::vector<Meter>& meters() const { return m_meters; }

    // Any thread
    //! What was written before is dropped
    void invalidate() { m_generation.fetch_add(1, std::memory_order_acq_rel); }
    uint32_t generation() const { return m_generation.load(std::memory_order_acquire); }

    //! The writer: `frames` frames from stream index `start`, as rendered while the generation was `generation`.
    //! `channels`: each meter's channels in turn; null where there is nothing to show. Dropped if the sink is full.
    void write(uint32_t generation, int64_t start, size_t frames, const float* const* channels);

    //! The reader: sends to `meter` what plays from stream index `index` for `frames` frames, heard from `time` on
    void forward(audio::IAudioMeter& meter, int64_t index, size_t frames, audio::TimePoint time, double sampleRate);

private:
    struct Block {
        uint32_t generation = 0;
        int64_t start = 0;
        size_t frames = 0;
        //! Channel after channel, maxFrames each
        std::vector<float> samples;
    };

    const std::vector<Meter> m_meters;
    const size_t m_maxFrames;
    size_t m_numChannels = 0;
    std::vector<Block> m_blocks;
    std::atomic<size_t> m_numWritten { 0 };
    std::atomic<size_t> m_numRead { 0 };
    std::atomic<uint32_t> m_generation { 0 };
};
}
