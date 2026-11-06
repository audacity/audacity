/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <cstdint>
#include <optional>
#include <utility>

#include "au3-clip-processing/sequence-graph/IPlayheadMapper.h"

namespace au::au4juce {
//! Maps the linear indices of a stream's samples to the timeline positions the playhead reads, as seeks and loops say
/*!
 * The device is at an index, which the device thread advances. What the playhead reads from an index on depends on the
 * index only, until the main thread changes the position or the loop: then what it reads from where the device is
 * changes, as with tracktion_graph's PlayHead, which this behaves as.
 *
 * Linear positions are timeline positions as if there was no loop. Playing from left of the loop rolls into it.
 *
 * Positions and indices are in samples.
 */
class PlayheadMapper final : public IPlayheadMapper
{
public:
    using Range = std::pair<int64_t, int64_t>;

    //! Before the device starts: it plays from `position`, looping `loop` if given, at least a sample long
    void start(int64_t position, std::optional<Range> loop);

    //! Main thread: plays from `position` where the device is; if looping, within the loop
    void setPosition(int64_t position);
    //! Main thread: loops `loop`, or stops looping; plays on from where the device is, within the new loop
    void setLoop(std::optional<Range> loop);

    //! Device thread: the device played `frames` more
    void advance(int64_t frames);

    //! Where the device is
    int64_t deviceIndex() const { return m_deviceIndex.load(std::memory_order_acquire); }
    //! Where in the timeline the device is
    int64_t position() const { return Map(deviceIndex(), 1).position.as_long_long(); }
    std::optional<Range> loop() const;

    //! The linear position of `index`
    int64_t linearPosition(int64_t index) const;

    // IPlayheadMapper; any thread
    Segment Map(sampleCount index, size_t maxLen) const override;

private:
    struct Mapping {
        //! The linear position of an index is the index plus this
        int64_t offset = 0;
        bool looping = false;
        int64_t loopStart = 0;
        int64_t loopEnd = 0;
    };

    Mapping load() const;
    void store(const Mapping& mapping);
    //! Where `mapping` reads from `index` on, and for how long contiguously
    static std::pair<int64_t, int64_t> map(const Mapping& mapping, int64_t index, int64_t maxLen);
    //! What `mapping` plays where the device is, at `position`
    Mapping anchored(Mapping mapping, int64_t position) const;

    // Written by the main thread only, read consistently with a sequence lock
    std::atomic<uint32_t> m_sequence { 0 };
    std::atomic<int64_t> m_offset { 0 };
    std::atomic<bool> m_looping { false };
    std::atomic<int64_t> m_loopStart { 0 };
    std::atomic<int64_t> m_loopEnd { 0 };

    std::atomic<int64_t> m_deviceIndex { 0 };
};
}
