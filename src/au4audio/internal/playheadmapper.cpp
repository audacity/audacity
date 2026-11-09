/*
* Audacity: A Digital Audio Editor
*/
#include "playheadmapper.h"

#include <algorithm>

using namespace au::au4audio;

void PlayheadMapper::start(int64_t position, std::optional<Range> loop)
{
    m_deviceIndex.store(0, std::memory_order_release);
    Mapping mapping;
    mapping.offset = position;
    if (loop) {
        mapping.looping = true;
        mapping.loopStart = loop->first;
        mapping.loopEnd = std::max(loop->first + 1, loop->second);
        // Right of the loop, play from its start; left of it, roll into it
        if (position >= mapping.loopEnd) {
            mapping.offset = mapping.loopStart;
        }
    }
    store(mapping);
}

void PlayheadMapper::setPosition(int64_t position)
{
    store(anchored(load(), position));
}

void PlayheadMapper::setLoop(std::optional<Range> loop)
{
    const auto current = load();
    const auto position = map(current, deviceIndex(), 1).first;
    auto mapping = current;
    mapping.looping = loop.has_value();
    if (loop) {
        mapping.loopStart = loop->first;
        mapping.loopEnd = std::max(loop->first + 1, loop->second);
    }
    store(anchored(mapping, position));
}

void PlayheadMapper::advance(int64_t frames)
{
    m_deviceIndex.store(m_deviceIndex.load(std::memory_order_relaxed) + frames, std::memory_order_release);
}

std::optional<PlayheadMapper::Range> PlayheadMapper::loop() const
{
    const auto mapping = load();
    if (!mapping.looping) {
        return std::nullopt;
    }
    return Range { mapping.loopStart, mapping.loopEnd };
}

int64_t PlayheadMapper::linearPosition(int64_t index) const
{
    return index + load().offset;
}

IPlayheadMapper::Segment PlayheadMapper::Map(sampleCount index, size_t maxLen) const
{
    const auto [position, length] = map(load(), index.as_long_long(), static_cast<int64_t>(maxLen));
    return { position, static_cast<size_t>(length) };
}

std::pair<int64_t, int64_t> PlayheadMapper::map(const Mapping& mapping, int64_t index, int64_t maxLen)
{
    maxLen = std::max<int64_t>(maxLen, 1);
    const auto linear = index + mapping.offset;
    if (!mapping.looping) {
        return { linear, maxLen };
    }
    if (linear < mapping.loopStart) {
        // Rolling into the loop
        return { linear, std::min(maxLen, mapping.loopStart - linear) };
    }
    const auto length = mapping.loopEnd - mapping.loopStart;
    const auto position = mapping.loopStart + (linear - mapping.loopStart) % length;
    return { position, std::min(maxLen, mapping.loopEnd - position) };
}

PlayheadMapper::Mapping PlayheadMapper::anchored(Mapping mapping, int64_t position) const
{
    const auto index = deviceIndex();
    if (mapping.looping) {
        const auto current = load();
        const bool rollingIn = current.looping && index + current.offset < current.loopStart;
        // As tracktion_graph's PlayHead: rolling in, it rolls on; else it plays within the loop
        position = rollingIn ? std::min(position, mapping.loopEnd) : std::clamp(position, mapping.loopStart, mapping.loopEnd - 1);
    }
    mapping.offset = position - index;
    return mapping;
}

PlayheadMapper::Mapping PlayheadMapper::load() const
{
    while (true) {
        const auto before = m_sequence.load(std::memory_order_acquire);
        if (before % 2 != 0) {
            // Being written
            continue;
        }
        Mapping mapping;
        mapping.offset = m_offset.load(std::memory_order_relaxed);
        mapping.looping = m_looping.load(std::memory_order_relaxed);
        mapping.loopStart = m_loopStart.load(std::memory_order_relaxed);
        mapping.loopEnd = m_loopEnd.load(std::memory_order_relaxed);
        std::atomic_thread_fence(std::memory_order_acquire);
        if (m_sequence.load(std::memory_order_relaxed) == before) {
            return mapping;
        }
    }
}

void PlayheadMapper::store(const Mapping& mapping)
{
    const auto sequence = m_sequence.load(std::memory_order_relaxed);
    m_sequence.store(sequence + 1, std::memory_order_relaxed);
    std::atomic_thread_fence(std::memory_order_release);
    m_offset.store(mapping.offset, std::memory_order_relaxed);
    m_looping.store(mapping.looping, std::memory_order_relaxed);
    m_loopStart.store(mapping.loopStart, std::memory_order_relaxed);
    m_loopEnd.store(mapping.loopEnd, std::memory_order_relaxed);
    m_sequence.store(sequence + 2, std::memory_order_release);
}
