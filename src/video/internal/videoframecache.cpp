/*
* Audacity: A Digital Audio Editor
*/
#include "videoframecache.h"

#include <algorithm>
#include <cstdlib>

using namespace au::video;

namespace {
size_t frameBytes(const VideoFrame& frame)
{
    if (frame.image.isNull()) {
        return 0;
    }
    return static_cast<size_t>(std::max<qsizetype>(0, frame.image.sizeInBytes()));
}
}

VideoFrameCache::VideoFrameCache(size_t byteBudget)
    : m_budget(byteBudget)
{
}

void VideoFrameCache::setByteBudget(size_t bytes)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_budget = bytes;
    evictIfOverBudget();
}

size_t VideoFrameCache::byteBudget() const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_budget;
}

void VideoFrameCache::put(const VideoFrame& frame, int64_t durationPts)
{
    if (!frame.valid()) {
        return;
    }

    std::lock_guard<std::mutex> lock(m_mutex);

    Entry entry;
    entry.frame = frame;
    entry.durationPts = std::max<int64_t>(1, durationPts);
    entry.bytes = frameBytes(frame);

    auto existing = m_entries.find(frame.pts);
    if (existing != m_entries.end()) {
        m_bytes -= existing->second.bytes;
        existing->second = entry;
        m_bytes += entry.bytes;
    } else {
        m_bytes += entry.bytes;
        m_entries.emplace(frame.pts, std::move(entry));
    }

    evictIfOverBudget();
}

VideoFrameCache::Lookup VideoFrameCache::frameFor(int64_t pts) const
{
    std::lock_guard<std::mutex> lock(m_mutex);

    m_lastRequested = pts;
    m_haveRequest = true;

    Lookup result;
    if (m_entries.empty()) {
        return result;
    }

    // The last entry starting at or before the requested timestamp.
    auto it = m_entries.upper_bound(pts);
    if (it == m_entries.begin()) {
        return result;   // everything held starts later than this
    }
    --it;

    result.frame = it->second.frame;
    result.covers = pts < it->first + it->second.durationPts;
    return result;
}

bool VideoFrameCache::contains(int64_t pts) const
{
    std::lock_guard<std::mutex> lock(m_mutex);

    if (m_entries.empty()) {
        return false;
    }

    auto it = m_entries.upper_bound(pts);
    if (it == m_entries.begin()) {
        return false;
    }
    --it;

    return pts < it->first + it->second.durationPts;
}

void VideoFrameCache::clear()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_entries.clear();
    m_bytes = 0;
}

size_t VideoFrameCache::count() const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_entries.size();
}

size_t VideoFrameCache::sizeBytes() const
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_bytes;
}

void VideoFrameCache::evictIfOverBudget()
{
    while (m_bytes > m_budget && m_entries.size() > 1) {
        // Drop whichever end is further from what was last asked for, so the
        // frames around the playhead survive. Playback walks forward, so this
        // usually sheds the past; a backwards scrub sheds the future instead.
        auto oldest = m_entries.begin();
        auto newest = std::prev(m_entries.end());

        const int64_t reference = m_haveRequest ? m_lastRequested : newest->first;
        const auto distance = [reference](int64_t pts) {
            return pts > reference ? pts - reference : reference - pts;
        };

        auto victim = distance(oldest->first) >= distance(newest->first) ? oldest : newest;
        m_bytes -= victim->second.bytes;
        m_entries.erase(victim);
    }

    // A single entry over budget is kept: showing the frame the playhead is on
    // matters more than the budget, and the next put will replace it anyway.
}
