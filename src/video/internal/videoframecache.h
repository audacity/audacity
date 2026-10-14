/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOFRAMECACHE_H
#define AU_VIDEO_VIDEOFRAMECACHE_H

#include <cstddef>
#include <cstdint>
#include <map>
#include <mutex>

#include "../videotypes.h"

namespace au::video {
//! Holds recently decoded frames so the GUI thread can answer "what is showing
//! now" without waiting on the decoder.
//!
//! Written by the decode worker and read by the GUI thread on every repaint,
//! so every entry point takes the lock, and the critical sections are kept to
//! a map lookup and a copy of an implicitly shared QImage.
//!
//! A miss returns the nearest earlier frame marked stale rather than nothing.
//! During playback a momentary miss means the decoder is a frame behind, and
//! holding the previous picture for one repaint is invisible where blanking to
//! black is not.
//!
//! Eviction keeps the frames nearest the last thing asked for. A plain
//! least-recently-used policy is wrong here: playback walks forward through
//! timestamps and would evict exactly the frames about to be needed.
class VideoFrameCache
{
public:
    struct Lookup {
        VideoFrame frame;

        //! The frame's own interval contains the requested timestamp. When
        //! false the frame is the nearest earlier one, held over.
        bool covers = false;

        bool valid() const { return frame.valid(); }
    };

    static constexpr size_t DEFAULT_BYTE_BUDGET = 64 * 1024 * 1024;

    explicit VideoFrameCache(size_t byteBudget = DEFAULT_BYTE_BUDGET);

    void setByteBudget(size_t bytes);
    size_t byteBudget() const;

    //! Stores a frame. durationPts is how long it is shown for, in the video
    //! stream's own time base; it decides which requests the frame covers.
    void put(const VideoFrame& frame, int64_t durationPts);

    Lookup frameFor(int64_t pts) const;

    //! True when a frame covering this timestamp is already held.
    bool contains(int64_t pts) const;

    void clear();

    size_t count() const;
    size_t sizeBytes() const;

private:
    struct Entry {
        VideoFrame frame;
        int64_t durationPts = 1;
        size_t bytes = 0;
    };

    //! Caller holds the lock.
    void evictIfOverBudget();

    mutable std::mutex m_mutex;
    std::map<int64_t, Entry> m_entries;
    size_t m_bytes = 0;
    size_t m_budget = 0;
    mutable int64_t m_lastRequested = 0;
    mutable bool m_haveRequest = false;
};
}

#endif // AU_VIDEO_VIDEOFRAMECACHE_H
