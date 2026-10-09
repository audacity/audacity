/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEODECODEWORKER_H
#define AU_VIDEO_VIDEODECODEWORKER_H

#include <atomic>
#include <condition_variable>
#include <cstdint>
#include <functional>
#include <mutex>
#include <thread>

#include "../ivideodecodebackend.h"
#include "videoframecache.h"

namespace au::video {
//! Runs the decoder off the GUI thread.
//!
//! Requests are latest-wins: only the most recent one is kept, because a
//! playhead being dragged produces them faster than any decoder can service
//! them and every superseded one is wasted work. Results go into the frame
//! cache, and a callback tells the owner a repaint is worth doing.
//!
//! Ownership rule, and it matters: the backend must be opened on the GUI
//! thread before being handed over, because FFmpegFunctions::Load caches into
//! an unsynchronised static and mutates process environment on some platforms.
//! After construction the backend belongs to this worker and nothing else may
//! touch it.
//!
//! The callback runs on the decode thread. Marshal to the GUI thread in it.
class VideoDecodeWorker
{
public:
    struct Request {
        muse::secs_t time = 0.0;
        int targetWidth = 0;
        int targetHeight = 0;
    };

    //! Called on the decode thread once a frame has been placed in the cache.
    using FrameReadyCallback = std::function<void ()>;

    //! Called on the decode thread when a request produced no frame, with the
    //! backend's reason. Without this a file the converter cannot handle is
    //! just a black rectangle with the resolution printed under it.
    using FrameFailedCallback = std::function<void (VideoError)>;

    VideoDecodeWorker(IVideoDecodeBackendPtr backend, VideoFrameCache* cache);
    ~VideoDecodeWorker();

    VideoDecodeWorker(const VideoDecodeWorker&) = delete;
    VideoDecodeWorker& operator=(const VideoDecodeWorker&) = delete;

    void setFrameReadyCallback(FrameReadyCallback callback);
    void setFrameFailedCallback(FrameFailedCallback callback);

    void start();

    //! Stops the thread and waits for it. Safe to call more than once.
    void stop();

    bool isRunning() const;

    //! Replaces any request not yet started. Cheap enough to call from a
    //! handler that runs inside the player's own timer tick.
    void request(const Request& request);

    //! Requests served since construction; for tests and diagnostics.
    uint64_t servedCount() const { return m_served.load(); }

    //! Requests dropped because a newer one arrived first.
    uint64_t supersededCount() const { return m_superseded.load(); }

private:
    void run();

    IVideoDecodeBackendPtr m_backend;
    VideoFrameCache* m_cache = nullptr;
    FrameReadyCallback m_frameReady;
    FrameFailedCallback m_frameFailed;

    mutable std::mutex m_mutex;
    std::condition_variable m_wake;
    Request m_pending;
    bool m_havePending = false;
    bool m_stopping = false;

    std::thread m_thread;
    std::atomic<bool> m_running { false };
    std::atomic<uint64_t> m_served { 0 };
    std::atomic<uint64_t> m_superseded { 0 };
};
}

#endif // AU_VIDEO_VIDEODECODEWORKER_H
