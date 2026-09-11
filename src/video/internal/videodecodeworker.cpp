/*
* Audacity: A Digital Audio Editor
*/
#include "videodecodeworker.h"

#include <utility>

using namespace au::video;

VideoDecodeWorker::VideoDecodeWorker(IVideoDecodeBackendPtr backend, VideoFrameCache* cache)
    : m_backend(std::move(backend)), m_cache(cache)
{
}

VideoDecodeWorker::~VideoDecodeWorker()
{
    stop();
}

void VideoDecodeWorker::setFrameReadyCallback(FrameReadyCallback callback)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_frameReady = std::move(callback);
}

void VideoDecodeWorker::setFrameFailedCallback(FrameFailedCallback callback)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    m_frameFailed = std::move(callback);
}

void VideoDecodeWorker::start()
{
    if (m_running.load()) {
        return;
    }

    {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_stopping = false;
    }

    m_running.store(true);
    m_thread = std::thread([this]() { run(); });
}

void VideoDecodeWorker::stop()
{
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (m_stopping && !m_thread.joinable()) {
            return;
        }
        m_stopping = true;
        m_havePending = false;
    }
    m_wake.notify_all();

    if (m_thread.joinable()) {
        m_thread.join();
    }
    m_running.store(false);
}

bool VideoDecodeWorker::isRunning() const
{
    return m_running.load();
}

void VideoDecodeWorker::request(const Request& request)
{
    if (request.targetWidth <= 0 || request.targetHeight <= 0) {
        return;
    }

    {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (m_stopping) {
            return;
        }
        if (m_havePending) {
            // The one already queued has not been started, so it is dead work.
            m_superseded.fetch_add(1);
        }
        m_pending = request;
        m_havePending = true;
    }
    m_wake.notify_one();
}

void VideoDecodeWorker::run()
{
    while (true) {
        Request current;
        FrameReadyCallback callback;
        FrameFailedCallback failed;

        {
            std::unique_lock<std::mutex> lock(m_mutex);
            m_wake.wait(lock, [this]() { return m_stopping || m_havePending; });

            if (m_stopping) {
                return;
            }

            current = m_pending;
            m_havePending = false;
            callback = m_frameReady;
            failed = m_frameFailed;
        }

        // Decoding happens with the lock released, so a request arriving
        // meanwhile supersedes rather than blocking the thread that made it.
        const VideoFrame frame = m_backend->frameAt(current.time,
                                                    current.targetWidth,
                                                    current.targetHeight);
        m_served.fetch_add(1);

        if (!frame.valid()) {
            // Report why. Silently dropping this is what turns an unsupported
            // file into a black rectangle with no explanation.
            if (failed) {
                failed(m_backend->lastFrameError());
            }
            continue;
        }

        if (m_cache != nullptr) {
            m_cache->put(frame, m_backend->frameDurationPts());
        }

        if (callback) {
            callback();
        }
    }
}
