/*
* Audacity: A Digital Audio Editor
*/
#include "videoservice.h"

#include <thread>

#include "global/async/async.h"

#include "ffmpeg/ffmpegsoftwarebackend.h"

using namespace au::video;

namespace {
//! Enough for a few seconds of panel-sized frames, which is what a scrub back
//! and forth over the same stretch needs. Full frames are large, so this is a
//! byte budget rather than a frame count.
constexpr size_t CACHE_BUDGET_BYTES = 64u * 1024u * 1024u;
}

const VideoStreamInfo VideoService::s_emptyInfo = VideoStreamInfo();

VideoService::VideoService(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx)
{
}

VideoService::~VideoService()
{
    detach();
}

VideoError VideoService::attach(const std::string& path)
{
    detach();

    // Opened here rather than on the worker: FFmpegFunctions::Load caches into
    // an unsynchronised static and mutates process environment on some
    // platforms, so it stays on the GUI thread.
    auto backend = std::make_shared<FFmpegSoftwareBackend>();
    const VideoError err = backend->open(path);

    m_error = err;
    if (err != VideoError::None) {
        m_attachedChanged.notify();
        return err;
    }

    m_backend = backend;
    m_path = path;

    m_cache = std::make_unique<VideoFrameCache>(CACHE_BUDGET_BYTES);
    m_worker = std::make_unique<VideoDecodeWorker>(m_backend, m_cache.get());

    // The callback runs on the decode thread, so it does nothing but hop to
    // the GUI thread; everything downstream of frameReady touches QML.
    //
    // The thread has to be named explicitly. Async::call defaults its target
    // to the calling thread, which here is the decode worker, and nothing
    // drains that queue - the notification would simply never arrive.
    const std::thread::id guiThread = std::this_thread::get_id();
    m_worker->setFrameReadyCallback([this, guiThread]() {
        muse::async::Async::call(this, [this]() {
            m_frameReady.notify();
        }, guiThread);
    });

    m_worker->start();

    m_attachedChanged.notify();
    return VideoError::None;
}

void VideoService::detach()
{
    const bool had = m_backend != nullptr || m_error != VideoError::None;

    // Order matters: the worker holds the backend and may be mid-decode, so it
    // is stopped and joined before anything it uses goes away.
    if (m_worker) {
        m_worker->stop();
        m_worker.reset();
    }

    m_backend.reset();
    m_cache.reset();
    m_path.clear();
    m_error = VideoError::None;

    if (had) {
        m_attachedChanged.notify();
    }
}

bool VideoService::isAttached() const
{
    return m_backend != nullptr && m_backend->isOpen();
}

std::string VideoService::attachedPath() const
{
    return m_path;
}

VideoError VideoService::lastError() const
{
    return m_error;
}

const VideoStreamInfo& VideoService::streamInfo() const
{
    return m_backend ? m_backend->streamInfo() : s_emptyInfo;
}

VideoFrame VideoService::cachedFrameAt(muse::secs_t projectTime, bool* covers) const
{
    if (covers != nullptr) {
        *covers = false;
    }

    if (!isAttached() || !m_cache) {
        return VideoFrame();
    }

    // timeToPts reads only state fixed when the file was opened, so this does
    // not race the decoder.
    const VideoFrameCache::Lookup lookup =
        m_cache->frameFor(m_backend->timeToPts(projectTime));

    if (covers != nullptr) {
        *covers = lookup.covers;
    }
    return lookup.frame;
}

void VideoService::requestFrame(muse::secs_t projectTime, int targetWidth, int targetHeight)
{
    if (!isAttached() || !m_worker) {
        return;
    }
    if (!isTimeInRange(projectTime)) {
        return;
    }

    VideoDecodeWorker::Request request;
    request.time = projectTime;
    request.targetWidth = targetWidth;
    request.targetHeight = targetHeight;

    m_worker->request(request);
}

bool VideoService::isTimeInRange(muse::secs_t projectTime) const
{
    if (!isAttached()) {
        return false;
    }

    const VideoStreamInfo& info = m_backend->streamInfo();
    if (info.duration <= 0.0) {
        return true;   // duration unknown; let the decoder decide
    }

    return projectTime >= muse::secs_t(0.0) && projectTime < info.duration;
}

muse::async::Notification VideoService::attachedChanged() const
{
    return m_attachedChanged;
}

muse::async::Notification VideoService::frameReady() const
{
    return m_frameReady;
}
