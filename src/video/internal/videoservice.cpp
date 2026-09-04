/*
* Audacity: A Digital Audio Editor
*/
#include "videoservice.h"

#include <cmath>
#include <filesystem>
#include <thread>

#include "global/async/async.h"

#include "au3wrap/internal/projectvideoref.h"
#include "project/iaudacityproject.h"

#include "ffmpeg/ffmpegsoftwarebackend.h"
#include "videopathresolve.h"

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

void VideoService::init()
{
    // A project becoming current is the point at which a saved attachment can
    // be reopened. The project has already been loaded and its attributes
    // parsed by the time this fires.
    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        restoreFromProject();
    });

    restoreFromProject();
}

std::string VideoService::projectDirectory() const
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return {};
    }

    const std::string projectFile = project->path().toStdString();
    if (projectFile.empty()) {
        return {};
    }

    std::error_code ec;
    const auto dir = std::filesystem::u8path(projectFile).parent_path();
    if (ec) {
        return {};
    }

    const auto u8 = dir.u8string();
    return std::string(u8.begin(), u8.end());
}

au::au3::ProjectVideoRef* VideoService::projectRef() const
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return nullptr;
    }

    auto* au3Project = reinterpret_cast<AudacityProject*>(project->au3ProjectPtr());
    if (au3Project == nullptr) {
        return nullptr;
    }

    return &au::au3::ProjectVideoRef::Get(*au3Project);
}

void VideoService::storeInProject()
{
    auto* ref = projectRef();
    if (ref == nullptr) {
        return;
    }

    if (m_path.empty()) {
        ref->clear();
        return;
    }

    ref->setPath(m_path);
    ref->setRelativePath(makeRelativeVideoPath(projectDirectory(), m_path));

    const VideoStreamInfo& info = streamInfo();
    ref->setDuration(info.duration.to_double());
    ref->setFrameRate(info.frameRate);
    ref->setOffset(m_offset.to_double());

    commitProjectChange();
}

void VideoService::commitProjectChange()
{
    // Writing into the project object is not enough on its own: without this
    // the attachment is only in memory, so closing the project discards it
    // with no prompt. Autosaves rather than pushing an undo entry - attaching
    // a video is a property of the session, not an edit to undo.
    if (projectHistory()) {
        projectHistory()->modifyState(true);
    }
}

void VideoService::restoreFromProject()
{
    auto* ref = projectRef();

    if (ref == nullptr || ref->isEmpty()) {
        // The new project has no video. Without this the previous project's
        // picture stays on screen and follows the new project's playhead.
        if (isAttached() || m_error != VideoError::None) {
            detachWithoutClearingProject();
        }
        return;
    }

    const std::string resolved = resolveVideoPath(
        ref->path(), ref->relativePath(), projectDirectory(),
        [](const std::string& candidate) {
            return std::filesystem::exists(std::filesystem::u8path(candidate));
        });

    if (resolved.empty()) {
        // The project opens regardless; the panel says the file is missing.
        m_error = VideoError::FileNotFound;
        m_attachedChanged.notify();
        return;
    }

    // Read before attaching: attach() begins with detach(), which resets the
    // offset along with the rest of the attachment state.
    const double savedOffset = ref->offset();

    const VideoError err = attach(resolved);
    if (err != VideoError::None) {
        return;
    }

    m_offset = muse::secs_t(savedOffset);

    // A path that still resolves after the media was replaced or re-encoded
    // is worse than one that does not, because nothing else would notice.
    const VideoStreamInfo& info = streamInfo();
    const double frameDuration = info.frameRate > 0.0 ? 1.0 / info.frameRate : 0.04;

    m_sourceMismatch =
        (ref->duration() > 0.0
         && std::fabs(ref->duration() - info.duration.to_double()) > frameDuration)
        || (ref->frameRate() > 0.0
            && std::fabs(ref->frameRate() - info.frameRate) > 0.01);

    if (m_sourceMismatch) {
        m_attachedChanged.notify();
    }
}

bool VideoService::hasRecordedAttachment() const
{
    if (isAttached()) {
        return true;
    }

    // The file may simply be unreachable right now - an unplugged drive, a
    // cloud path that has not synced. The project still records it.
    const au::au3::ProjectVideoRef* ref = projectRef();
    return ref != nullptr && !ref->isEmpty();
}

bool VideoService::sourceMismatch() const
{
    return m_sourceMismatch;
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
    m_sourceMismatch = false;

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
            // A frame arrived, so whatever went wrong before has passed.
            // Without this one transient failure pins its message for the
            // rest of the session.
            if (m_error != VideoError::None) {
                m_error = VideoError::None;
                m_attachedChanged.notify();
            }
            m_frameReady.notify();
        }, guiThread);
    });

    // A frame that could not be produced has a reason, and the panel says it
    // rather than showing an unexplained black rectangle.
    m_worker->setFrameFailedCallback([this, guiThread](VideoError reason) {
        if (reason == VideoError::None) {
            return;
        }
        muse::async::Async::call(this, [this, reason]() {
            if (m_error == reason) {
                return;
            }
            m_error = reason;
            m_attachedChanged.notify();
        }, guiThread);
    });

    m_worker->start();

    storeInProject();

    m_attachedChanged.notify();
    return VideoError::None;
}

void VideoService::detachWithoutClearingProject()
{
    stopDecoding();

    m_path.clear();
    m_error = VideoError::None;
    m_sourceMismatch = false;

    // The offset belongs to the attachment, not to the panel: a different
    // video must not inherit the last one's alignment.
    m_offset = muse::secs_t(0.0);

    m_attachedChanged.notify();
    m_offsetChanged.notify();
}

void VideoService::stopDecoding()
{
    // Order matters: the worker holds the backend and may be mid-decode, so it
    // is stopped and joined before anything it uses goes away.
    if (m_worker) {
        m_worker->stop();
        m_worker.reset();
    }

    m_backend.reset();
    m_cache.reset();
}

void VideoService::detach()
{
    const bool had = m_backend != nullptr || m_error != VideoError::None;

    stopDecoding();
    m_path.clear();
    m_error = VideoError::None;
    m_sourceMismatch = false;

    if (auto* ref = projectRef()) {
        ref->clear();
        commitProjectChange();
    }

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

muse::secs_t VideoService::toVideoTime(muse::secs_t projectTime) const
{
    // Positive offset means the picture runs late, so the frame shown at
    // project time t is the one the file holds at t - offset.
    return projectTime - m_offset;
}

muse::secs_t VideoService::offset() const
{
    return m_offset;
}

void VideoService::setOffset(muse::secs_t offset)
{
    if (std::fabs(offset.to_double() - m_offset.to_double()) < 1e-9) {
        return;
    }

    m_offset = offset;

    // The cache is keyed on video pts, which the offset does not change, so
    // it stays valid - only the mapping from project time moves.
    storeInProject();
    commitProjectChange();

    m_offsetChanged.notify();
}

muse::async::Notification VideoService::offsetChanged() const
{
    return m_offsetChanged;
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
        m_cache->frameFor(m_backend->timeToPts(toVideoTime(projectTime)));

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
    request.time = toVideoTime(projectTime);
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

    const muse::secs_t videoTime = toVideoTime(projectTime);
    return videoTime >= muse::secs_t(0.0) && videoTime < info.duration;
}

muse::async::Notification VideoService::attachedChanged() const
{
    return m_attachedChanged;
}

muse::async::Notification VideoService::frameReady() const
{
    return m_frameReady;
}
