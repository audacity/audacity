/*
* Audacity: A Digital Audio Editor
*/
#include "videoservice.h"

#include "ffmpeg/ffmpegsoftwarebackend.h"

using namespace au::video;

const VideoStreamInfo VideoService::s_emptyInfo = VideoStreamInfo();

VideoService::VideoService(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx)
{
}

VideoError VideoService::attach(const std::string& path)
{
    detach();

    auto backend = std::make_shared<FFmpegSoftwareBackend>();
    const VideoError err = backend->open(path);

    m_error = err;
    if (err != VideoError::None) {
        m_attachedChanged.notify();
        return err;
    }

    m_backend = backend;
    m_path = path;
    m_attachedChanged.notify();
    return VideoError::None;
}

void VideoService::detach()
{
    const bool had = m_backend != nullptr || m_error != VideoError::None;

    m_backend.reset();
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

VideoFrame VideoService::frameAt(muse::secs_t projectTime,
                                 int targetWidth, int targetHeight)
{
    if (!isAttached()) {
        return VideoFrame();
    }

    // M1 anchors the video at the start of the timeline. Attaching it to a
    // clip's play start time, so that dragging the audio moves the picture
    // with it, is what the persistence milestone adds.
    return m_backend->frameAt(projectTime, targetWidth, targetHeight);
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
