/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOSERVICE_H
#define AU_VIDEO_VIDEOSERVICE_H

#include <memory>
#include <string>

#include "global/async/asyncable.h"
#include "modularity/ioc.h"

#include "../ivideodecodebackend.h"
#include "../ivideoservice.h"
#include "videodecodeworker.h"
#include "videoframecache.h"

namespace au::video {
//! Owns the video attached to a project, and the machinery that decodes it.
//!
//! The backend is opened here, on the GUI thread, and then handed to the decode
//! worker; nothing else touches it afterwards. Reads go through the frame
//! cache, which is why they can happen on every repaint.
class VideoService : public IVideoService, public muse::async::Asyncable,
    public muse::Contextable
{
public:
    explicit VideoService(const muse::modularity::ContextPtr& ctx);
    ~VideoService() override;

    VideoError attach(const std::string& path) override;
    void detach() override;

    bool isAttached() const override;
    std::string attachedPath() const override;
    VideoError lastError() const override;

    const VideoStreamInfo& streamInfo() const override;

    VideoFrame cachedFrameAt(muse::secs_t projectTime, bool* covers = nullptr) const override;
    void requestFrame(muse::secs_t projectTime, int targetWidth, int targetHeight) override;

    bool isTimeInRange(muse::secs_t projectTime) const override;

    muse::async::Notification attachedChanged() const override;
    muse::async::Notification frameReady() const override;

private:
    IVideoDecodeBackendPtr m_backend;
    std::unique_ptr<VideoFrameCache> m_cache;
    std::unique_ptr<VideoDecodeWorker> m_worker;

    std::string m_path;
    VideoError m_error = VideoError::None;

    muse::async::Notification m_attachedChanged;
    muse::async::Notification m_frameReady;

    static const VideoStreamInfo s_emptyInfo;
};
}

#endif // AU_VIDEO_VIDEOSERVICE_H
