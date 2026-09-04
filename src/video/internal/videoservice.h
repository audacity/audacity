/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOSERVICE_H
#define AU_VIDEO_VIDEOSERVICE_H

#include <memory>
#include <string>

#include "modularity/ioc.h"

#include "../ivideodecodebackend.h"
#include "../ivideoservice.h"

namespace au::video {
class VideoService : public IVideoService, public muse::Contextable
{
public:
    explicit VideoService(const muse::modularity::ContextPtr& ctx);

    VideoError attach(const std::string& path) override;
    void detach() override;

    bool isAttached() const override;
    std::string attachedPath() const override;
    VideoError lastError() const override;

    const VideoStreamInfo& streamInfo() const override;

    VideoFrame frameAt(muse::secs_t projectTime,
                       int targetWidth, int targetHeight) override;

    bool isTimeInRange(muse::secs_t projectTime) const override;

    muse::async::Notification attachedChanged() const override;

private:
    IVideoDecodeBackendPtr m_backend;
    std::string m_path;
    VideoError m_error = VideoError::None;
    muse::async::Notification m_attachedChanged;

    static const VideoStreamInfo s_emptyInfo;
};
}

#endif // AU_VIDEO_VIDEOSERVICE_H
