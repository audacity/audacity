/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOSERVICE_H
#define AU_VIDEO_VIDEOSERVICE_H

#include <memory>
#include <string>

#include "global/async/asyncable.h"
#include "modularity/ioc.h"

#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"

namespace au::au3 {
class ProjectVideoRef;
}

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
class VideoService : public IVideoService, public muse::async::Asyncable, public muse::Contextable
{
public:
    explicit VideoService(const muse::modularity::ContextPtr& ctx);

    //! Subscribes to the project changing, so an attachment saved with a
    //! project is restored when it is opened again.
    void init();

    //! Tears down the decoder without touching the project's record. Used when
    //! switching projects, where the old project's attachment must stay
    //! recorded against the old one, and when the application quits, which is
    //! not the user detaching anything.
    void detachWithoutClearingProject();
    ~VideoService() override;

    VideoError attach(const std::string& path) override;

    //! Whether an attach should mark the project as changed. Restoring a
    //! saved attachment must not: it writes back what it read.
    enum class CommitToProject {
        No,
        Yes
    };

    VideoError attach(const std::string& path, CommitToProject commit);
    void detach() override;

    bool isAttached() const override;
    bool hasRecordedAttachment() const override;
    bool sourceMismatch() const override;
    std::string attachedPath() const override;
    VideoError lastError() const override;

    const VideoStreamInfo& streamInfo() const override;

    muse::secs_t offset() const override;
    void setOffset(muse::secs_t offset) override;
    muse::async::Notification offsetChanged() const override;

    VideoFrame cachedFrameAt(muse::secs_t projectTime, bool* covers = nullptr) const override;
    void setViewSize(int width, int height) override;
    void requestFrame(muse::secs_t projectTime) override;

    bool isTimeInRange(muse::secs_t projectTime) const override;

    muse::async::Notification attachedChanged() const override;
    muse::async::Notification frameReady() const override;

private:
    //! Reads the attachment back out of the project that has just been made
    //! current, and tries to open it.
    void restoreFromProject();

    //! Writes the current attachment into the project so it is saved with it.
    void storeInProject();

    //! Commits an attach or detach into the project so it survives a close.
    void commitProjectChange();

    void stopDecoding();

    //! Absolute path of the directory the project lives in, or empty when it
    //! has never been saved.
    std::string projectDirectory() const;

    //! The current project's video record, or null when there is no project.
    au::au3::ProjectVideoRef* projectRef() const;
    muse::secs_t toVideoTime(muse::secs_t projectTime) const;

    muse::ContextInject<context::IGlobalContext> globalContext { this };

    //! Only to commit an attach or detach so it is not lost on close.
    muse::ContextInject<trackedit::IProjectHistory> projectHistory { this };

    IVideoDecodeBackendPtr m_backend;
    std::unique_ptr<VideoFrameCache> m_cache;
    std::unique_ptr<VideoDecodeWorker> m_worker;

    std::string m_path;
    VideoError m_error = VideoError::None;
    muse::secs_t m_offset { 0.0 };

    //! Largest decode size any view has asked for; see requestFrame().
    int m_targetWidth = 0;
    int m_targetHeight = 0;
    muse::async::Notification m_offsetChanged;

    muse::async::Notification m_attachedChanged;
    muse::async::Notification m_frameReady;

    //! Set when the reopened file disagrees with what was recorded, which
    //! means the path still resolves but points at different material.
    bool m_sourceMismatch = false;

    static const VideoStreamInfo s_emptyInfo;
};
}

#endif // AU_VIDEO_VIDEOSERVICE_H
