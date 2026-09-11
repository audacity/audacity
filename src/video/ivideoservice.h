/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_IVIDEOSERVICE_H
#define AU_VIDEO_IVIDEOSERVICE_H

#include <string>

#include "global/async/notification.h"
#include "global/types/secs.h"
#include "modularity/imoduleinterface.h"

#include "videotypes.h"

namespace au::video {
//! Owns the video attached to the current project and answers "what frame is
//! showing at this moment on the timeline".
//!
//! Per project context: there is one player and one transport per project
//! window, so a video follows the project it was attached to.
class IVideoService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IVideoService)

public:
    virtual ~IVideoService() = default;

    //! Opens a file and makes it the project's video. Returns the reason it
    //! could not be used, which the panel shows rather than going blank.
    virtual VideoError attach(const std::string& path) = 0;
    virtual void detach() = 0;

    virtual bool isAttached() const = 0;

    //! Whether this project has a video associated with it at all, including
    //! one whose file did not resolve on open. Distinct from isAttached(),
    //! which is "currently decodable" - a video on an unplugged drive is
    //! still the project's video, and must not be replaced behind the user's
    //! back.
    virtual bool hasRecordedAttachment() const = 0;

    //! The file that was reopened does not match what was recorded with the
    //! project - different duration or frame rate. The path resolved, but to
    //! different material, which is worse than not resolving at all because
    //! nothing else would ever notice.
    virtual bool sourceMismatch() const = 0;
    virtual std::string attachedPath() const = 0;
    virtual VideoError lastError() const = 0;

    virtual const VideoStreamInfo& streamInfo() const = 0;

    //! Seconds the picture is shifted along the timeline. Positive means the
    //! video runs late: at project time t the frame shown is the one at
    //! t - offset. Saved with the project.
    virtual muse::secs_t offset() const = 0;
    virtual void setOffset(muse::secs_t offset) = 0;

    //! Fires when the offset changes, so the panel can redraw at once.
    virtual muse::async::Notification offsetChanged() const = 0;

    //! What is already decoded for this project time. Returns immediately, so
    //! it is safe to call on every repaint. `covers` says whether the frame's
    //! own interval contains the time or whether it is an earlier one held
    //! over because the decoder has not caught up.
    virtual VideoFrame cachedFrameAt(muse::secs_t projectTime, bool* covers = nullptr) const = 0;

    //! How large this view wants the picture, in device pixels. Call on every
    //! repaint, whether or not a frame is wanted: the decode size is the
    //! largest any view has asked for, and a view that keeps hitting the cache
    //! would otherwise never say how big it is.
    virtual void setViewSize(int width, int height) = 0;

    //! Asks the decoder for this time. Returns at once; the frame appears in
    //! the cache later and frameReady() fires. Requests supersede one another,
    //! so calling this on every position report is cheap. The size comes from
    //! setViewSize().
    virtual void requestFrame(muse::secs_t projectTime) = 0;

    //! Fires on the GUI thread once a newly decoded frame is available.
    virtual muse::async::Notification frameReady() const = 0;

    //! Whether the attached video actually covers this project time. Outside
    //! it there is nothing to show, and the panel says so rather than leaving
    //! the last decoded frame on screen looking current.
    virtual bool isTimeInRange(muse::secs_t projectTime) const = 0;

    //! Fires when a video is attached or detached, or the error state changes.
    virtual muse::async::Notification attachedChanged() const = 0;
};
}

#endif // AU_VIDEO_IVIDEOSERVICE_H
