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
    virtual std::string attachedPath() const = 0;
    virtual VideoError lastError() const = 0;

    virtual const VideoStreamInfo& streamInfo() const = 0;

    //! Frame covering the given project time, at the requested size in device
    //! pixels. Returns an invalid frame when nothing is attached or the time
    //! falls outside the video.
    virtual VideoFrame frameAt(muse::secs_t projectTime,
                               int targetWidth, int targetHeight) = 0;

    //! Whether the attached video actually covers this project time. Outside
    //! it there is nothing to show, and the panel says so rather than leaving
    //! the last decoded frame on screen looking current.
    virtual bool isTimeInRange(muse::secs_t projectTime) const = 0;

    //! Fires when a video is attached or detached, or the error state changes.
    virtual muse::async::Notification attachedChanged() const = 0;
};
}

#endif // AU_VIDEO_IVIDEOSERVICE_H
