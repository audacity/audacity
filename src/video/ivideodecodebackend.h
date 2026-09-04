/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_IVIDEODECODEBACKEND_H
#define AU_VIDEO_IVIDEODECODEBACKEND_H

#include <memory>
#include <string>

#include "global/types/secs.h"

#include "videotypes.h"

namespace au::video {
//! The decoder seam.
//!
//! There is one implementation, backed by the FFmpeg that Audacity already
//! loads at runtime for audio import. It is an interface anyway so that a
//! different decoder can be substituted later without the sync controller,
//! the persistence, or the QML knowing about it.
class IVideoDecodeBackend
{
public:
    virtual ~IVideoDecodeBackend() = default;

    //! Opens a file and probes its first video stream. Returns the reason on
    //! failure; the caller is expected to show it rather than a blank panel.
    virtual VideoError open(const std::string& path) = 0;
    virtual void close() = 0;
    virtual bool isOpen() const = 0;

    virtual const VideoStreamInfo& streamInfo() const = 0;

    //! Returns the frame covering the given content-relative time, converted
    //! and scaled to fit inside the requested box while keeping the source
    //! display aspect ratio, so the caller centres it rather than stretching
    //! it. The size is in device pixels, not
    //! logical ones: the caller multiplies by the device pixel ratio, because
    //! decoding at logical size renders at half resolution on a 2x display.
    virtual VideoFrame frameAt(muse::secs_t time, int targetWidth, int targetHeight) = 0;

    //! Content-relative time to a timestamp on the video stream's timeline.
    //! Reads only state fixed at open(), so unlike frameAt() it is safe to
    //! call from another thread while the decoder is working.
    virtual int64_t timeToPts(muse::secs_t time) const = 0;

    //! How long a frame is shown for, in the video stream's own time base.
    //! Used to decide which requests a cached frame covers. Only a fallback
    //! for containers that carry no per-packet duration; variable frame rate
    //! content legitimately holds a single frame for seconds.
    virtual int64_t frameDurationPts() const = 0;
};

using IVideoDecodeBackendPtr = std::shared_ptr<IVideoDecodeBackend>;
}

#endif // AU_VIDEO_IVIDEODECODEBACKEND_H
