/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOTYPES_H
#define AU_VIDEO_VIDEOTYPES_H

#include <cstdint>
#include <string>

#include <QImage>
#include <QString>

#include "global/types/ratio.h"
#include "global/types/secs.h"

namespace au::video {
//! Why the panel has nothing to show. Every one of these is a state the user
//! can be told about plainly, rather than an empty rectangle.
enum class VideoError {
    None = 0,
    FFmpegNotFound,     //!< no usable FFmpeg on this machine
    FFmpegTooOld,       //!< found, but without the send/receive decoding API
    FileNotFound,
    CannotOpen,
    NoVideoStream,
    NoDecoder,          //!< the codec is not in this FFmpeg build
    UnsupportedFormat,  //!< pixel format the converter does not handle yet
    UnsupportedHdr,     //!< high dynamic range transfer function
    DecodeFailed,
};

//! Translated, like every other string the panel shows.
QString errorMessage(VideoError err);

//! A decoded frame, already converted and scaled to the size that was asked
//! for. Owns its pixels: the decode worker reuses its own buffers, so nothing
//! that points into them may outlive a request.
struct VideoFrame {
    QImage image;
    int64_t pts = 0;          //!< raw presentation timestamp, stream time base
    muse::secs_t time = 0.0;  //!< pts converted to content-relative seconds
    bool valid() const { return !image.isNull(); }
};

//! What is known about an attached video once it has been probed.
struct VideoStreamInfo {
    int streamIndex = -1;
    int width = 0;
    int height = 0;
    muse::secs_t duration = 0.0;
    double frameRate = 0.0;

    //! Start time of the stream Audacity imported audio from, in seconds.
    //! Frame timestamps sit on the same container timeline, so this is the
    //! anchor that maps project time to a frame.
    muse::secs_t audioStartTime = 0.0;
    muse::secs_t videoStartTime = 0.0;

    //! Whether the file carries audio at all. Without this a start time of
    //! zero is indistinguishable from having no audio stream, which matters
    //! to anything that would offer to put that audio on the timeline.
    bool hasAudioStream = false;

    bool isValid() const { return streamIndex >= 0 && width > 0 && height > 0; }
};
}

#endif // AU_VIDEO_VIDEOTYPES_H
