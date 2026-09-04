/*
* Audacity: A Digital Audio Editor
*/
#include "../videotypes.h"

#include "translation.h"

namespace au::video {
QString errorMessage(VideoError err)
{
    switch (err) {
    case VideoError::None:
        return QString();
    case VideoError::FFmpegNotFound:
        return muse::qtrc("video", "FFmpeg not found");
    case VideoError::FFmpegTooOld:
        return muse::qtrc("video", "This FFmpeg build is too old for video preview. FFmpeg 3.4 or newer is required; audio import is unaffected.");
    case VideoError::FileNotFound:
        return muse::qtrc("video", "Video file not found");
    case VideoError::CannotOpen:
        return muse::qtrc("video", "Cannot open this file");
    case VideoError::NoVideoStream:
        return muse::qtrc("video", "This file has no video track");
    case VideoError::NoDecoder:
        return muse::qtrc("video", "This FFmpeg build cannot decode this video codec");
    case VideoError::UnsupportedFormat:
        return muse::qtrc("video", "This video uses a pixel format that is not supported yet");
    case VideoError::UnsupportedHdr:
        return muse::qtrc("video", "HDR video is not supported yet");
    case VideoError::DecodeFailed:
        return muse::qtrc("video", "Could not decode this video");
    }
    return QString();
}
}
