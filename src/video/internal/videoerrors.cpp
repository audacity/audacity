/*
* Audacity: A Digital Audio Editor
*/
#include "../videotypes.h"

namespace au::video {
std::string errorMessage(VideoError err)
{
    switch (err) {
    case VideoError::None:
        return {};
    case VideoError::FFmpegNotFound:
        return "FFmpeg not found";
    case VideoError::FFmpegTooOld:
        return "This FFmpeg build is too old for video preview "
               "(FFmpeg 3.4 or newer required). Audio import is unaffected.";
    case VideoError::FileNotFound:
        return "Video file not found";
    case VideoError::CannotOpen:
        return "Cannot open this file";
    case VideoError::NoVideoStream:
        return "This file has no video track";
    case VideoError::NoDecoder:
        return "This FFmpeg build cannot decode this video codec";
    case VideoError::UnsupportedFormat:
        return "This video uses a pixel format that is not supported yet";
    case VideoError::UnsupportedHdr:
        return "HDR video is not supported yet";
    case VideoError::DecodeFailed:
        return "Could not decode this video";
    }
    return {};
}
}
