/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_PIXELCONVERT_H
#define AU_VIDEO_PIXELCONVERT_H

#include <cstdint>

#include <QImage>

#include "mod-ffmpeg/lib-ffmpeg-support/FFmpegTypes.h"

namespace au::video {
//! Converts a planar 8-bit YUV 4:2:0 frame to RGB at an arbitrary target size,
//! in one pass.
//!
//! The scale is fused into the conversion on purpose: the panel is small and
//! the source may be 4K, so converting at full size first would do an order of
//! magnitude more work than the result needs. Each destination pixel averages
//! the whole source rectangle that maps onto it rather than point-sampling it,
//! because point sampling turns burnt-in timecode and slates into noise, and
//! reading those is a large part of why anyone scrubs video.
//!
//! Returns a null image if the format is not one this handles.
QImage yuv420ToImage(const uint8_t* const data[3], const int lineSize[3],
                     int srcWidth, int srcHeight,
                     int dstWidth, int dstHeight,
                     AudacityAVPixelFormat format,
                     AudacityAVColorSpace colorSpace,
                     AudacityAVColorRange colorRange);

//! Whether yuv420ToImage can handle this pixel format.
bool isSupportedPixelFormat(AudacityAVPixelFormat format);
}

#endif // AU_VIDEO_PIXELCONVERT_H
