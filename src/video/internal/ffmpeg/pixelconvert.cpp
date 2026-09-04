/*
* Audacity: A Digital Audio Editor
*/
#include "pixelconvert.h"

#include <algorithm>

namespace {
//! 8.8 fixed point coefficients. Limited range maps 16..235 onto 0..255; full
//! range is already 0..255 and only needs the chroma terms.
struct YuvCoeffs {
    int yOffset;
    int yGain;
    int rCr, gCb, gCr, bCb;
};

constexpr YuvCoeffs BT601_LIMITED { 16, 298, 409, -100, -208, 516 };
constexpr YuvCoeffs BT709_LIMITED { 16, 298, 459,  -55, -136, 541 };
constexpr YuvCoeffs BT601_FULL    {  0, 256, 359,  -88, -183, 454 };
constexpr YuvCoeffs BT709_FULL    {  0, 256, 403,  -48, -120, 475 };

const YuvCoeffs& pickCoeffs(AudacityAVPixelFormat format,
                            AudacityAVColorSpace space,
                            AudacityAVColorRange range,
                            int height)
{
    // yuvj420p is full range by definition, whatever the range field says.
    const bool full = range == AUDACITY_AVCOL_RANGE_JPEG
                      || format == AUDACITY_AV_PIX_FMT_YUVJ420P;

    bool bt709 = false;
    switch (space) {
    case AUDACITY_AVCOL_SPC_BT709:
        bt709 = true;
        break;
    case AUDACITY_AVCOL_SPC_BT601:
        bt709 = false;
        break;
    default:
        // Unspecified is very common. Resolution is the conventional tiebreak:
        // standard definition is 601, anything larger is 709.
        bt709 = height > 576;
        break;
    }

    if (full) {
        return bt709 ? BT709_FULL : BT601_FULL;
    }
    return bt709 ? BT709_LIMITED : BT601_LIMITED;
}

inline uint8_t clamp8(int v)
{
    return static_cast<uint8_t>(v < 0 ? 0 : (v > 255 ? 255 : v));
}
}

namespace au::video {
bool isSupportedPixelFormat(AudacityAVPixelFormat format)
{
    return format == AUDACITY_AV_PIX_FMT_YUV420P
           || format == AUDACITY_AV_PIX_FMT_YUVJ420P;
}

QImage yuv420ToImage(const uint8_t* const data[3], const int lineSize[3],
                     int srcWidth, int srcHeight,
                     int dstWidth, int dstHeight,
                     AudacityAVPixelFormat format,
                     AudacityAVColorSpace colorSpace,
                     AudacityAVColorRange colorRange)
{
    if (!isSupportedPixelFormat(format)) {
        return QImage();
    }
    if (srcWidth <= 0 || srcHeight <= 0 || dstWidth <= 0 || dstHeight <= 0) {
        return QImage();
    }
    if (data[0] == nullptr || data[1] == nullptr || data[2] == nullptr) {
        return QImage();
    }

    const YuvCoeffs& c = pickCoeffs(format, colorSpace, colorRange, srcHeight);

    QImage image(dstWidth, dstHeight, QImage::Format_RGB888);
    if (image.isNull()) {
        return image;
    }

    for (int dy = 0; dy < dstHeight; ++dy) {
        const int sy0 = static_cast<int>(static_cast<int64_t>(dy) * srcHeight / dstHeight);
        const int sy1 = std::max(sy0 + 1,
                                 static_cast<int>(static_cast<int64_t>(dy + 1) * srcHeight / dstHeight));

        uint8_t* out = image.scanLine(dy);

        for (int dx = 0; dx < dstWidth; ++dx) {
            const int sx0 = static_cast<int>(static_cast<int64_t>(dx) * srcWidth / dstWidth);
            const int sx1 = std::max(sx0 + 1,
                                     static_cast<int>(static_cast<int64_t>(dx + 1) * srcWidth / dstWidth));

            int accY = 0, accU = 0, accV = 0, n = 0;
            for (int sy = sy0; sy < sy1; ++sy) {
                const uint8_t* rowY = data[0] + static_cast<size_t>(sy) * lineSize[0];
                const uint8_t* rowU = data[1] + static_cast<size_t>(sy >> 1) * lineSize[1];
                const uint8_t* rowV = data[2] + static_cast<size_t>(sy >> 1) * lineSize[2];
                for (int sx = sx0; sx < sx1; ++sx) {
                    accY += rowY[sx];
                    accU += rowU[sx >> 1];
                    accV += rowV[sx >> 1];
                    ++n;
                }
            }

            const int y = accY / n;
            const int u = accU / n - 128;
            const int v = accV / n - 128;

            // The +128 rounds rather than truncates. Without it the top of the
            // limited range lands on 254 instead of 255, so nothing is ever
            // quite white.
            const int yy = (y - c.yOffset) * c.yGain + 128;

            out[dx * 3 + 0] = clamp8((yy + c.rCr * v) >> 8);
            out[dx * 3 + 1] = clamp8((yy + c.gCb * u + c.gCr * v) >> 8);
            out[dx * 3 + 2] = clamp8((yy + c.bCb * u) >> 8);
        }
    }

    return image;
}
}
