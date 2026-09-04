/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <vector>

#include "internal/ffmpeg/pixelconvert.h"

using namespace au::video;

namespace {
//! A planar YUV 4:2:0 image built in memory, with the padded line sizes a real
//! decoded frame would have so the converter is exercised against strides that
//! are wider than the picture.
class Yuv420Image
{
public:
    Yuv420Image(int width, int height, uint8_t y, uint8_t u, uint8_t v)
        : m_width(width), m_height(height)
    {
        const int chromaW = (width + 1) / 2;
        const int chromaH = (height + 1) / 2;

        // Deliberately over-wide strides: decoders align their planes, and a
        // converter that assumes stride == width reads the wrong pixels.
        m_strideY = width + 7;
        m_strideC = chromaW + 3;

        m_y.assign(static_cast<size_t>(m_strideY) * height, y);
        m_u.assign(static_cast<size_t>(m_strideC) * chromaH, u);
        m_v.assign(static_cast<size_t>(m_strideC) * chromaH, v);
    }

    void setLumaColumnRange(int fromX, int toX, uint8_t value)
    {
        for (int row = 0; row < m_height; ++row) {
            for (int x = fromX; x < toX; ++x) {
                m_y[static_cast<size_t>(row) * m_strideY + x] = value;
            }
        }
    }

    const uint8_t* const* data()
    {
        m_planes[0] = m_y.data();
        m_planes[1] = m_u.data();
        m_planes[2] = m_v.data();
        return m_planes;
    }

    const int* lineSize()
    {
        m_lineSize[0] = m_strideY;
        m_lineSize[1] = m_strideC;
        m_lineSize[2] = m_strideC;
        return m_lineSize;
    }

    int width() const { return m_width; }
    int height() const { return m_height; }

private:
    int m_width = 0;
    int m_height = 0;
    int m_strideY = 0;
    int m_strideC = 0;
    std::vector<uint8_t> m_y, m_u, m_v;
    const uint8_t* m_planes[3] {};
    int m_lineSize[3] {};
};

struct Rgb {
    int r = 0, g = 0, b = 0;
    bool operator==(const Rgb& o) const { return r == o.r && g == o.g && b == o.b; }
};

std::ostream& operator<<(std::ostream& os, const Rgb& c)
{
    return os << "rgb(" << c.r << ", " << c.g << ", " << c.b << ")";
}

Rgb pixelAt(const QImage& image, int x, int y)
{
    const uint8_t* line = image.constScanLine(y);
    return { line[x * 3 + 0], line[x * 3 + 1], line[x * 3 + 2] };
}

//! Converts a solid colour and returns the single resulting pixel.
Rgb convertSolid(uint8_t y, uint8_t u, uint8_t v,
                 AudacityAVPixelFormat format,
                 AudacityAVColorSpace space,
                 AudacityAVColorRange range,
                 int sourceHeight = 720)
{
    Yuv420Image src(16, sourceHeight, y, u, v);
    const QImage out = yuv420ToImage(src.data(), src.lineSize(),
                                     src.width(), src.height(),
                                     4, 4, format, space, range);
    EXPECT_FALSE(out.isNull());
    return pixelAt(out, 1, 1);
}
}

// ---------------------------------------------------------------------------
// Rejected input. Every one of these has to produce a null image rather than
// reading out of bounds, because the pixel format of a frame is whatever the
// file said it was.
// ---------------------------------------------------------------------------

TEST(PixelConvertTests, RejectsUnsupportedPixelFormat)
{
    Yuv420Image src(16, 16, 128, 128, 128);
    EXPECT_TRUE(yuv420ToImage(src.data(), src.lineSize(), 16, 16, 8, 8,
                              AUDACITY_AV_PIX_FMT_UNSUPPORTED,
                              AUDACITY_AVCOL_SPC_BT709,
                              AUDACITY_AVCOL_RANGE_MPEG).isNull());

    EXPECT_TRUE(yuv420ToImage(src.data(), src.lineSize(), 16, 16, 8, 8,
                              AUDACITY_AV_PIX_FMT_NONE,
                              AUDACITY_AVCOL_SPC_BT709,
                              AUDACITY_AVCOL_RANGE_MPEG).isNull());
}

TEST(PixelConvertTests, RejectsNullPlanes)
{
    const uint8_t* planes[3] = { nullptr, nullptr, nullptr };
    const int lineSize[3] = { 16, 8, 8 };
    EXPECT_TRUE(yuv420ToImage(planes, lineSize, 16, 16, 8, 8,
                              AUDACITY_AV_PIX_FMT_YUV420P,
                              AUDACITY_AVCOL_SPC_BT709,
                              AUDACITY_AVCOL_RANGE_MPEG).isNull());
}

TEST(PixelConvertTests, RejectsNonPositiveDimensions)
{
    Yuv420Image src(16, 16, 128, 128, 128);
    for (const auto& dims : std::vector<std::array<int, 4> >{
             { 0, 16, 8, 8 }, { 16, 0, 8, 8 }, { 16, 16, 0, 8 }, { 16, 16, 8, 0 },
             { -4, 16, 8, 8 }, { 16, 16, 8, -1 } }) {
        EXPECT_TRUE(yuv420ToImage(src.data(), src.lineSize(),
                                  dims[0], dims[1], dims[2], dims[3],
                                  AUDACITY_AV_PIX_FMT_YUV420P,
                                  AUDACITY_AVCOL_SPC_BT709,
                                  AUDACITY_AVCOL_RANGE_MPEG).isNull());
    }
}

TEST(PixelConvertTests, ReportsWhichFormatsAreSupported)
{
    EXPECT_TRUE(isSupportedPixelFormat(AUDACITY_AV_PIX_FMT_YUV420P));
    EXPECT_TRUE(isSupportedPixelFormat(AUDACITY_AV_PIX_FMT_YUVJ420P));
    EXPECT_FALSE(isSupportedPixelFormat(AUDACITY_AV_PIX_FMT_UNSUPPORTED));
    EXPECT_FALSE(isSupportedPixelFormat(AUDACITY_AV_PIX_FMT_NONE));
}

// ---------------------------------------------------------------------------
// Output shape
// ---------------------------------------------------------------------------

TEST(PixelConvertTests, ProducesExactlyTheRequestedSize)
{
    Yuv420Image src(1280, 720, 128, 128, 128);
    const QImage out = yuv420ToImage(src.data(), src.lineSize(), 1280, 720,
                                     637, 361,
                                     AUDACITY_AV_PIX_FMT_YUV420P,
                                     AUDACITY_AVCOL_SPC_BT709,
                                     AUDACITY_AVCOL_RANGE_MPEG);
    ASSERT_FALSE(out.isNull());
    EXPECT_EQ(out.width(), 637);
    EXPECT_EQ(out.height(), 361);
    EXPECT_EQ(out.format(), QImage::Format_RGB888);
}

TEST(PixelConvertTests, HandlesOddDimensionsAndUpscaling)
{
    // Odd sizes make the chroma planes smaller than half, which is where an
    // off-by-one reads past the end.
    Yuv420Image src(7, 5, 235, 128, 128);
    const QImage out = yuv420ToImage(src.data(), src.lineSize(), 7, 5, 19, 13,
                                     AUDACITY_AV_PIX_FMT_YUV420P,
                                     AUDACITY_AVCOL_SPC_BT709,
                                     AUDACITY_AVCOL_RANGE_MPEG);
    ASSERT_FALSE(out.isNull());
    EXPECT_EQ(out.width(), 19);
    EXPECT_EQ(out.height(), 13);
    EXPECT_EQ(pixelAt(out, 9, 6), (Rgb { 255, 255, 255 }));
}

// ---------------------------------------------------------------------------
// Range. Limited range has to reach both ends: the top of it landing on 254
// means nothing in the picture is ever quite white.
// ---------------------------------------------------------------------------

TEST(PixelConvertTests, LimitedRangeReachesBlackAndWhite)
{
    EXPECT_EQ(convertSolid(16, 128, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                           AUDACITY_AVCOL_SPC_BT709, AUDACITY_AVCOL_RANGE_MPEG),
              (Rgb { 0, 0, 0 }));

    EXPECT_EQ(convertSolid(235, 128, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                           AUDACITY_AVCOL_SPC_BT709, AUDACITY_AVCOL_RANGE_MPEG),
              (Rgb { 255, 255, 255 }));

    EXPECT_EQ(convertSolid(126, 128, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                           AUDACITY_AVCOL_SPC_BT709, AUDACITY_AVCOL_RANGE_MPEG),
              (Rgb { 128, 128, 128 }));
}

TEST(PixelConvertTests, LimitedRangeClampsBeyondTheNominalRange)
{
    // Sub-black and super-white are legal in a stream and must not wrap.
    EXPECT_EQ(convertSolid(0, 128, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                           AUDACITY_AVCOL_SPC_BT709, AUDACITY_AVCOL_RANGE_MPEG),
              (Rgb { 0, 0, 0 }));

    EXPECT_EQ(convertSolid(255, 128, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                           AUDACITY_AVCOL_SPC_BT709, AUDACITY_AVCOL_RANGE_MPEG),
              (Rgb { 255, 255, 255 }));
}

TEST(PixelConvertTests, FullRangeUsesTheWholeScale)
{
    EXPECT_EQ(convertSolid(0, 128, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                           AUDACITY_AVCOL_SPC_BT601, AUDACITY_AVCOL_RANGE_JPEG),
              (Rgb { 0, 0, 0 }));

    EXPECT_EQ(convertSolid(255, 128, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                           AUDACITY_AVCOL_SPC_BT601, AUDACITY_AVCOL_RANGE_JPEG),
              (Rgb { 255, 255, 255 }));

    EXPECT_EQ(convertSolid(128, 128, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                           AUDACITY_AVCOL_SPC_BT601, AUDACITY_AVCOL_RANGE_JPEG),
              (Rgb { 128, 128, 128 }));
}

TEST(PixelConvertTests, YuvjIsFullRangeWhateverTheRangeFieldSays)
{
    // yuvj420p means full range by definition. Files routinely leave the range
    // field unset, and treating those as limited crushes the blacks.
    EXPECT_EQ(convertSolid(0, 128, 128, AUDACITY_AV_PIX_FMT_YUVJ420P,
                           AUDACITY_AVCOL_SPC_BT601,
                           AUDACITY_AVCOL_RANGE_UNSPECIFIED),
              (Rgb { 0, 0, 0 }));

    EXPECT_EQ(convertSolid(255, 128, 128, AUDACITY_AV_PIX_FMT_YUVJ420P,
                           AUDACITY_AVCOL_SPC_BT601,
                           AUDACITY_AVCOL_RANGE_UNSPECIFIED),
              (Rgb { 255, 255, 255 }));
}

// ---------------------------------------------------------------------------
// Colour matrices
// ---------------------------------------------------------------------------

TEST(PixelConvertTests, Bt709AndBt601DifferOnSaturatedChroma)
{
    const Rgb bt709 = convertSolid(126, 128, 255, AUDACITY_AV_PIX_FMT_YUV420P,
                                   AUDACITY_AVCOL_SPC_BT709,
                                   AUDACITY_AVCOL_RANGE_MPEG);
    const Rgb bt601 = convertSolid(126, 128, 255, AUDACITY_AV_PIX_FMT_YUV420P,
                                   AUDACITY_AVCOL_SPC_BT601,
                                   AUDACITY_AVCOL_RANGE_MPEG);

    EXPECT_EQ(bt709, (Rgb { 255, 61, 128 }));
    EXPECT_EQ(bt601, (Rgb { 255, 25, 128 }));
    EXPECT_NE(bt709, bt601);
}

TEST(PixelConvertTests, ChromaMovesTheExpectedChannels)
{
    // Maximum V is the red axis; maximum U is the blue axis.
    const Rgb red = convertSolid(126, 128, 255, AUDACITY_AV_PIX_FMT_YUV420P,
                                 AUDACITY_AVCOL_SPC_BT709,
                                 AUDACITY_AVCOL_RANGE_MPEG);
    EXPECT_EQ(red.r, 255);
    EXPECT_LT(red.g, 128);
    EXPECT_EQ(red.b, 128);

    const Rgb blue = convertSolid(126, 255, 128, AUDACITY_AV_PIX_FMT_YUV420P,
                                  AUDACITY_AVCOL_SPC_BT709,
                                  AUDACITY_AVCOL_RANGE_MPEG);
    EXPECT_EQ(blue.b, 255);
    EXPECT_LT(blue.g, 128);
    EXPECT_EQ(blue.r, 128);
}

TEST(PixelConvertTests, UnspecifiedColorSpaceFallsBackOnResolution)
{
    // The conventional tiebreak: standard definition is 601, HD and above 709.
    const Rgb sd = convertSolid(126, 128, 255, AUDACITY_AV_PIX_FMT_YUV420P,
                                AUDACITY_AVCOL_SPC_UNSPECIFIED,
                                AUDACITY_AVCOL_RANGE_MPEG, 480);
    const Rgb hd = convertSolid(126, 128, 255, AUDACITY_AV_PIX_FMT_YUV420P,
                                AUDACITY_AVCOL_SPC_UNSPECIFIED,
                                AUDACITY_AVCOL_RANGE_MPEG, 720);

    EXPECT_EQ(sd, (Rgb { 255, 25, 128 }));    // BT.601
    EXPECT_EQ(hd, (Rgb { 255, 61, 128 }));    // BT.709
}

// ---------------------------------------------------------------------------
// Downscaling. Point sampling a large frame into a small panel turns burnt-in
// timecode and slates into noise, and reading those is much of the reason
// anyone scrubs video, so the averaging is a requirement rather than polish.
// ---------------------------------------------------------------------------

TEST(PixelConvertTests, DownscaleAveragesRatherThanPointSamples)
{
    Yuv420Image src(4, 4, 16, 128, 128);
    src.setLumaColumnRange(2, 4, 235);   // left half black, right half white

    const QImage out = yuv420ToImage(src.data(), src.lineSize(), 4, 4, 1, 1,
                                     AUDACITY_AV_PIX_FMT_YUV420P,
                                     AUDACITY_AVCOL_SPC_BT709,
                                     AUDACITY_AVCOL_RANGE_MPEG);
    ASSERT_FALSE(out.isNull());

    // Point sampling would give 0 or 255; the mean of the two is what proves
    // every source pixel was read.
    EXPECT_EQ(pixelAt(out, 0, 0), (Rgb { 127, 127, 127 }));
}

TEST(PixelConvertTests, DownscaleKeepsAThinFeatureVisible)
{
    // A one-pixel white line in an otherwise black 64-wide frame, reduced to
    // 8 across. Point sampling loses it entirely most of the time; averaging
    // always leaves something behind.
    Yuv420Image src(64, 8, 16, 128, 128);
    src.setLumaColumnRange(33, 34, 235);

    const QImage out = yuv420ToImage(src.data(), src.lineSize(), 64, 8, 8, 8,
                                     AUDACITY_AV_PIX_FMT_YUV420P,
                                     AUDACITY_AVCOL_SPC_BT709,
                                     AUDACITY_AVCOL_RANGE_MPEG);
    ASSERT_FALSE(out.isNull());

    const Rgb lit = pixelAt(out, 4, 4);
    EXPECT_GT(lit.r, 0) << "the thin line was dropped entirely";
    EXPECT_LT(lit.r, 255) << "the line should be averaged with its neighbours";
}

TEST(PixelConvertTests, SolidColourSurvivesAnyScale)
{
    Yuv420Image src(300, 200, 235, 128, 128);
    for (const auto& size : std::vector<std::pair<int, int> >{
             { 1, 1 }, { 7, 3 }, { 150, 100 }, { 300, 200 }, { 640, 360 } }) {
        const QImage out = yuv420ToImage(src.data(), src.lineSize(), 300, 200,
                                         size.first, size.second,
                                         AUDACITY_AV_PIX_FMT_YUV420P,
                                         AUDACITY_AVCOL_SPC_BT709,
                                         AUDACITY_AVCOL_RANGE_MPEG);
        ASSERT_FALSE(out.isNull());
        EXPECT_EQ(pixelAt(out, size.first / 2, size.second / 2),
                  (Rgb { 255, 255, 255 }))
            << "at " << size.first << "x" << size.second;
    }
}
