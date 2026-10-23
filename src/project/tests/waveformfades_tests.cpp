/*
 * Audacity: A Digital Audio Editor
 */

#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

#include <algorithm>
#include <vector>

#include "au3wrap/internal/au3project.h"
#include "au3-project/Project.h"
#include "au3-screen-geometry/ZoomInfo.h"
#include "au3-clip-processing/ClipFades.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track-paint/waveform/WaveBitmapCache.h"
#include "au3-wave-track-paint/waveform/WaveDataCache.h"
#include "testing/testcontext.h"

namespace {
constexpr auto TEST_SAMPLE_RATE = 10000;
constexpr auto TEST_CLIP_DURATION = 1.0;
constexpr auto TEST_TRIM_LEFT = 0.2;
constexpr auto TEST_FADE_IN = 0.2;
constexpr auto TEST_PIXELS_PER_SECOND = 1000.0;
constexpr auto TEST_HEIGHT = 101;
//! The painted heights are rounded to rows
constexpr auto TEST_RELATIVE_HEIGHT_TOLERANCE = 3.0 / TEST_HEIGHT;
constexpr auto TEST_STRETCH_RATIO = 2.0;
//! A logarithmic fade spans the whole dB range of the display
constexpr auto TEST_DB_RANGE = -ClipFades::logarithmicFadeFloorDb;

const graphics::Color sampleColor { 255, 0, 0 };
const graphics::Color otherColor { 0, 0, 255 };

class TestBitmapElement final : public WaveBitmapCacheElement
{
public:
    uint8_t* Allocate(size_t width, size_t height) override
    {
        mWidth = width;
        mHeight = height;
        mBytes.resize(width * height * 3);
        return mBytes.data();
    }

    size_t Width() const override { return mWidth; }
    size_t Height() const override { return mHeight; }

    //! How many pixels of the column are painted with the sample color
    int SampleRows(size_t column) const
    {
        auto count = 0;
        for (size_t row = 0; row < mHeight; ++row) {
            const auto* pixel = &mBytes[(row * mWidth + column) * 3];
            if (pixel[0] == sampleColor.GetRed() && pixel[1] == sampleColor.GetGreen() && pixel[2] == sampleColor.GetBlue()) {
                ++count;
            }
        }
        return count;
    }

private:
    size_t mWidth = 0;
    size_t mHeight = 0;
    std::vector<uint8_t> mBytes;
};
}

namespace au::project {
class WaveformFadesTests : public ::testing::Test
{
protected:
    QTemporaryDir m_directory;
    au3::Au3ProjectAccessor m_accessor { au::testutils::makeTestContext() };
    std::shared_ptr<WaveTrack> m_track;
    std::shared_ptr<WaveClip> m_clip;
    std::unique_ptr<WaveBitmapCache> m_bitmapCache;
    bool m_dbScale = false;

    ::AudacityProject& project()
    {
        return *reinterpret_cast<::AudacityProject*>(m_accessor.au3ProjectPtr());
    }

    void SetUp() override
    {
        const auto path = m_directory.filePath("fades.aup4");
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", path));
        ASSERT_TRUE(m_accessor.load(path.toStdString(), false));

        //! A full-scale square wave: each column's min and max are -1 and 1, so the painted height is the gain
        m_track = WaveTrackFactory::Get(project()).Create(floatSample, TEST_SAMPLE_RATE);
        m_clip = m_track->CreateClip(0.0, "clip");
        std::vector<float> samples(TEST_CLIP_DURATION * TEST_SAMPLE_RATE);
        for (size_t i = 0; i < samples.size(); ++i) {
            samples[i] = i % 2 ? 1.f : -1.f;
        }
        auto data = reinterpret_cast<constSamplePtr>(samples.data());
        m_clip->Append(&data, floatSample, samples.size(), 1, floatSample);
        m_clip->Flush();
        m_track->InsertInterval(m_clip, true);

        m_clip->SetTrimLeft(TEST_TRIM_LEFT);
        ClipFades::Get(*m_clip).setFadeIn(TEST_FADE_IN);
    }

    void TearDown() override
    {
        m_bitmapCache.reset();
        m_accessor.close();
    }

    //! The painted height of each column of the clip's visible part, like `minmaxrmspainter.cpp` draws it
    std::vector<int> paintedHeights()
    {
        if (!m_bitmapCache) {
            auto dataCache = std::make_shared<WaveDataCache>(*m_clip, 0);
            m_bitmapCache = std::make_unique<WaveBitmapCache>(*m_clip, dataCache, [] { return std::make_unique<TestBitmapElement>(); });
        }
        auto& bitmapCache = *m_bitmapCache;

        WavePaintParameters params;
        params.SetDisplayParameters(TEST_HEIGHT, -1.0, 1.0, false)
        .SetBlankColor(otherColor)
        .SetBackgroundColors(otherColor, otherColor)
        .SetSampleColors(sampleColor, sampleColor)
        .SetRMSColors(sampleColor, sampleColor)
        .SetClippingColors(otherColor, otherColor)
        .SetDBParameters(TEST_DB_RANGE, m_dbScale);
        bitmapCache.SetPaintParameters(params);

        const ZoomInfo zoomInfo(0.0, TEST_PIXELS_PER_SECOND);
        const auto t0 = m_clip->GetTrimLeft();
        const auto t1 = t0 + m_clip->GetPlayDuration();
        std::vector<int> heights;
        auto range = bitmapCache.PerformLookup(zoomInfo, t0, t1);
        for (auto it = range.begin(); it != range.end(); ++it) {
            const auto& element = static_cast<const TestBitmapElement&>(*it);
            // As in WaveformPainter::Draw
            const auto width = WaveBitmapCache::CacheElementWidth - it.GetLeftOffset() - it.GetRightOffset();
            const auto drawableWidth = std::min<size_t>(width, element.Width() - it.GetLeftOffset());
            for (size_t column = it.GetLeftOffset(); column < it.GetLeftOffset() + drawableWidth; ++column) {
                heights.push_back(element.SampleRows(column));
            }
        }
        return heights;
    }

    //! The painted height at `t` seconds from the play start, relative to the full height
    static double relativeHeight(const std::vector<int>& heights, double t)
    {
        return heights.at(static_cast<size_t>(t * TEST_PIXELS_PER_SECOND)) / static_cast<double>(TEST_HEIGHT);
    }

    //! The painted heights follow a linear fade-in of `fadeIn` seconds
    static void expectLinearFadeIn(const std::vector<int>& heights, double fadeIn)
    {
        EXPECT_NEAR(relativeHeight(heights, fadeIn / 4), 1.0 / 4, TEST_RELATIVE_HEIGHT_TOLERANCE);
        EXPECT_NEAR(relativeHeight(heights, fadeIn / 2), 1.0 / 2, TEST_RELATIVE_HEIGHT_TOLERANCE);
        EXPECT_NEAR(relativeHeight(heights, 2 * fadeIn), 1.0, TEST_RELATIVE_HEIGHT_TOLERANCE);
    }
};

TEST_F(WaveformFadesTests, TrimmedClipIsPaintedWithItsFadeIn)
{
    expectLinearFadeIn(paintedHeights(), TEST_FADE_IN);
}

TEST_F(WaveformFadesTests, StretchedClipIsPaintedWithItsStretchedFadeIn)
{
    //! [GIVEN] the clip stretched, and its fade-in with it
    m_clip->StretchBy(TEST_STRETCH_RATIO);
    constexpr auto stretchedFadeIn = TEST_STRETCH_RATIO * TEST_FADE_IN;
    ASSERT_DOUBLE_EQ(ClipFades::Get(*m_clip).fadeIn(), stretchedFadeIn);

    expectLinearFadeIn(paintedHeights(), stretchedFadeIn);
}

TEST_F(WaveformFadesTests, FadeChangesInvalidateTheCache)
{
    constexpr auto t = TEST_FADE_IN / 2;

    //! [GIVEN] the clip painted once
    EXPECT_NEAR(relativeHeight(paintedHeights(), t), t / TEST_FADE_IN, TEST_RELATIVE_HEIGHT_TOLERANCE);

    //! [WHEN] its fade-in is lengthened
    constexpr auto longerFadeIn = 2 * TEST_FADE_IN;
    ClipFades::Get(*m_clip).setFadeIn(longerFadeIn);

    //! [THEN] painting it again, with the same cache, shows the new fade-in
    EXPECT_NEAR(relativeHeight(paintedHeights(), t), t / longerFadeIn, TEST_RELATIVE_HEIGHT_TOLERANCE);
}

TEST_F(WaveformFadesTests, LogarithmicFadeIsPaintedLinearlyInDbScale)
{
    //! [GIVEN] a logarithmic fade-in, i.e. linear in dB
    ClipFades::Get(*m_clip).setFadeInType(FadeType::Logarithmic);

    //! [WHEN] it is painted in dB scale
    m_dbScale = true;

    //! [THEN] it looks linear
    expectLinearFadeIn(paintedHeights(), TEST_FADE_IN);
}
}
