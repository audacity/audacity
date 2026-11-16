/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <algorithm>

#include "projectscene/view/tracksitemsview/wavedisplaybounds.h"
#include "projectscene/view/tracksitemsview/wavescenegraph.h"

namespace au::projectscene {
namespace {
//! Pixels per second
constexpr double ZOOM = 100.0;
constexpr double TILE_WIDTH = 256.0;

IMinMaxRMSPainter::ClipData makeClipData(double playStartTime, double playEndTime, double trimLeft = 0.0,
                                         double sequenceEndTime = -1.0)
{
    IMinMaxRMSPainter::ClipData data;
    data.valid = true;
    data.channelCount = 1;
    data.playStartTime = playStartTime;
    data.playEndTime = playEndTime;
    data.trimLeft = trimLeft;
    data.sequenceEndTime = sequenceEndTime < 0.0 ? playEndTime : sequenceEndTime;
    data.samplesPerSecond = 44100.0;
    data.tileWidth = TILE_WIDTH;
    return data;
}

WaveSamplePoint makeSample(float x, float y, bool selected = false, bool clipping = false)
{
    return WaveSamplePoint { x, y, selected, clipping };
}

std::pair<float, float> xRange(const std::vector<WaveSceneGraph::ColoredPoint>& points)
{
    const auto [minIt, maxIt] = std::minmax_element(points.begin(), points.end(), [](const auto& a, const auto& b) { return a.x < b.x; });
    return { minIt->x, maxIt->x };
}

std::pair<float, float> yRange(const std::vector<WaveSceneGraph::ColoredPoint>& points)
{
    const auto [minIt, maxIt] = std::minmax_element(points.begin(), points.end(), [](const auto& a, const auto& b) { return a.y < b.y; });
    return { minIt->y, maxIt->y };
}

ClipTime makeClipTime(double startTime, double endTime, double itemStartTime, double itemEndTime)
{
    ClipTime time;
    time.startTime = startTime;
    time.endTime = endTime;
    time.itemStartTime = itemStartTime;
    time.itemEndTime = itemEndTime;
    return time;
}
}

TEST(ProjectScene_WaveSceneGraphTests, FullyVisibleClipWindowIsExact)
{
    //! [GIVEN] A clip from 1 s to 3 s, fully visible, whose container starts a quarter pixel past a whole pixel
    const auto data = makeClipData(1.0, 3.0);
    const auto time = makeClipTime(1.0, 3.0, 1.0, 3.0);

    //! [WHEN] The window is computed
    const auto window = WaveSceneGraph::computeWindow(time, data, ZOOM, 100.25, false, false);

    //! [THEN] It covers the clip exactly, and the content offset carries the quarter pixel
    EXPECT_DOUBLE_EQ(window.fromPixelExact, 0.0);
    EXPECT_DOUBLE_EQ(window.fromPixelSnapped, 0.0);
    EXPECT_DOUBLE_EQ(window.toPixelExact, 200.0);
    EXPECT_DOUBLE_EQ(window.toPixelSnapped, 200.0);
    EXPECT_FALSE(window.hasPlaceholder);
    EXPECT_DOUBLE_EQ(window.subpixelResidual, 0.25);
    EXPECT_DOUBLE_EQ(window.contentOffset, 0.25);
}

TEST(ProjectScene_WaveSceneGraphTests, ViewportClampedEdgesSnapOutwardToTiles)
{
    //! [GIVEN] A 10 s clip of which only 3 s to 5 s is in the view
    const auto data = makeClipData(0.0, 10.0);
    const auto time = makeClipTime(0.0, 10.0, 3.0, 5.0);

    //! [WHEN] The window is computed
    const auto window = WaveSceneGraph::computeWindow(time, data, ZOOM, 300.0, false, false);

    //! [THEN] Both edges move out to whole tiles, and the content offset undoes the left snap
    EXPECT_DOUBLE_EQ(window.fromPixelExact, 300.0);
    EXPECT_DOUBLE_EQ(window.fromPixelSnapped, 256.0);
    EXPECT_DOUBLE_EQ(window.toPixelExact, 500.0);
    EXPECT_DOUBLE_EQ(window.toPixelSnapped, 512.0);
    EXPECT_DOUBLE_EQ(window.contentOffset, -44.0);
}

TEST(ProjectScene_WaveSceneGraphTests, RightSnapStopsAtTheSequenceEnd)
{
    //! [GIVEN] A clip whose audio ends at 5.0625 s, in view up to 5 s
    const auto data = makeClipData(0.0, 5.0625);
    const auto time = makeClipTime(0.0, 5.0625, 0.0, 5.0);

    //! [WHEN] The window is computed
    const auto window = WaveSceneGraph::computeWindow(time, data, ZOOM, 0.0, false, false);

    //! [THEN] The right edge snaps out, but not past the last whole pixel of the audio
    EXPECT_DOUBLE_EQ(window.toPixelSnapped, 506.0);
}

TEST(ProjectScene_WaveSceneGraphTests, TrimmedClipWindowStartsAtTheTrim)
{
    //! [GIVEN] A clip played from 2 s with 1 s trimmed at its start
    const auto data = makeClipData(2.0, 4.0, 1.0);
    const auto time = makeClipTime(2.0, 4.0, 2.0, 4.0);

    //! [WHEN] The window is computed
    const auto window = WaveSceneGraph::computeWindow(time, data, ZOOM, 200.0, false, false);

    //! [THEN] It is measured from the untrimmed sequence start, without snapping
    EXPECT_DOUBLE_EQ(window.fromPixelExact, 100.0);
    EXPECT_DOUBLE_EQ(window.fromPixelSnapped, 100.0);
    EXPECT_DOUBLE_EQ(window.toPixelExact, 300.0);
}

TEST(ProjectScene_WaveSceneGraphTests, RecordingClipGetsAPlaceholderPastItsAudio)
{
    //! [GIVEN] A clip extended to 10 s while its recorded audio ends at 6 s
    const auto data = makeClipData(0.0, 6.0);
    const auto time = makeClipTime(0.0, 10.0, 0.0, 10.0);

    //! [WHEN] The window is computed while recording, and while not recording
    const auto recording = WaveSceneGraph::computeWindow(time, data, ZOOM, 0.0, false, true);
    const auto notRecording = WaveSceneGraph::computeWindow(time, data, ZOOM, 0.0, false, false);

    //! [THEN] Only while recording, the audio is looked up to its end and the rest is a placeholder
    EXPECT_TRUE(recording.hasPlaceholder);
    EXPECT_DOUBLE_EQ(recording.placeholderFromPixel, 600.0);
    EXPECT_DOUBLE_EQ(recording.toPixelSnapped, 600.0);
    EXPECT_FALSE(notRecording.hasPlaceholder);
    EXPECT_DOUBLE_EQ(notRecording.toPixelSnapped, 1000.0);
}

TEST(ProjectScene_WaveSceneGraphTests, DragGhostFollowsItsOwnTimeAndContainer)
{
    //! [GIVEN] A drag ghost shown at 5 s for a clip that still starts at 1 s
    const auto data = makeClipData(1.0, 3.0);
    const auto time = makeClipTime(5.0, 7.0, 5.0, 7.0);

    //! [WHEN] The window is computed
    const auto window = WaveSceneGraph::computeWindow(time, data, ZOOM, 500.4, true, false);

    //! [THEN] The window is placed from the ghost's own start, and no sub-pixel residual is applied
    EXPECT_DOUBLE_EQ(window.fromPixelExact, 0.0);
    EXPECT_DOUBLE_EQ(window.toPixelExact, 200.0);
    EXPECT_DOUBLE_EQ(window.subpixelResidual, 0.0);
}

TEST(ProjectScene_WaveSceneGraphTests, SignatureChangesWithTheDataVersion)
{
    //! [GIVEN] Two otherwise identical inputs whose clip data versions differ
    auto data = makeClipData(0.0, 2.0);
    const auto time = makeClipTime(0.0, 2.0, 0.0, 2.0);
    const auto window = WaveSceneGraph::computeWindow(time, data, ZOOM, 0.0, false, false);
    IWavePainter::Params params;
    params.zoom = ZOOM;
    const WaveMetrics metrics;
    const trackedit::ClipKey key { 1, 2 };

    data.dataVersion = 1;
    const auto first = WaveSceneGraph::makeSignature(key, data, params, metrics, window, 4.0f, true);
    const auto same = WaveSceneGraph::makeSignature(key, data, params, metrics, window, 4.0f, true);
    data.dataVersion = 2;
    const auto changed = WaveSceneGraph::makeSignature(key, data, params, metrics, window, 4.0f, true);

    //! [THEN] Only the changed version makes the signature differ
    EXPECT_TRUE(first == same);
    EXPECT_FALSE(first == changed);
}

TEST(ProjectScene_WaveSceneGraphTests, SignatureChangesWithThePlotType)
{
    //! [GIVEN] Two otherwise identical inputs drawn as different plots
    const auto data = makeClipData(0.0, 2.0);
    const auto time = makeClipTime(0.0, 2.0, 0.0, 2.0);
    const auto window = WaveSceneGraph::computeWindow(time, data, ZOOM, 0.0, false, false);
    IWavePainter::Params params;
    params.zoom = ZOOM;
    const WaveMetrics metrics;
    const trackedit::ClipKey key { 1, 2 };

    //! [WHEN] Their signatures are made
    const auto minMax = WaveSceneGraph::makeSignature(key, data, params, metrics, window, 4.0f, true, IWavePainter::PlotType::MinMaxRMS);
    const auto dots = WaveSceneGraph::makeSignature(key, data, params, metrics, window, 4.0f, true, IWavePainter::PlotType::ConnectingDots);

    //! [THEN] They differ
    EXPECT_FALSE(minMax == dots);
}

TEST(ProjectScene_WaveSceneGraphTests, ConnectedDotsKeepSubpixelPositions)
{
    //! [GIVEN] Two samples between whole pixels, in a channel 2 pixels from the top
    const std::vector<WaveSamplePoint> samples { makeSample(10.25f, 5.5f), makeSample(11.75f, 6.0f) };
    WaveSceneGraph::Frame frame;

    //! [WHEN] They are drawn as connected dots
    WaveSceneGraph::appendConnectedDots(frame, samples, 2.0f, 100.0f, IWavePainter::Style {}, false);

    //! [THEN] The line is centred on them, one pixel thick
    ASSERT_FALSE(frame.sampleTriangles.empty());
    const auto [left, right] = xRange(frame.sampleTriangles);
    const auto [top, bottom] = yRange(frame.sampleTriangles);
    EXPECT_FLOAT_EQ(left, 9.75f);
    EXPECT_FLOAT_EQ(right, 12.25f);
    EXPECT_FLOAT_EQ(top, 7.0f);
    EXPECT_FLOAT_EQ(bottom, 8.5f);
    EXPECT_TRUE(frame.sampleHeadTriangles.empty());
}

TEST(ProjectScene_WaveSceneGraphTests, ClippedConnectedDotsGetAFullHeightBar)
{
    //! [GIVEN] A clipping sample
    const std::vector<WaveSamplePoint> samples { makeSample(4.5f, 20.0f, false, true) };
    WaveSceneGraph::Frame withClipping;
    WaveSceneGraph::Frame withoutClipping;

    //! [WHEN] It is drawn with and without showing clipping
    WaveSceneGraph::appendConnectedDots(withClipping, samples, 10.0f, 50.0f, IWavePainter::Style {}, true);
    WaveSceneGraph::appendConnectedDots(withoutClipping, samples, 10.0f, 50.0f, IWavePainter::Style {}, false);

    //! [THEN] Only when showing clipping, a bar covers the whole channel
    const auto [top, bottom] = yRange(withClipping.sampleTriangles);
    EXPECT_FLOAT_EQ(top, 10.0f);
    EXPECT_FLOAT_EQ(bottom, 60.0f);
    EXPECT_FLOAT_EQ(yRange(withoutClipping.sampleTriangles).first, 29.5f);
    EXPECT_FLOAT_EQ(yRange(withoutClipping.sampleTriangles).second, 30.5f);
}

TEST(ProjectScene_WaveSceneGraphTests, StemsReachTheZeroLine)
{
    //! [GIVEN] A sample 10 pixels into a channel starting at 3, above a zero line at 50
    const std::vector<WaveSamplePoint> samples { makeSample(20.25f, 10.0f) };
    WaveSceneGraph::Frame frame;

    //! [WHEN] It is drawn as a stem
    WaveSceneGraph::appendStems(frame, samples, 3.0f, 94.0f, 50.0f, IWavePainter::Style {}, false);

    //! [THEN] The stalk runs from the sample through the zero line, and the head is centred on the sample
    const auto [stalkTop, stalkBottom] = yRange(frame.sampleTriangles);
    EXPECT_FLOAT_EQ(stalkTop, 12.5f);
    EXPECT_FLOAT_EQ(stalkBottom, 50.5f);
    ASSERT_FALSE(frame.sampleHeadTriangles.empty());
    const auto [headLeft, headRight] = xRange(frame.sampleHeadTriangles);
    EXPECT_FLOAT_EQ((headLeft + headRight) / 2.0f, 20.25f);
}

TEST(ProjectScene_WaveSceneGraphTests, StemHeadsOutsideTheChannelAreSkipped)
{
    //! [GIVEN] A sample above its channel
    const std::vector<WaveSamplePoint> samples { makeSample(5.0f, -1.0f) };
    WaveSceneGraph::Frame frame;

    //! [WHEN] It is drawn as a stem
    WaveSceneGraph::appendStems(frame, samples, 3.0f, 94.0f, 50.0f, IWavePainter::Style {}, false);

    //! [THEN] Its stalk is drawn, its head isn't
    EXPECT_FALSE(frame.sampleTriangles.empty());
    EXPECT_TRUE(frame.sampleHeadTriangles.empty());
}

TEST(ProjectScene_WaveSceneGraphTests, DisplayBoundsAreDBScaledInADBView)
{
    //! [GIVEN] A view zoomed to [-0.1, 0.1] with a 60 dB range
    IWavePainter::Params params;
    params.displayBounds = { -0.1f, 0.1f };
    params.dbRange = -60.0;

    //! [WHEN] Its display bounds are taken in a linear and in a dB view
    params.isLinear = true;
    const auto linear = waveDisplayBounds(params);
    params.isLinear = false;
    const auto dB = waveDisplayBounds(params);

    //! [THEN] Only the dB view maps them like the samples, 20 dB above the floor
    EXPECT_FLOAT_EQ(linear.first, -0.1f);
    EXPECT_FLOAT_EQ(linear.second, 0.1f);
    EXPECT_NEAR(dB.first, -40.0f / 60.0f, 1e-6f);
    EXPECT_NEAR(dB.second, 40.0f / 60.0f, 1e-6f);
}

TEST(ProjectScene_WaveSceneGraphTests, ColumnsStayInsideTheirChannel)
{
    //! [GIVEN] A column of a vertically zoomed view reaching past both ends of a channel from 100 to 200
    const WaveColumnVertex column { 5.0f, -40.0f, 140.0f, -10.0f, 110.0f, false, false };

    //! [WHEN] It is turned into a vertex
    const auto v = WaveSceneGraph::toVertex(column, 100.0f, 200.0f, {});

    //! [THEN] Its band and RMS band are clamped to the channel
    EXPECT_FLOAT_EQ(v.maxY, 100.0f);
    EXPECT_FLOAT_EQ(v.minY, 200.0f);
    EXPECT_FLOAT_EQ(v.rmsMaxY, 100.0f);
    EXPECT_FLOAT_EQ(v.rmsMinY, 200.0f);
}
}
