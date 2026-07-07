/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

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
}
