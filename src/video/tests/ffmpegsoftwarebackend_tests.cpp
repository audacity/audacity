/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <cmath>
#include <string>

#include <QFileInfo>
#include <QImage>

#include "internal/ffmpeg/ffmpegsoftwarebackend.h"

using namespace au::video;

namespace {
//! Where make_fixtures.py writes its output.
std::string fixturesDir()
{
    return std::string(au_video_tests_DATA_ROOT) + "/fixtures";
}

std::string fixture(const std::string& name)
{
    return fixturesDir() + "/" + name;
}

bool fixtureExists(const std::string& name)
{
    return QFileInfo::exists(QString::fromStdString(fixture(name)));
}

//! The fixtures are generated rather than committed, and no CI job installs
//! FFmpeg, so these tests report why they did not run instead of failing.
#define REQUIRE_FIXTURE(name)                                                  \
    do {                                                                       \
        if (!fixtureExists(name)) {                                            \
            GTEST_SKIP() << "Missing " << fixture(name)                        \
                         << ". Generate it with tools/videosync/make_fixtures.py"; \
        }                                                                      \
    } while (0)

#define REQUIRE_OPENED(backend, name)                                          \
    do {                                                                       \
        const VideoError err = backend.open(fixture(name));                    \
        if (err == VideoError::FFmpegNotFound || err == VideoError::FFmpegTooOld) { \
            GTEST_SKIP() << "No usable FFmpeg on this machine: "               \
                         << errorMessage(err);                                 \
        }                                                                      \
        ASSERT_EQ(err, VideoError::None) << errorMessage(err);                 \
    } while (0)

constexpr double FPS = 25.0;
constexpr int MARKER_EVERY_FRAMES = 25;

//! The fixture lights a 360x360 patch at (460, 40) for exactly one frame per
//! second. Sampling it turns "did the seek land where it claimed" into a
//! measurement rather than an impression.
int flashPatchLuma(const QImage& image)
{
    if (image.isNull()) {
        return -1;
    }

    // Only meaningful when the frame was decoded at its native size.
    const int x0 = 460, y0 = 40, side = 360;
    long sum = 0;
    int count = 0;
    for (int y = y0; y < y0 + side && y < image.height(); ++y) {
        const uint8_t* line = image.constScanLine(y);
        for (int x = x0; x < x0 + side && x < image.width(); ++x) {
            sum += line[x * 3];    // red channel; the patch is neutral
            ++count;
        }
    }
    return count > 0 ? static_cast<int>(sum / count) : -1;
}

bool isMarkerTime(double seconds)
{
    const int frame = static_cast<int>(std::floor(seconds * FPS + 1e-6));
    return frame % MARKER_EVERY_FRAMES == 0;
}
}

// ---------------------------------------------------------------------------
// Opening
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, StartsClosed)
{
    FFmpegSoftwareBackend backend;
    EXPECT_FALSE(backend.isOpen());
    EXPECT_FALSE(backend.streamInfo().isValid());
}

TEST(FFmpegSoftwareBackendTests, ReportsAMissingFileRatherThanCrashing)
{
    FFmpegSoftwareBackend backend;
    const VideoError err = backend.open(fixture("no-such-file-anywhere.mkv"));
    if (err == VideoError::FFmpegNotFound) {
        GTEST_SKIP() << "No usable FFmpeg on this machine";
    }
    EXPECT_EQ(err, VideoError::CannotOpen);
    EXPECT_FALSE(backend.isOpen());
}

TEST(FFmpegSoftwareBackendTests, RejectsAFileWithNoVideoStream)
{
    REQUIRE_FIXTURE("reference25.wav");

    FFmpegSoftwareBackend backend;
    const VideoError err = backend.open(fixture("reference25.wav"));
    if (err == VideoError::FFmpegNotFound) {
        GTEST_SKIP() << "No usable FFmpeg on this machine";
    }
    EXPECT_EQ(err, VideoError::NoVideoStream);
    EXPECT_FALSE(backend.isOpen());
}

TEST(FFmpegSoftwareBackendTests, ProbesTheStream)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    EXPECT_TRUE(backend.isOpen());

    const VideoStreamInfo& info = backend.streamInfo();
    EXPECT_GE(info.streamIndex, 0);
    EXPECT_NEAR(info.frameRate, FPS, 0.01);
    EXPECT_GT(info.duration.to_double(), 0.0)
        << "duration must fall back to the container when the stream has none";
}

TEST(FFmpegSoftwareBackendTests, CloseResetsEverything)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");
    ASSERT_TRUE(backend.isOpen());

    backend.close();
    EXPECT_FALSE(backend.isOpen());
    EXPECT_FALSE(backend.streamInfo().isValid());
    EXPECT_FALSE(backend.frameAt(1.0, 320, 180).valid());
}

// ---------------------------------------------------------------------------
// Frame selection. These are the assertions that would catch the picture
// drifting away from the playhead.
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, ReturnsTheFrameCoveringTheRequestedTime)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    const double frameDuration = 1.0 / FPS;

    for (double target : { 0.0, 0.5, 1.02, 2.5, 4.4, 7.98, 9.5 }) {
        const VideoFrame frame = backend.frameAt(target, 1280, 720);
        ASSERT_TRUE(frame.valid()) << "no frame at t=" << target;

        // Half-open interval: the frame starts at or before the target and
        // the next one starts after it.
        EXPECT_LE(frame.time.to_double(), target + 1e-6)
            << "frame starts after the time it should cover, t=" << target;
        EXPECT_GT(frame.time.to_double() + frameDuration, target - 1e-6)
            << "frame ends before the time it should cover, t=" << target;
    }
}

TEST(FFmpegSoftwareBackendTests, LandsOnMarkerFramesAndOnlyOnThem)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    // Aim at the middle of a frame so rounding cannot put us on its neighbour.
    const double halfFrame = 0.5 / FPS;

    for (double second : { 0.0, 1.0, 2.0, 3.0, 4.0 }) {
        const double onMarker = second + halfFrame;
        const VideoFrame lit = backend.frameAt(onMarker, 1280, 720);
        ASSERT_TRUE(lit.valid()) << "no frame at t=" << onMarker;
        ASSERT_TRUE(isMarkerTime(onMarker));
        EXPECT_GT(flashPatchLuma(lit.image), 200)
            << "flash patch should be lit at t=" << onMarker;

        const double offMarker = second + 0.5 + halfFrame;
        const VideoFrame dark = backend.frameAt(offMarker, 1280, 720);
        ASSERT_TRUE(dark.valid()) << "no frame at t=" << offMarker;
        ASSERT_FALSE(isMarkerTime(offMarker));
        EXPECT_LT(flashPatchLuma(dark.image), 100)
            << "flash patch should be dark at t=" << offMarker;
    }
}

TEST(FFmpegSoftwareBackendTests, SeeksBackwardsCorrectly)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    const double halfFrame = 0.5 / FPS;

    // Jumping backwards forces a keyframe seek and a decoder flush, which is
    // the path that leaves stale or corrupt frames on screen when it is wrong.
    const VideoFrame late = backend.frameAt(8.0 + halfFrame, 1280, 720);
    ASSERT_TRUE(late.valid());
    EXPECT_GT(flashPatchLuma(late.image), 200);

    const VideoFrame early = backend.frameAt(1.0 + halfFrame, 1280, 720);
    ASSERT_TRUE(early.valid());
    EXPECT_GT(flashPatchLuma(early.image), 200);
    EXPECT_NEAR(early.time.to_double(), 1.0, 1.0 / FPS);

    const VideoFrame between = backend.frameAt(3.5 + halfFrame, 1280, 720);
    ASSERT_TRUE(between.valid());
    EXPECT_LT(flashPatchLuma(between.image), 100);
}

TEST(FFmpegSoftwareBackendTests, RepeatedRequestsForTheSameTimeAgree)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    const VideoFrame first = backend.frameAt(6.02, 1280, 720);
    ASSERT_TRUE(first.valid());

    backend.frameAt(1.0, 1280, 720);     // wander off
    const VideoFrame again = backend.frameAt(6.02, 1280, 720);
    ASSERT_TRUE(again.valid());

    EXPECT_EQ(first.pts, again.pts);
    EXPECT_EQ(flashPatchLuma(first.image), flashPatchLuma(again.image));
}

// ---------------------------------------------------------------------------
// Output sizing
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, FitsInsideTheRequestedBoxKeepingAspect)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    // A tall, narrow box against 16:9 source: the result has to be letterboxed
    // by the caller, not stretched to fill.
    const VideoFrame frame = backend.frameAt(2.0, 200, 900);
    ASSERT_TRUE(frame.valid());

    EXPECT_LE(frame.image.width(), 200);
    EXPECT_LE(frame.image.height(), 900);

    const double sourceAspect = 1280.0 / 720.0;
    const double resultAspect = static_cast<double>(frame.image.width())
                                / frame.image.height();
    EXPECT_NEAR(resultAspect, sourceAspect, 0.02);
}

TEST(FFmpegSoftwareBackendTests, RejectsNonPositiveTargetSizes)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    EXPECT_FALSE(backend.frameAt(1.0, 0, 100).valid());
    EXPECT_FALSE(backend.frameAt(1.0, 100, 0).valid());
    EXPECT_FALSE(backend.frameAt(1.0, -10, 100).valid());
}

// ---------------------------------------------------------------------------
// The anchor. Within one container the audio and video streams do not start at
// the same timestamp: the audio begins earlier by the encoder priming, which
// libavformat then strips. The picture is anchored on the video start, so a
// container offset must not shift which frame a given project time selects.
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, RecordsThatTheTwoStreamsStartApart)
{
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.ts");

    const VideoStreamInfo& info = backend.streamInfo();

    // The generator muxes MPEG-TS with a container offset, and the two streams
    // sit ~21 ms apart because of the AAC priming.
    EXPECT_GT(info.audioStartTime.to_double(), 0.0);
    EXPECT_GT(info.videoStartTime.to_double(), 0.0);
    EXPECT_NE(info.audioStartTime.to_double(), info.videoStartTime.to_double())
        << "this fixture is only interesting because the streams differ";
}

TEST(FFmpegSoftwareBackendTests, ContainerOffsetDoesNotShiftTheContent)
{
    REQUIRE_FIXTURE("sync25.mkv");
    REQUIRE_FIXTURE("sync25-offset.mp4");

    FFmpegSoftwareBackend plain;
    REQUIRE_OPENED(plain, "sync25.mkv");

    FFmpegSoftwareBackend offset;
    REQUIRE_OPENED(offset, "sync25-offset.mp4");

    // sync25-offset.mp4 carries a 1.5 s container offset. Anchoring correctly
    // means the same content time still selects the same picture; anchoring
    // wrongly shifts it by tens of frames.
    EXPECT_GT(offset.streamInfo().audioStartTime.to_double(), 1.0);

    const double halfFrame = 0.5 / FPS;
    for (double second : { 1.0, 3.0, 5.0 }) {
        const double t = second + halfFrame;

        const VideoFrame a = plain.frameAt(t, 1280, 720);
        const VideoFrame b = offset.frameAt(t, 1280, 720);
        ASSERT_TRUE(a.valid()) << "plain fixture, t=" << t;
        ASSERT_TRUE(b.valid()) << "offset fixture, t=" << t;

        EXPECT_GT(flashPatchLuma(a.image), 200) << "plain, t=" << t;
        EXPECT_GT(flashPatchLuma(b.image), 200)
            << "offset fixture landed on the wrong frame at t=" << t;
    }
}

// ---------------------------------------------------------------------------
// Beyond the end
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, KnowsItsDurationSoTheRangeCheckWorks)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    // Matroska stores no per-stream duration; without the container fallback
    // this reads zero and seeking past the end silently holds the last frame.
    EXPECT_GT(backend.streamInfo().duration.to_double(), 5.0);
    EXPECT_LT(backend.streamInfo().duration.to_double(), 60.0);
}
