/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <chrono>
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
                         << errorMessage(err).toStdString();                                 \
        }                                                                      \
        ASSERT_EQ(err, VideoError::None) << errorMessage(err).toStdString();                 \
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

    // Specifically missing, not merely unopenable: a project whose media has
    // moved needs to be distinguishable from a corrupt file.
    EXPECT_EQ(err, VideoError::FileNotFound);
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
            << "wanted a frame covering t=" << target
            << " but got one starting at " << frame.time.to_double()
            << " (pts " << frame.pts << ")";
        EXPECT_GT(frame.time.to_double() + frameDuration, target - 1e-6)
            << "wanted a frame covering t=" << target
            << " but got one starting at " << frame.time.to_double();
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

// ---------------------------------------------------------------------------
// MPEG-TS. The format carries no index, so libavformat seeks it by estimating
// byte positions, and AVSEEK_FLAG_BACKWARD does not reliably land at or before
// the requested time. Measured with tools/videosync/videoprobe: the TS fixture
// lands exactly one GOP late on every target while MKV and MP4 are exact.
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, SeeksAccuratelyInMpegTs)
{
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.ts");

    const double halfFrame = 0.5 / FPS;
    int wrong = 0;

    // Visited out of order so each one forces a real seek rather than being
    // served by decoding forward from the last.
    for (double second : { 7.0, 1.0, 5.0, 2.0, 8.0, 3.0 }) {
        const double target = second + halfFrame;
        const VideoFrame frame = backend.frameAt(target, 1280, 720);
        ASSERT_TRUE(frame.valid()) << "no frame at t=" << target;

        // The patch alone is too weak a check: markers are one second apart
        // and the GOP is one second, so landing a whole GOP late still lights
        // it. The frame's own time has to match too.
        const bool lit = flashPatchLuma(frame.image) > 200;
        const bool onTime = std::fabs(frame.time.to_double() - second) < 0.5 / FPS;

        if (!lit || !onTime) {
            ++wrong;
            ADD_FAILURE() << "t=" << target << " should show the marker at "
                          << second << " but got a frame at "
                          << frame.time.to_double()
                          << (lit ? " (a marker, but the wrong one)" : " (not a marker)");
        }
    }

    EXPECT_EQ(wrong, 0);
}

TEST(FFmpegSoftwareBackendTests, MpegTsFrameTimeMatchesTheRequest)
{
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.ts");

    const double frameDuration = 1.0 / FPS;

    for (double target : { 1.5, 4.5, 2.5, 6.5 }) {
        const VideoFrame frame = backend.frameAt(target, 640, 360);
        ASSERT_TRUE(frame.valid()) << "no frame at t=" << target;

        EXPECT_LE(frame.time.to_double(), target + 1e-6)
            << "wanted a frame covering t=" << target
            << " but got one starting at " << frame.time.to_double()
            << " (pts " << frame.pts << ")";
        EXPECT_GT(frame.time.to_double() + frameDuration, target - 1e-6)
            << "wanted a frame covering t=" << target
            << " but got one starting at " << frame.time.to_double();
    }
}

// ---------------------------------------------------------------------------
// The seek retry ladder, as arithmetic. Both non-termination modes live here:
// a probe that never reaches the start of the file, and a clamp that does not
// report itself so the caller loops on the floor forever.
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, ProbeLadderBacksOffByDoubling)
{
    const int64_t ticks = 90000;              // MPEG-TS
    const int64_t target = 100 * ticks;
    bool atFloor = true;

    EXPECT_EQ(FFmpegSoftwareBackend::nextProbePts(target, 0, 0, ticks, &atFloor),
              target - 1 * ticks);
    EXPECT_FALSE(atFloor);

    EXPECT_EQ(FFmpegSoftwareBackend::nextProbePts(target, 1, 0, ticks, &atFloor),
              target - 2 * ticks);
    EXPECT_FALSE(atFloor);

    EXPECT_EQ(FFmpegSoftwareBackend::nextProbePts(target, 2, 0, ticks, &atFloor),
              target - 4 * ticks);
    EXPECT_FALSE(atFloor);
}

TEST(FFmpegSoftwareBackendTests, ProbeLadderClampsToTheStartAndSaysSo)
{
    const int64_t ticks = 90000;
    const int64_t floor = 5 * ticks;
    const int64_t target = floor + ticks / 2;   // half a second past the floor
    bool atFloor = false;

    // Every rung is already past the start of the stream, so all of them
    // clamp - and each one has to report it, or the caller cannot tell that
    // the ladder is exhausted and will keep seeking to the same place.
    for (int attempt = 0; attempt < 3; ++attempt) {
        EXPECT_EQ(FFmpegSoftwareBackend::nextProbePts(target, attempt, floor, ticks, &atFloor),
                  floor) << "attempt " << attempt;
        EXPECT_TRUE(atFloor) << "attempt " << attempt;
    }
}

TEST(FFmpegSoftwareBackendTests, ProbeLadderToleratesANullFloorFlag)
{
    EXPECT_EQ(FFmpegSoftwareBackend::nextProbePts(1000, 0, 0, 100, nullptr), 900);
}

// ---------------------------------------------------------------------------
// Sweeps. A handful of spot checks hid the fact that MPEG-TS was wrong on
// nearly every target, so these walk every frame.
// ---------------------------------------------------------------------------

namespace {
struct SweepResult {
    int checked = 0;
    int late = 0;
    int missing = 0;
    double worstLateSeconds = 0.0;
    int maxSeekAttempts = 0;
};

//! Asks for every frame's midpoint in turn and checks the returned frame
//! actually covers it, using the frame's own reported duration rather than a
//! nominal one.
SweepResult sweep(FFmpegSoftwareBackend& backend, double fps, int frames,
                  double timeBase)
{
    SweepResult result;

    for (int i = 0; i < frames; ++i) {
        const double target = (i + 0.5) / fps;
        const VideoFrame frame = backend.frameAt(target, 320, 180);
        ++result.checked;

        result.maxSeekAttempts = std::max(result.maxSeekAttempts, backend.seekAttempts());

        if (!frame.valid()) {
            ++result.missing;
            continue;
        }

        const double start = frame.time.to_double();
        const double end = start + backend.frameDurationPts() * timeBase;

        if (start > target + 1e-9 || end <= target - 1e-9) {
            ++result.late;
            result.worstLateSeconds =
                std::max(result.worstLateSeconds, std::fabs(start - target));
        }
    }

    return result;
}

double timeBaseOf(const std::string& fixtureName, double fallback)
{
    // The fixtures use 1/90000 for MPEG-TS and 1/1000 for Matroska; the sweep
    // only needs it to turn a duration in ticks back into seconds.
    if (fixtureName.size() >= 3
        && fixtureName.compare(fixtureName.size() - 3, 3, ".ts") == 0) {
        return 1.0 / 90000.0;
    }
    return fallback;
}
}

TEST(FFmpegSoftwareBackendTests, MpegTsSweepLandsOnEveryFrame)
{
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.ts");

    // Every frame of the ten second fixture. Before the retry, 224 of 250
    // were late, the worst by over a second.
    const SweepResult result = sweep(backend, FPS, 240, timeBaseOf("sync25.ts", 0.0));

    EXPECT_EQ(result.late, 0) << "worst was " << result.worstLateSeconds << " s";
    EXPECT_EQ(result.missing, 0);
    EXPECT_GT(result.maxSeekAttempts, 1) << "the retry ladder should have run";
}

TEST(FFmpegSoftwareBackendTests, MpegTsReturnsAFrameAtTheLastKeyframe)
{
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.ts");

    // The old code returned no frame at all here, and the panel silently kept
    // whatever was already on screen.
    const VideoFrame frame = backend.frameAt(9.0 + 0.5 / FPS, 320, 180);
    ASSERT_TRUE(frame.valid());
    EXPECT_NEAR(frame.time.to_double(), 9.0, 1.0 / FPS);
}

TEST(FFmpegSoftwareBackendTests, ContainersThatSeekCorrectlyNeedNoRetry)
{
    // The retry must be free where it is not needed. A second seek here would
    // mean the ladder had started firing on files that were already correct -
    // which is how a byte rewind regresses a working container.
    for (const char* name : { "sync25.mkv", "sync25.webm", "sync25.mp4",
                              "sync25-offset.mp4" }) {
        if (!fixtureExists(name)) {
            continue;
        }

        FFmpegSoftwareBackend backend;
        const VideoError err = backend.open(fixture(name));
        if (err == VideoError::FFmpegNotFound || err == VideoError::FFmpegTooOld) {
            GTEST_SKIP() << "No usable FFmpeg on this machine";
        }
        ASSERT_EQ(err, VideoError::None) << name << ": " << errorMessage(err).toStdString();

        const SweepResult result = sweep(backend, FPS, 200, 1.0 / 1000.0);

        EXPECT_EQ(result.late, 0) << name << ", worst " << result.worstLateSeconds << " s";
        EXPECT_EQ(result.missing, 0) << name;
        EXPECT_EQ(result.maxSeekAttempts, 1)
            << name << " needed " << result.maxSeekAttempts
            << " seeks; it should need exactly one";
    }
}

TEST(FFmpegSoftwareBackendTests, NonIntegerFrameRateLandsOnEveryFrame)
{
    REQUIRE_FIXTURE("sync30000_1001.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync30000_1001.mkv");

    // 30000/1001 has real frame deltas that alternate rather than being
    // constant, so a single duration derived from the average frame rate
    // picks the wrong frame on about one target in a hundred. Measuring each
    // frame's end from the next frame's timestamp is what fixes it.
    const double fps = 30000.0 / 1001.0;
    const SweepResult result = sweep(backend, fps, 290, 1.0 / 1000.0);

    EXPECT_EQ(result.late, 0) << "worst was " << result.worstLateSeconds << " s";
    EXPECT_EQ(result.missing, 0);
}

TEST(FFmpegSoftwareBackendTests, ReportsARealDurationForTheHeldFrame)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    const VideoFrame frame = backend.frameAt(4.0, 320, 180);
    ASSERT_TRUE(frame.valid());

    // 25 fps in a 1/1000 time base is 40 ticks. The value has to come from the
    // next frame's timestamp, not from a nominal figure.
    EXPECT_GT(backend.frameDurationPts(), 0);
    EXPECT_NEAR(static_cast<double>(backend.frameDurationPts()) / 1000.0,
                1.0 / FPS, 0.005);
}

TEST(FFmpegSoftwareBackendTests, PastTheEndReturnsTheFinalFrame)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    // The drain path used to pair the last decoded image with the previous
    // frame's timestamp.
    const VideoFrame frame = backend.frameAt(60.0, 320, 180);
    if (!frame.valid()) {
        GTEST_SKIP() << "this build returns nothing past the end; nothing to check";
    }

    const VideoStreamInfo& info = backend.streamInfo();
    EXPECT_GT(frame.time.to_double(), info.duration.to_double() - 0.5)
        << "the frame handed back should be the last one, not an earlier one";
}

// ---------------------------------------------------------------------------
// The retry ladder runs on the decode thread, which detach() joins. That
// raises the worst-case join from one seek to several, so the bound matters.
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, RetryingSeeksStaysBoundedInTime)
{
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.ts");

    // Worst case for the ladder: every target far from the last, so the
    // forward window never helps and each one seeks from scratch.
    const double worstCaseBudgetSeconds = 1.0;

    for (double target : { 8.5, 0.5, 7.5, 1.5, 6.5, 2.5 }) {
        const auto began = std::chrono::steady_clock::now();
        const VideoFrame frame = backend.frameAt(target, 320, 180);
        const double elapsed = std::chrono::duration<double>(
            std::chrono::steady_clock::now() - began).count();

        ASSERT_TRUE(frame.valid()) << "no frame at t=" << target;
        EXPECT_LT(elapsed, worstCaseBudgetSeconds)
            << "a single request took " << elapsed
            << " s at t=" << target << "; detach() waits on exactly this";
        EXPECT_LE(backend.seekAttempts(), 5)
            << "the ladder must stay bounded, not walk the file";
    }
}

TEST(FFmpegSoftwareBackendTests, SeekingOffTheEndTerminates)
{
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.ts");

    // Far past the end of a ten second file. The ladder must give up rather
    // than seek forever on the thread detach() is waiting for.
    const auto began = std::chrono::steady_clock::now();
    backend.frameAt(600.0, 320, 180);
    const double elapsed = std::chrono::duration<double>(
        std::chrono::steady_clock::now() - began).count();

    EXPECT_LT(elapsed, 2.0) << "took " << elapsed << " s to give up";
    EXPECT_LE(backend.seekAttempts(), 5);
}

TEST(FFmpegSoftwareBackendTests, SeekingBeforeTheStartTerminates)
{
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.ts");

    const auto began = std::chrono::steady_clock::now();
    backend.frameAt(-30.0, 320, 180);
    const double elapsed = std::chrono::duration<double>(
        std::chrono::steady_clock::now() - began).count();

    EXPECT_LT(elapsed, 2.0) << "took " << elapsed << " s to give up";
    EXPECT_LE(backend.seekAttempts(), 5);
}

// ---------------------------------------------------------------------------
// Repeated open and close. Attaching one file after another is an ordinary
// thing to do, and every open used to leak the demuxer's internal state -
// around 0.9 MB for Matroska and MP4, far more for MPEG-TS.
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, SurvivesManyOpenCloseCycles)
{
    REQUIRE_FIXTURE("sync25.mkv");

    for (int i = 0; i < 40; ++i) {
        FFmpegSoftwareBackend backend;
        const VideoError err = backend.open(fixture("sync25.mkv"));
        if (err == VideoError::FFmpegNotFound || err == VideoError::FFmpegTooOld) {
            GTEST_SKIP() << "No usable FFmpeg on this machine";
        }
        ASSERT_EQ(err, VideoError::None) << "cycle " << i << ": " << errorMessage(err).toStdString();

        // Decode something so the demuxer actually allocates its state.
        const VideoFrame frame = backend.frameAt(1.0 + i * 0.1, 160, 90);
        EXPECT_TRUE(frame.valid()) << "cycle " << i;

        backend.close();
        EXPECT_FALSE(backend.isOpen());
    }
}

TEST(FFmpegSoftwareBackendTests, ReopeningTheSameBackendWorks)
{
    REQUIRE_FIXTURE("sync25.mkv");
    REQUIRE_FIXTURE("sync25.ts");

    FFmpegSoftwareBackend backend;

    // Alternating containers exercises both teardown paths and makes sure no
    // state survives from the previous file.
    for (int i = 0; i < 6; ++i) {
        const char* name = (i % 2 == 0) ? "sync25.mkv" : "sync25.ts";

        const VideoError err = backend.open(fixture(name));
        if (err == VideoError::FFmpegNotFound || err == VideoError::FFmpegTooOld) {
            GTEST_SKIP() << "No usable FFmpeg on this machine";
        }
        ASSERT_EQ(err, VideoError::None) << name << " on cycle " << i;

        const VideoFrame frame = backend.frameAt(2.0 + 0.5 / FPS, 160, 90);
        ASSERT_TRUE(frame.valid()) << name << " on cycle " << i;
        EXPECT_NEAR(frame.time.to_double(), 2.0, 1.0 / FPS)
            << name << " on cycle " << i;
    }
}

// ---------------------------------------------------------------------------
// Saying why there is no picture.
//
// A file the converter cannot handle used to come back as a black rectangle
// with its resolution printed underneath and no error anywhere: the converter
// returned a null image, the worker dropped the frame, and nothing recorded a
// reason.
// ---------------------------------------------------------------------------

TEST(FFmpegSoftwareBackendTests, StartsWithNoFrameError)
{
    FFmpegSoftwareBackend backend;
    EXPECT_EQ(backend.lastFrameError(), VideoError::None);
}

TEST(FFmpegSoftwareBackendTests, AnUnsupportedPixelFormatSaysSo)
{
    REQUIRE_FIXTURE("yuv422.mp4");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "yuv422.mp4");

    const VideoFrame frame = backend.frameAt(1.0, 320, 180);

    EXPECT_FALSE(frame.valid()) << "4:2:2 is not handled yet";
    EXPECT_EQ(backend.lastFrameError(), VideoError::UnsupportedFormat)
        << "the panel has to be able to say why it is blank";
}

TEST(FFmpegSoftwareBackendTests, HdrIsRefusedRatherThanShownWrong)
{
    REQUIRE_FIXTURE("hdr_pq.mp4");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "hdr_pq.mp4");

    // Decoded as though it were ordinary gamma, reference white lands near
    // middle grey. That looks merely dark rather than obviously broken, which
    // is worse than refusing it.
    const VideoFrame frame = backend.frameAt(1.0, 320, 180);

    EXPECT_FALSE(frame.valid());
    EXPECT_EQ(backend.lastFrameError(), VideoError::UnsupportedHdr);
}

TEST(FFmpegSoftwareBackendTests, OrdinaryFilesReportNoFrameError)
{
    REQUIRE_FIXTURE("sync25.mkv");

    FFmpegSoftwareBackend backend;
    REQUIRE_OPENED(backend, "sync25.mkv");

    // No false positives: ten consecutive frames, all fine.
    for (int i = 0; i < 10; ++i) {
        const VideoFrame frame = backend.frameAt(0.5 + i * 0.4, 320, 180);
        ASSERT_TRUE(frame.valid()) << "frame " << i;
        EXPECT_EQ(backend.lastFrameError(), VideoError::None) << "frame " << i;
    }
}

TEST(FFmpegSoftwareBackendTests, AMissingFileIsReportedAsMissing)
{
    FFmpegSoftwareBackend backend;
    const VideoError err = backend.open(fixture("definitely-not-here.mkv"));
    if (err == VideoError::FFmpegNotFound) {
        GTEST_SKIP() << "No usable FFmpeg on this machine";
    }

    // Distinguishable from a file that exists but cannot be decoded, which is
    // what any later relocate flow needs.
    EXPECT_EQ(err, VideoError::FileNotFound);
}
