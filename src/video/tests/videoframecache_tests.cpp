/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <atomic>
#include <thread>
#include <vector>

#include "internal/videoframecache.h"

using namespace au::video;

namespace {
//! A frame whose pixels are a known flat colour, so a lookup can be checked
//! for identity without comparing whole images.
VideoFrame makeFrame(int64_t pts, int width = 16, int height = 16, int tint = 0)
{
    VideoFrame frame;
    frame.image = QImage(width, height, QImage::Format_RGB888);
    frame.image.fill(QColor(tint & 0xff, (tint >> 8) & 0xff, 0));
    frame.pts = pts;
    frame.time = muse::secs_t(static_cast<double>(pts) / 1000.0);
    return frame;
}

int tintOf(const VideoFrame& frame)
{
    if (frame.image.isNull()) {
        return -1;
    }
    const uint8_t* line = frame.image.constScanLine(0);
    return line[0] | (line[1] << 8);
}
}

TEST(VideoFrameCacheTests, StartsEmpty)
{
    VideoFrameCache cache;
    EXPECT_EQ(cache.count(), 0u);
    EXPECT_EQ(cache.sizeBytes(), 0u);
    EXPECT_FALSE(cache.frameFor(0).valid());
    EXPECT_FALSE(cache.contains(0));
}

TEST(VideoFrameCacheTests, IgnoresInvalidFrames)
{
    VideoFrameCache cache;
    cache.put(VideoFrame(), 40);
    EXPECT_EQ(cache.count(), 0u);
}

TEST(VideoFrameCacheTests, ReturnsAFrameCoveringTheRequest)
{
    VideoFrameCache cache;
    cache.put(makeFrame(1000), 40);

    // Half-open interval [1000, 1040).
    for (int64_t pts : { 1000, 1001, 1039 }) {
        const auto lookup = cache.frameFor(pts);
        ASSERT_TRUE(lookup.valid()) << "pts " << pts;
        EXPECT_TRUE(lookup.covers) << "pts " << pts;
        EXPECT_EQ(lookup.frame.pts, 1000);
    }
}

TEST(VideoFrameCacheTests, TreatsTheEndOfTheIntervalAsExclusive)
{
    VideoFrameCache cache;
    cache.put(makeFrame(1000), 40);

    const auto lookup = cache.frameFor(1040);
    ASSERT_TRUE(lookup.valid());
    EXPECT_FALSE(lookup.covers) << "1040 belongs to the next frame, not this one";
    EXPECT_EQ(lookup.frame.pts, 1000) << "but it is still the best thing to show";
}

TEST(VideoFrameCacheTests, HoldsTheEarlierFrameRatherThanBlanking)
{
    VideoFrameCache cache;
    cache.put(makeFrame(0, 16, 16, 1), 40);
    cache.put(makeFrame(1000, 16, 16, 2), 40);

    // Nothing covers 500. Blanking here would flash the panel black for one
    // repaint every time the decoder fell a frame behind.
    const auto lookup = cache.frameFor(500);
    ASSERT_TRUE(lookup.valid());
    EXPECT_FALSE(lookup.covers);
    EXPECT_EQ(lookup.frame.pts, 0);
    EXPECT_EQ(tintOf(lookup.frame), 1);
}

TEST(VideoFrameCacheTests, HasNothingToShowBeforeTheFirstFrame)
{
    VideoFrameCache cache;
    cache.put(makeFrame(1000), 40);

    // There is no earlier frame to hold over, so this genuinely has nothing.
    EXPECT_FALSE(cache.frameFor(0).valid());
    EXPECT_FALSE(cache.contains(0));
}

TEST(VideoFrameCacheTests, PicksTheLatestCoveringFrame)
{
    VideoFrameCache cache;
    for (int i = 0; i < 10; ++i) {
        cache.put(makeFrame(i * 40, 16, 16, i), 40);
    }

    const auto lookup = cache.frameFor(4 * 40 + 5);
    ASSERT_TRUE(lookup.valid());
    EXPECT_TRUE(lookup.covers);
    EXPECT_EQ(lookup.frame.pts, 160);
    EXPECT_EQ(tintOf(lookup.frame), 4);
}

TEST(VideoFrameCacheTests, ReplacingAFrameKeepsTheAccountingStraight)
{
    VideoFrameCache cache;
    cache.put(makeFrame(1000, 16, 16, 1), 40);
    const size_t oneFrame = cache.sizeBytes();
    ASSERT_GT(oneFrame, 0u);

    cache.put(makeFrame(1000, 16, 16, 2), 40);
    EXPECT_EQ(cache.count(), 1u);
    EXPECT_EQ(cache.sizeBytes(), oneFrame) << "replacing must not double-count";
    EXPECT_EQ(tintOf(cache.frameFor(1000).frame), 2);
}

TEST(VideoFrameCacheTests, ContainsMatchesCoverage)
{
    VideoFrameCache cache;
    cache.put(makeFrame(1000), 40);

    EXPECT_TRUE(cache.contains(1000));
    EXPECT_TRUE(cache.contains(1039));
    EXPECT_FALSE(cache.contains(1040));
    EXPECT_FALSE(cache.contains(999));
}

TEST(VideoFrameCacheTests, ClearEmptiesEverything)
{
    VideoFrameCache cache;
    for (int i = 0; i < 5; ++i) {
        cache.put(makeFrame(i * 40), 40);
    }
    ASSERT_GT(cache.count(), 0u);

    cache.clear();
    EXPECT_EQ(cache.count(), 0u);
    EXPECT_EQ(cache.sizeBytes(), 0u);
    EXPECT_FALSE(cache.frameFor(0).valid());
}

// ---------------------------------------------------------------------------
// Eviction
// ---------------------------------------------------------------------------

TEST(VideoFrameCacheTests, StaysWithinItsByteBudget)
{
    const VideoFrame sample = makeFrame(0);
    const size_t perFrame = static_cast<size_t>(sample.image.sizeInBytes());

    VideoFrameCache cache(perFrame * 4);
    for (int i = 0; i < 50; ++i) {
        cache.frameFor(i * 40);
        cache.put(makeFrame(i * 40), 40);
    }

    EXPECT_LE(cache.sizeBytes(), perFrame * 4);
    EXPECT_LE(cache.count(), 4u);
    EXPECT_GT(cache.count(), 0u);
}

TEST(VideoFrameCacheTests, KeepsWhatIsNearThePlayhead)
{
    const VideoFrame sample = makeFrame(0);
    const size_t perFrame = static_cast<size_t>(sample.image.sizeInBytes());

    VideoFrameCache cache(perFrame * 5);

    // Fill well past the budget while walking forward, the way playback does.
    for (int i = 0; i < 40; ++i) {
        const int64_t pts = i * 40;
        cache.frameFor(pts);
        cache.put(makeFrame(pts), 40);
    }

    // The frames around the last request are the ones about to be needed
    // again; a plain least-recently-used policy would have shed exactly these.
    EXPECT_TRUE(cache.contains(39 * 40));
    EXPECT_FALSE(cache.contains(0)) << "the distant past should have gone";
}

TEST(VideoFrameCacheTests, ScrubbingBackwardsShedsTheFuture)
{
    const VideoFrame sample = makeFrame(0);
    const size_t perFrame = static_cast<size_t>(sample.image.sizeInBytes());

    VideoFrameCache cache(perFrame * 60);
    for (int i = 0; i < 50; ++i) {
        cache.put(makeFrame(i * 40), 40);
    }
    ASSERT_TRUE(cache.contains(49 * 40));

    // Now the playhead is dragged back to the start and the budget tightens.
    cache.frameFor(0);
    cache.setByteBudget(perFrame * 4);

    EXPECT_TRUE(cache.contains(0)) << "what is under the playhead must survive";
    EXPECT_FALSE(cache.contains(49 * 40));
    EXPECT_LE(cache.sizeBytes(), perFrame * 4);
}

TEST(VideoFrameCacheTests, AlwaysKeepsAtLeastOneFrame)
{
    VideoFrameCache cache(1);   // a budget nothing can fit in
    cache.put(makeFrame(1000), 40);

    // Showing the frame the playhead is on matters more than the budget.
    EXPECT_EQ(cache.count(), 1u);
    EXPECT_TRUE(cache.frameFor(1000).valid());
}

// ---------------------------------------------------------------------------
// Threading. Written by the decode worker, read by the GUI thread on every
// repaint, so this has to hold up under both at once.
// ---------------------------------------------------------------------------

TEST(VideoFrameCacheTests, SurvivesConcurrentReadsAndWrites)
{
    VideoFrameCache cache(2u * 1024u * 1024u);
    std::atomic<bool> stop { false };
    std::atomic<int> reads { 0 };

    std::thread writer([&cache, &stop]() {
        for (int i = 0; !stop.load(); ++i) {
            cache.put(makeFrame((i % 500) * 40), 40);
        }
    });

    std::thread reader([&cache, &stop, &reads]() {
        while (!stop.load()) {
            const auto lookup = cache.frameFor((reads.load() % 500) * 40);
            if (lookup.valid()) {
                // Touch the pixels: a torn image would fault or mismatch here.
                (void)tintOf(lookup.frame);
            }
            reads.fetch_add(1);
        }
    });

    std::this_thread::sleep_for(std::chrono::milliseconds(200));
    stop.store(true);
    writer.join();
    reader.join();

    EXPECT_GT(reads.load(), 0);
    EXPECT_LE(cache.sizeBytes(), cache.byteBudget());
}

TEST(VideoFrameCacheTests, ClearIsSafeWhileBeingRead)
{
    VideoFrameCache cache;
    std::atomic<bool> stop { false };

    std::thread churn([&cache, &stop]() {
        for (int i = 0; !stop.load(); ++i) {
            cache.put(makeFrame((i % 100) * 40), 40);
            if (i % 50 == 0) {
                cache.clear();
            }
        }
    });

    for (int i = 0; i < 20000; ++i) {
        const auto lookup = cache.frameFor((i % 100) * 40);
        if (lookup.valid()) {
            (void)tintOf(lookup.frame);
        }
    }

    stop.store(true);
    churn.join();
    SUCCEED();
}
