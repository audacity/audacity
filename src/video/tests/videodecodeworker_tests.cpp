/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>

#include "internal/videodecodeworker.h"

using namespace au::video;
using namespace std::chrono_literals;

namespace {
//! A backend that decodes nothing, so the worker's behaviour can be tested
//! without FFmpeg, without fixtures, and without waiting on real decoding.
class FakeBackend : public IVideoDecodeBackend
{
public:
    VideoError open(const std::string&) override { m_open = true; return VideoError::None; }
    void close() override { m_open = false; }
    bool isOpen() const override { return m_open; }
    const VideoStreamInfo& streamInfo() const override { return m_info; }
    int64_t frameDurationPts() const override { return 40; }

    VideoError lastFrameError() const override { return m_frameError.load(); }
    void setFrameError(VideoError err) { m_frameError.store(err); }

    //! Milliseconds, matching the pts the fake frames carry.
    int64_t timeToPts(muse::secs_t time) const override
    {
        return static_cast<int64_t>(time.to_double() * 1000.0);
    }

    VideoFrame frameAt(muse::secs_t time, int width, int height) override
    {
        m_calls.fetch_add(1);
        m_lastWidth.store(width);
        m_lastHeight.store(height);
        m_lastTimeMs.store(static_cast<int64_t>(time.to_double() * 1000.0));

        if (m_delay.count() > 0) {
            std::this_thread::sleep_for(m_delay);
        }

        if (m_failing.load()) {
            return VideoFrame();
        }

        VideoFrame frame;
        frame.image = QImage(width, height, QImage::Format_RGB888);
        frame.image.fill(Qt::white);
        // Quantise to the frame grid so repeated nearby requests land on the
        // same cache entry, the way a real decoder behaves.
        frame.pts = (static_cast<int64_t>(time.to_double() * 1000.0) / 40) * 40;
        frame.time = time;
        return frame;
    }

    void setDelay(std::chrono::milliseconds delay) { m_delay = delay; }
    void setFailing(bool failing) { m_failing.store(failing); }

    int calls() const { return m_calls.load(); }
    int lastWidth() const { return m_lastWidth.load(); }
    int lastHeight() const { return m_lastHeight.load(); }
    int64_t lastTimeMs() const { return m_lastTimeMs.load(); }

private:
    bool m_open = false;
    VideoStreamInfo m_info;
    std::chrono::milliseconds m_delay { 0 };
    std::atomic<bool> m_failing { false };
    std::atomic<VideoError> m_frameError { VideoError::None };
    std::atomic<int> m_calls { 0 };
    std::atomic<int> m_lastWidth { 0 };
    std::atomic<int> m_lastHeight { 0 };
    std::atomic<int64_t> m_lastTimeMs { 0 };
};

//! Blocks until a condition holds or the deadline passes, so nothing here
//! depends on a fixed sleep being long enough.
template<typename Predicate>
bool waitFor(Predicate predicate, std::chrono::milliseconds timeout = 3000ms)
{
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    while (std::chrono::steady_clock::now() < deadline) {
        if (predicate()) {
            return true;
        }
        std::this_thread::sleep_for(1ms);
    }
    return predicate();
}

VideoDecodeWorker::Request requestAt(double seconds, int w = 320, int h = 180)
{
    VideoDecodeWorker::Request r;
    r.time = muse::secs_t(seconds);
    r.targetWidth = w;
    r.targetHeight = h;
    return r;
}
}

TEST(VideoDecodeWorkerTests, DoesNotRunUntilStarted)
{
    auto backend = std::make_shared<FakeBackend>();
    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    EXPECT_FALSE(worker.isRunning());
    worker.request(requestAt(1.0));

    std::this_thread::sleep_for(50ms);
    EXPECT_EQ(backend->calls(), 0);
}

TEST(VideoDecodeWorkerTests, DecodesARequestAndCachesIt)
{
    auto backend = std::make_shared<FakeBackend>();
    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    std::atomic<int> ready { 0 };
    worker.setFrameReadyCallback([&ready]() { ready.fetch_add(1); });

    worker.start();
    EXPECT_TRUE(worker.isRunning());

    worker.request(requestAt(1.0));

    ASSERT_TRUE(waitFor([&ready]() { return ready.load() > 0; }));
    EXPECT_GE(backend->calls(), 1);
    EXPECT_EQ(backend->lastWidth(), 320);
    EXPECT_EQ(backend->lastHeight(), 180);
    EXPECT_GT(cache.count(), 0u);

    worker.stop();
    EXPECT_FALSE(worker.isRunning());
}

TEST(VideoDecodeWorkerTests, IgnoresRequestsWithNoSize)
{
    auto backend = std::make_shared<FakeBackend>();
    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);
    worker.start();

    worker.request(requestAt(1.0, 0, 180));
    worker.request(requestAt(1.0, 320, 0));
    worker.request(requestAt(1.0, -5, 180));

    std::this_thread::sleep_for(60ms);
    EXPECT_EQ(backend->calls(), 0) << "a zero-sized panel has nothing to decode into";

    worker.stop();
}

// ---------------------------------------------------------------------------
// Latest-wins. A playhead being dragged produces requests faster than any
// decoder can service them, and every superseded one is wasted work.
// ---------------------------------------------------------------------------

TEST(VideoDecodeWorkerTests, KeepsOnlyTheNewestPendingRequest)
{
    auto backend = std::make_shared<FakeBackend>();
    backend->setDelay(40ms);

    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);
    worker.start();

    // First one starts decoding and occupies the thread; the rest pile up on
    // a slot that only holds one, so only the last of them should be served.
    worker.request(requestAt(1.0));
    std::this_thread::sleep_for(10ms);

    for (int i = 0; i < 20; ++i) {
        worker.request(requestAt(2.0 + i));
    }

    ASSERT_TRUE(waitFor([&]() { return worker.servedCount() >= 2; }));
    std::this_thread::sleep_for(120ms);

    EXPECT_LE(backend->calls(), 3)
        << "twenty queued requests should not become twenty decodes";
    EXPECT_GT(worker.supersededCount(), 10u);
    EXPECT_EQ(backend->lastTimeMs(), 21000)
        << "the newest request is the one that matters";

    worker.stop();
}

TEST(VideoDecodeWorkerTests, CountsWhatItDropped)
{
    auto backend = std::make_shared<FakeBackend>();
    backend->setDelay(30ms);

    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);
    worker.start();

    worker.request(requestAt(0.0));
    std::this_thread::sleep_for(5ms);
    worker.request(requestAt(1.0));
    worker.request(requestAt(2.0));
    worker.request(requestAt(3.0));

    ASSERT_TRUE(waitFor([&]() { return worker.servedCount() >= 2; }));
    EXPECT_EQ(worker.supersededCount(), 2u);

    worker.stop();
}

// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------

TEST(VideoDecodeWorkerTests, StopWaitsForTheThreadEvenMidDecode)
{
    auto backend = std::make_shared<FakeBackend>();
    backend->setDelay(80ms);

    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);
    worker.start();
    worker.request(requestAt(1.0));

    ASSERT_TRUE(waitFor([&]() { return backend->calls() > 0; }));

    // Stopping in the middle of a decode has to join rather than detach, or
    // the backend is destroyed underneath a thread still using it.
    worker.stop();
    EXPECT_FALSE(worker.isRunning());
}

TEST(VideoDecodeWorkerTests, StopIsIdempotent)
{
    auto backend = std::make_shared<FakeBackend>();
    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    worker.stop();          // never started
    worker.start();
    worker.stop();
    worker.stop();          // again
    SUCCEED();
}

TEST(VideoDecodeWorkerTests, IgnoresRequestsAfterStopping)
{
    auto backend = std::make_shared<FakeBackend>();
    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    worker.start();
    worker.stop();

    const int before = backend->calls();
    worker.request(requestAt(5.0));
    std::this_thread::sleep_for(50ms);
    EXPECT_EQ(backend->calls(), before);
}

TEST(VideoDecodeWorkerTests, CanBeRestarted)
{
    auto backend = std::make_shared<FakeBackend>();
    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    std::atomic<int> ready { 0 };
    worker.setFrameReadyCallback([&ready]() { ready.fetch_add(1); });

    worker.start();
    worker.request(requestAt(1.0));
    ASSERT_TRUE(waitFor([&ready]() { return ready.load() > 0; }));
    worker.stop();

    const int afterFirst = ready.load();

    worker.start();
    worker.request(requestAt(2.0));
    ASSERT_TRUE(waitFor([&]() { return ready.load() > afterFirst; }));
    worker.stop();
}

TEST(VideoDecodeWorkerTests, DestructorStopsTheThread)
{
    auto backend = std::make_shared<FakeBackend>();
    backend->setDelay(30ms);
    VideoFrameCache cache;

    {
        VideoDecodeWorker worker(backend, &cache);
        worker.start();
        worker.request(requestAt(1.0));
        ASSERT_TRUE(waitFor([&]() { return backend->calls() > 0; }));
    }   // must join here rather than leaving a thread on a destroyed object

    SUCCEED();
}

// ---------------------------------------------------------------------------
// Failure handling
// ---------------------------------------------------------------------------

TEST(VideoDecodeWorkerTests, AFailedDecodeCachesNothingAndDoesNotNotify)
{
    auto backend = std::make_shared<FakeBackend>();
    backend->setFailing(true);

    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    std::atomic<int> ready { 0 };
    worker.setFrameReadyCallback([&ready]() { ready.fetch_add(1); });

    worker.start();
    worker.request(requestAt(1.0));

    ASSERT_TRUE(waitFor([&]() { return backend->calls() > 0; }));
    std::this_thread::sleep_for(50ms);

    EXPECT_EQ(cache.count(), 0u);
    EXPECT_EQ(ready.load(), 0) << "nothing was produced, so nothing to repaint";

    worker.stop();
}

TEST(VideoDecodeWorkerTests, RecoversAfterAFailure)
{
    auto backend = std::make_shared<FakeBackend>();
    backend->setFailing(true);

    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    std::atomic<int> ready { 0 };
    worker.setFrameReadyCallback([&ready]() { ready.fetch_add(1); });

    worker.start();
    worker.request(requestAt(1.0));
    ASSERT_TRUE(waitFor([&]() { return backend->calls() > 0; }));

    backend->setFailing(false);
    worker.request(requestAt(2.0));

    ASSERT_TRUE(waitFor([&ready]() { return ready.load() > 0; }));
    EXPECT_GT(cache.count(), 0u);

    worker.stop();
}

TEST(VideoDecodeWorkerTests, WorksWithoutACache)
{
    auto backend = std::make_shared<FakeBackend>();
    VideoDecodeWorker worker(backend, nullptr);

    std::atomic<int> ready { 0 };
    worker.setFrameReadyCallback([&ready]() { ready.fetch_add(1); });

    worker.start();
    worker.request(requestAt(1.0));
    EXPECT_TRUE(waitFor([&ready]() { return ready.load() > 0; }));
    worker.stop();
}

// ---------------------------------------------------------------------------
// Under load
// ---------------------------------------------------------------------------

TEST(VideoDecodeWorkerTests, SurvivesAFloodOfRequests)
{
    auto backend = std::make_shared<FakeBackend>();
    backend->setDelay(1ms);

    VideoFrameCache cache(4u * 1024u * 1024u);
    VideoDecodeWorker worker(backend, &cache);

    std::atomic<int> ready { 0 };
    worker.setFrameReadyCallback([&ready]() { ready.fetch_add(1); });
    worker.start();

    // Roughly what dragging a playhead across a long project produces.
    for (int i = 0; i < 2000; ++i) {
        worker.request(requestAt(i * 0.01));
    }

    ASSERT_TRUE(waitFor([&ready]() { return ready.load() > 0; }));
    worker.stop();

    EXPECT_LT(backend->calls(), 2000)
        << "the point of latest-wins is that most of these never run";
    EXPECT_LE(cache.sizeBytes(), cache.byteBudget());
}

TEST(VideoDecodeWorkerTests, ReportsWhyAFrameCouldNotBeProduced)
{
    auto backend = std::make_shared<FakeBackend>();
    backend->setFailing(true);
    backend->setFrameError(VideoError::UnsupportedFormat);

    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    std::atomic<int> failures { 0 };
    std::atomic<VideoError> reported { VideoError::None };
    worker.setFrameFailedCallback([&](VideoError err) {
        reported.store(err);
        failures.fetch_add(1);
    });

    worker.start();
    worker.request(requestAt(1.0));

    // Silently dropping this is what turns an unsupported file into a black
    // rectangle with the resolution printed under it and no explanation.
    ASSERT_TRUE(waitFor([&failures]() { return failures.load() > 0; }));
    EXPECT_EQ(reported.load(), VideoError::UnsupportedFormat);
    EXPECT_EQ(cache.count(), 0u);

    worker.stop();
}

TEST(VideoDecodeWorkerTests, DoesNotReportFailureWhenAFrameArrives)
{
    auto backend = std::make_shared<FakeBackend>();
    VideoFrameCache cache;
    VideoDecodeWorker worker(backend, &cache);

    std::atomic<int> failures { 0 };
    std::atomic<int> ready { 0 };
    worker.setFrameFailedCallback([&failures](VideoError) { failures.fetch_add(1); });
    worker.setFrameReadyCallback([&ready]() { ready.fetch_add(1); });

    worker.start();
    worker.request(requestAt(1.0));

    ASSERT_TRUE(waitFor([&ready]() { return ready.load() > 0; }));
    std::this_thread::sleep_for(50ms);
    EXPECT_EQ(failures.load(), 0);

    worker.stop();
}

// ---------------------------------------------------------------------------
// Switching away from a video must actually stop it. The panel follows the
// current project's playhead, so a stale backend keeps drawing the previous
// project's picture against the new project's time.
// ---------------------------------------------------------------------------

TEST(VideoDecodeWorkerTests, StoppingReleasesTheBackend)
{
    std::weak_ptr<FakeBackend> observer;

    {
        auto backend = std::make_shared<FakeBackend>();
        observer = backend;

        VideoFrameCache cache;
        VideoDecodeWorker worker(backend, &cache);
        worker.start();
        worker.request(requestAt(1.0));
        ASSERT_TRUE(waitFor([&]() { return backend->calls() > 0; }));
        worker.stop();
    }

    // The worker holds the only other reference. If it outlived its owner the
    // backend would still be decoding against a project that has gone away.
    EXPECT_TRUE(observer.expired());
}
