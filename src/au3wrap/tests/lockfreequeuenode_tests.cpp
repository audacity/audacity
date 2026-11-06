/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <atomic>
#include <chrono>
#include <functional>
#include <thread>
#include <vector>

#include "au3-clip-processing/sequence-graph/LockFreeQueueNode.h"

namespace {
constexpr size_t TEST_NUM_CHANNELS = 1;
constexpr size_t TEST_CHUNK_SIZE = 16;
constexpr size_t TEST_NUM_CHUNKS = 4;
constexpr size_t TEST_CAPACITY = TEST_CHUNK_SIZE * TEST_NUM_CHUNKS;
constexpr size_t TEST_RESYNC_LEAD = 2 * TEST_CHUNK_SIZE;
//! Added to the samples of the input once it changes
constexpr long long TEST_CHANGED_OFFSET = 5000;
constexpr auto TEST_TIMEOUT = std::chrono::seconds(5);
constexpr auto TEST_POLL_PERIOD = std::chrono::milliseconds(1);

//! Its samples are their indices, plus an offset that changes the input
class IndexNode final : public ISequenceProcessingNode
{
public:
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        const auto offset = m_offset.load();
        for (size_t i = 0; i < len; ++i) {
            buffers[0][i] = static_cast<float>(offset + start.as_long_long() + static_cast<long long>(i));
        }
    }

    std::atomic<long long> m_offset { 0 };
};

std::vector<float> read(ISequenceProcessingNode& node, long long start, size_t len)
{
    std::vector<float> buffer(len);
    float* const buffers[] { buffer.data() };
    node.GetSamples(buffers, start, len);
    return buffer;
}

std::vector<float> values(long long start, size_t len)
{
    std::vector<float> result(len);
    for (size_t i = 0; i < len; ++i) {
        result[i] = static_cast<float>(start + static_cast<long long>(i));
    }
    return result;
}

//! Calls `poll` until it returns true, for at most `TEST_TIMEOUT`
bool waitFor(const std::function<bool()>& poll)
{
    const auto deadline = std::chrono::steady_clock::now() + TEST_TIMEOUT;
    while (std::chrono::steady_clock::now() < deadline) {
        if (poll()) {
            return true;
        }
        std::this_thread::sleep_for(TEST_POLL_PERIOD);
    }
    return false;
}

std::unique_ptr<LockFreeQueueNode> makeQueue(std::shared_ptr<IndexNode> input)
{
    return std::make_unique<LockFreeQueueNode>(std::move(input), TEST_NUM_CHANNELS, TEST_CHUNK_SIZE, TEST_NUM_CHUNKS,
                                               TEST_RESYNC_LEAD);
}
}

TEST(LockFreeQueueNodeTests, readsGetTheInput)
{
    // Started, the queue is full: reading up to its capacity doesn't depend on the worker
    const auto queue = makeQueue(std::make_shared<IndexNode>());
    queue->Start();
    constexpr size_t firstLen = TEST_CHUNK_SIZE / 2 + 1;
    queue->Prepare(0);
    EXPECT_EQ(read(*queue, 0, firstLen), values(0, firstLen));
    queue->Prepare(firstLen);
    EXPECT_EQ(read(*queue, firstLen, TEST_CAPACITY - firstLen), values(firstLen, TEST_CAPACITY - firstLen));
}

TEST(LockFreeQueueNodeTests, readsCanBeReadAgain)
{
    const auto queue = makeQueue(std::make_shared<IndexNode>());
    queue->Start();
    queue->Prepare(0);
    const auto first = read(*queue, 0, TEST_CAPACITY);
    EXPECT_EQ(read(*queue, 0, TEST_CAPACITY), first);
}

TEST(LockFreeQueueNodeTests, anInvalidationIsSilentAndResynchronizes)
{
    //! [GIVEN] the input changed after it was read ahead
    const auto input = std::make_shared<IndexNode>();
    const auto queue = makeQueue(input);
    queue->Start();
    input->m_offset = TEST_CHANGED_OFFSET;

    //! [WHEN] what was read ahead is invalidated
    queue->Invalidate();

    //! [THEN] it is silent where it was read ahead ...
    queue->Prepare(0);
    EXPECT_EQ(read(*queue, 0, TEST_CHUNK_SIZE), std::vector<float>(TEST_CHUNK_SIZE, 0.f));

    //! ... and the changed input is read again shortly ahead of the stream, in the worker's own time
    const auto expected = values(TEST_CHANGED_OFFSET + TEST_RESYNC_LEAD, TEST_CHUNK_SIZE);
    EXPECT_TRUE(waitFor([&] {
        queue->Prepare(0);
        return read(*queue, TEST_RESYNC_LEAD, TEST_CHUNK_SIZE) == expected;
    }));
}

TEST(LockFreeQueueNodeTests, saysHowMuchIsQueued)
{
    //! [GIVEN] a full queue, from where the stream is
    const auto queue = makeQueue(std::make_shared<IndexNode>());
    queue->Start();
    queue->Prepare(0);

    //! [THEN] what it holds is available, up to its end
    constexpr size_t start = TEST_CAPACITY - TEST_CHUNK_SIZE / 2;
    EXPECT_EQ(queue->Available(0, TEST_CAPACITY), TEST_CAPACITY);
    EXPECT_EQ(queue->Available(start, TEST_CHUNK_SIZE), TEST_CAPACITY - start);
    EXPECT_EQ(queue->Available(TEST_CAPACITY, TEST_CHUNK_SIZE), 0u);
}

TEST(LockFreeQueueNodeTests, withoutResyncLeadAWaitingReaderGetsAllTheInput)
{
    //! [GIVEN] a queue that resynchronizes where the stream is
    const auto input = std::make_shared<IndexNode>();
    const auto queue = std::make_unique<LockFreeQueueNode>(input, TEST_NUM_CHANNELS, TEST_CHUNK_SIZE, TEST_NUM_CHUNKS, 0);
    queue->Start();
    input->m_offset = TEST_CHANGED_OFFSET;

    //! [WHEN] what was read ahead is invalidated
    queue->Invalidate();

    //! [THEN] a reader that waits gets the changed input from where it is, without a gap
    EXPECT_TRUE(waitFor([&] {
        queue->Prepare(0);
        return queue->Available(0, TEST_CHUNK_SIZE) == TEST_CHUNK_SIZE;
    }));
    EXPECT_EQ(read(*queue, 0, TEST_CHUNK_SIZE), values(TEST_CHANGED_OFFSET, TEST_CHUNK_SIZE));
}
