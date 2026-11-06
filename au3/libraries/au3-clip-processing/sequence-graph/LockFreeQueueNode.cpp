/*
 * Audacity: A Digital Audio Editor
 */
#include "LockFreeQueueNode.h"

#include <algorithm>
#include <chrono>
#include <optional>

namespace {
//! How long the worker sleeps when there is nothing to do
constexpr auto workerPollPeriod = std::chrono::milliseconds(5);
}

LockFreeQueueNode::LockFreeQueueNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numChannels, size_t chunkSize,
                                     size_t numChunks, size_t resyncLead)
    : mInput{std::move(input)}
    , mNumChannels{numChannels}
    , mChunkSize{chunkSize}
    , mResyncLead{static_cast<long long>(resyncLead)}
    , mChunks(numChunks)
    , mWritePointers(numChannels)
{
    for (auto& chunk : mChunks) {
        chunk.channels.assign(numChannels, std::vector<float>(chunkSize));
    }
}

LockFreeQueueNode::~LockFreeQueueNode()
{
    mStopping.store(true, std::memory_order_relaxed);
    if (mWorker.joinable()) {
        mWorker.join();
    }
}

void LockFreeQueueNode::Start()
{
    mNext = mReaderIndex.load(std::memory_order_relaxed);
    while (Produce()) {
    }
    mWorker = std::thread { [this] { Work(); } };
}

void LockFreeQueueNode::Invalidate()
{
    // Release: the worker then reads the changed input
    mGeneration.fetch_add(1, std::memory_order_release);
}

sampleCount LockFreeQueueNode::ReadEnd() const
{
    // Sequentially consistent with its store: the changes of the input before this load are seen by reads after it
    return mReadEnd.load();
}

// Worker side

bool LockFreeQueueNode::Produce()
{
    // Before reading the input, so that a chunk read while the input changes is of the previous generation
    const auto generation = mGeneration.load(std::memory_order_acquire);
    const auto readerIndex = mReaderIndex.load(std::memory_order_relaxed);
    if (generation != mWorkerGeneration) {
        mWorkerGeneration = generation;
        mNext = readerIndex + mResyncLead;
    }

    const auto numWritten = mNumWritten.load(std::memory_order_relaxed);
    if (numWritten - mNumRead.load(std::memory_order_acquire) == mChunks.size()) {
        return false;
    }
    if (mNext + static_cast<long long>(mChunkSize) <= readerIndex) {
        // Too late to be played: catch up
        mNext = readerIndex + mResyncLead;
    }
    auto& chunk = mChunks[numWritten % mChunks.size()];
    for (size_t i = 0; i < mNumChannels; ++i) {
        mWritePointers[i] = chunk.channels[i].data();
    }
    // Before reading, which then sees the input as it is after this store
    mReadEnd.store(mNext + static_cast<long long>(mChunkSize));
    mInput->GetSamples(mWritePointers.data(), mNext, mChunkSize);
    chunk.index = mNext;
    chunk.len = mChunkSize;
    chunk.generation = generation;
    mNext += static_cast<long long>(mChunkSize);
    mNumWritten.store(numWritten + 1, std::memory_order_release);
    return true;
}

void LockFreeQueueNode::Work()
{
    while (!mStopping.load(std::memory_order_relaxed)) {
        if (!Produce()) {
            std::this_thread::sleep_for(workerPollPeriod);
        }
    }
}

// Reader side

void LockFreeQueueNode::PopFront()
{
    mNumRead.store(mNumRead.load(std::memory_order_relaxed) + 1, std::memory_order_release);
}

const LockFreeQueueNode::Chunk* LockFreeQueueNode::Find(long long index) const
{
    const auto numWritten = mNumWritten.load(std::memory_order_acquire);
    for (auto n = mNumRead.load(std::memory_order_relaxed); n != numWritten; ++n) {
        const auto& chunk = mChunks[n % mChunks.size()];
        if (chunk.generation == mReaderGeneration && chunk.index <= index && index < chunk.End()) {
            return &chunk;
        }
    }
    return nullptr;
}

long long LockFreeQueueNode::NextQueued(long long index) const
{
    std::optional<long long> next;
    const auto numWritten = mNumWritten.load(std::memory_order_acquire);
    for (auto n = mNumRead.load(std::memory_order_relaxed); n != numWritten; ++n) {
        const auto& chunk = mChunks[n % mChunks.size()];
        if (chunk.generation == mReaderGeneration && chunk.index > index) {
            next = std::min(next.value_or(chunk.index), chunk.index);
        }
    }
    return next.value_or(-1);
}

void LockFreeQueueNode::Prepare(sampleCount sampleIndex)
{
    const auto index = sampleIndex.as_long_long();
    mReaderIndex.store(index, std::memory_order_relaxed);
    // Until the next preparation, so that re-reading gets the same
    mReaderGeneration = mGeneration.load(std::memory_order_acquire);

    // Release what the stream is past, and what was read before the input changed. Chunks are queued in the order of
    // their generations; the worker may already have queued some of a generation this preparation doesn't know yet.
    while (mNumRead.load(std::memory_order_relaxed) != mNumWritten.load(std::memory_order_acquire)) {
        const auto& front = mChunks[mNumRead.load(std::memory_order_relaxed) % mChunks.size()];
        const auto older = static_cast<int32_t>(front.generation - mReaderGeneration) < 0;
        if (!older && front.End() > index) {
            break;
        }
        PopFront();
    }
}

size_t LockFreeQueueNode::Available(sampleCount start, size_t len) const
{
    size_t done = 0;
    while (done < len) {
        const auto index = start.as_long_long() + static_cast<long long>(done);
        const auto chunk = Find(index);
        if (!chunk) {
            break;
        }
        done += std::min(len - done, static_cast<size_t>(chunk->End() - index));
    }
    return done;
}

void LockFreeQueueNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    size_t done = 0;
    while (done < len) {
        const auto index = start.as_long_long() + static_cast<long long>(done);
        if (const auto chunk = Find(index)) {
            const auto offset = static_cast<size_t>(index - chunk->index);
            const auto count = std::min(len - done, chunk->len - offset);
            for (size_t i = 0; i < mNumChannels; ++i) {
                std::copy_n(chunk->channels[i].data() + offset, count, buffers[i] + done);
            }
            done += count;
            continue;
        }
        const auto next = NextQueued(index);
        const auto count = next < 0 ? len - done : std::min(len - done, static_cast<size_t>(next - index));
        for (size_t i = 0; i < mNumChannels; ++i) {
            std::fill_n(buffers[i] + done, count, 0.f);
        }
        done += count;
    }
}
