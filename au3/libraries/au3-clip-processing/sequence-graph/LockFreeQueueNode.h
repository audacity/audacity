/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ISequenceProcessingNode.h"

#include <atomic>
#include <cstdint>
#include <memory>
#include <thread>
#include <vector>

/**
 * @brief Reads its input ahead on a worker thread, so that reading it doesn't wait for the input
 *
 * @details It is read by index in a stream, e.g. the samples of an audio device, as its input is. The worker reads the
 * input ahead of where the stream was last read, into a queue of chunks. Reading takes from the queue, without locking
 * nor allocating.
 *
 * Reading may re-read what was read last, up to the index it was prepared for: see `Prepare`.
 *
 * If the input changes, e.g. its stream jumps, what is queued must be invalidated: see `Invalidate`.
 *
 * If the queue doesn't have what the stream needs, the reader gets silence: if the worker is late, until it catches
 * up, skipping ahead if it fell behind; after an invalidation, until the worker resynchronizes with the stream shortly
 * ahead.
 *
 * Once started, the input is only read by the worker.
 */
class LockFreeQueueNode final : public ISequenceProcessingNode
{
public:
    //! @param resyncLead how far ahead of the stream the worker resumes reading, when it fell behind or after an
    //! invalidation
    LockFreeQueueNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numChannels, size_t chunkSize, size_t numChunks,
                      size_t resyncLead);
    ~LockFreeQueueNode() override;

    //! Fills the queue, then starts the worker. Call once, before reading.
    void Start();

    //! Any thread, after the input changed: what is queued isn't read anymore, and the worker resynchronizes
    void Invalidate();
    //! Any thread: where what the worker read, or is reading, ends. Unless invalidated, what is queued was read before
    //! this was loaded.
    sampleCount ReadEnd() const;

    //! Reader: the stream is at `index`. Releases what is before it.
    void Prepare(sampleCount index);

    //! Reader: the samples of the stream from `start`, which must not be before the index last prepared
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

    //! Reader: how many samples of the stream from `start` on are queued, at most `len`. A reader that waits for them,
    //! rather than reading silence, plays the input late rather than skipping it, when the worker is late.
    size_t Available(sampleCount start, size_t len) const;

private:
    struct Chunk {
        std::vector<std::vector<float> > channels;
        long long index = 0;
        size_t len = 0;
        //! The generation of the input it was read from
        uint32_t generation = 0;

        long long End() const { return index + static_cast<long long>(len); }
    };

    // Worker side
    bool Produce();
    void Work();

    // Reader side
    void PopFront();
    const Chunk* Find(long long index) const;
    //! The first index from `index` on that something is queued for
    long long NextQueued(long long index) const;

    const std::shared_ptr<ISequenceProcessingNode> mInput;
    const size_t mNumChannels;
    const size_t mChunkSize;
    const long long mResyncLead;

    std::vector<Chunk> mChunks;
    //! How many chunks were written and read in total; a chunk is queued from its writing to its reading
    std::atomic<size_t> mNumWritten { 0 };
    std::atomic<size_t> mNumRead { 0 };

    //! Incremented when the input changes
    std::atomic<uint32_t> mGeneration { 0 };

    //! Where the stream was last prepared at
    std::atomic<long long> mReaderIndex { 0 };
    std::atomic<long long> mReadEnd { 0 };

    std::atomic<bool> mStopping { false };
    std::thread mWorker;

    // Worker only
    uint32_t mWorkerGeneration = 0;
    //! The index of the next chunk
    long long mNext = 0;
    std::vector<float*> mWritePointers;

    // Reader only
    uint32_t mReaderGeneration = 0;
};
