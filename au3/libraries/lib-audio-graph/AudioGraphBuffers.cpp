/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioGraphBuffers.cpp

  Paul Licameli split from PerTrackEffect.cpp

**********************************************************************/

#include "AudioGraphBuffers.h"
#include "SampleCount.h"
#include <cassert>
#include <cstring>

AudioGraph::Buffers::Buffers(size_t blockSize)
    : mBufferSize{blockSize}, mBlockSize{blockSize}
{
    assert(blockSize > 0);
    assert(IsRewound());
}

AudioGraph::Buffers::Buffers(
    unsigned nChannels, size_t blockSize, size_t nBlocks,
    size_t padding)
{
    Reinit(nChannels, blockSize, nBlocks, padding);
}

void AudioGraph::Buffers::Reinit(
    unsigned nChannels, size_t blockSize, size_t nBlocks, size_t padding)
{
    assert(blockSize > 0);
    assert(nBlocks > 0);
    mBuffers.resize(nChannels);
    mPositions.resize(nChannels);
    const auto bufferSize = blockSize * nBlocks;
    for (auto& buffer : mBuffers) {
        // Guarantee initial zeroes (needed at least in last buffer sometimes)
        buffer.resize(bufferSize + padding, 0.0f);
    }
    mBufferSize = bufferSize;
    mBlockSize = blockSize;
    Rewind();
}

void AudioGraph::Buffers::Discard(size_t drop, size_t keep)
{
#ifndef NDEBUG
    sampleCount oldRemaining = Remaining();
#endif
    // Assert the pre
    assert(drop + keep <= Remaining());

#if 1
    // Bounds checking version not assuming the pre, new in 3.2
    if (mBuffers.empty()) {
        return;
    }

    // First buffer
    auto iterP = mPositions.begin();
    auto iterB = mBuffers.begin();
    auto position = *iterP;
    auto data = iterB->data();
    auto end = data + iterB->size();

    // Defend against excessive input values
    end = std::max(data, std::min(end, position + drop + keep));
    position = std::min(end, position);
    drop = std::min<size_t>(end - position, drop);
    // i.e. usually keep * sizeof(float) :
    const size_t size = ((end - position) - drop) * sizeof(float);
    memmove(position, position + drop, size);

    // other buffers; assuming equal sizes and relative positions (invariants)
    for (const auto endB = mBuffers.end(); ++iterB != endB;) {
        position = *++iterP;
        memmove(position, position + drop, size);
    }
#else
    // Version that assumes the precondition,
    // which pre-3.2 did without known errors
    for (auto position : mPositions) {
        memmove(position, position + drop, keep * sizeof(float));
    }
#endif
    // Assert the post
    assert(oldRemaining == Remaining());
}

void AudioGraph::Buffers::Advance(size_t count)
{
#ifndef NDEBUG
    sampleCount oldRemaining = Remaining();
#endif
    // Assert the pre
    assert(count <= Remaining());

#if 1
    // Bounds checking version not assuming the pre, new in 3.2
    if (mBuffers.empty()) {
        return;
    }

    // First buffer; defend against excessive count
    auto iterP = mPositions.begin();
    auto iterB = mBuffers.begin();
    auto& position = *iterP;
    auto data = iterB->data();
    auto end = data + iterB->size();
    // invariant assumed, and preserved
    assert(data <= position && position <= end);
    count = std::min<size_t>(end - position, count);
    position += count;
    assert(data <= position && position <= end);

    // other buffers; assuming equal sizes and relative positions (invariants)
    for (const auto endB = mBuffers.end(); ++iterB != endB;) {
        auto& position = *++iterP;
        // invariant assumed, and preserved
        assert(iterB->data() <= position);
        assert(position <= iterB->data() + iterB->size());

        position += count;

        assert(iterB->data() <= position);
        assert(position <= iterB->data() + iterB->size());
    }
#else
    // Version that assumes the precondition,
    // which pre-3.2 did without known errors
    for (auto& position : mPositions) {
        position += count;
    }
#endif
    // Assert the post
    assert(Remaining() == oldRemaining - count);
}

void AudioGraph::Buffers::Rewind()
{
    auto iterP = mPositions.begin();
    for (auto& buffer : mBuffers) {
        *iterP++ = buffer.data();
    }
    assert(IsRewound());
}

size_t AudioGraph::Buffers::Rotate()
{
    auto oldRemaining = Remaining();
    Rewind();
    const auto free = BufferSize() - oldRemaining;
    // Shift any partial block of unread data leftward
    Discard(free, oldRemaining);
    assert(IsRewound());
    return oldRemaining;
}

constSamplePtr AudioGraph::Buffers::GetReadPosition(unsigned iChannel) const
{
    iChannel = std::min(iChannel, Channels() - 1);
    auto buffer = mBuffers[iChannel].data();
    return reinterpret_cast<constSamplePtr>(buffer);
}

float& AudioGraph::Buffers::GetWritePosition(unsigned iChannel)
{
    assert(iChannel < Channels());
    return mBuffers[iChannel].data()[ Position() ];
}

void AudioGraph::Buffers::ClearBuffer(unsigned iChannel, size_t n)
{
    if (iChannel < mPositions.size()) {
        auto p = mPositions[iChannel];
        auto& buffer = mBuffers[iChannel];
        auto end = buffer.data() + buffer.size();
        p = std::min(end, p);
        n = std::min<size_t>(end - p, n);
        std::fill(p, p + n, 0);
    }
}
