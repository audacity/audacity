/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WideSampleSource.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from PerTrackEffect.cpp

*******************************************************************//**

\class PerTrackEffect
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/
#include "WideSampleSource.h"

#include "AudioGraphBuffers.h"
#include "WideSampleSequence.h"
#include <cassert>

WideSampleSource::WideSampleSource(const WideSampleSequence& sequence,
                                   size_t nChannels, sampleCount start, sampleCount len, Poller pollUser)
    : mSequence{sequence}, mnChannels{nChannels}, mPollUser{move(pollUser)}
    , mPos{start}, mOutputRemaining{len}
{
    assert(nChannels <= sequence.NChannels());
}

WideSampleSource::~WideSampleSource() = default;

bool WideSampleSource::AcceptsBuffers(const Buffers& buffers) const
{
    return mOutputRemaining == 0 || buffers.Channels() > 0;
}

bool WideSampleSource::AcceptsBlockSize(size_t) const
{
    return true;
}

sampleCount WideSampleSource::Remaining() const
{
    return std::max<sampleCount>(0, mOutputRemaining);
}

#define stackAllocate(T, count) static_cast<T*>(alloca(count * sizeof(T)))

std::optional<size_t> WideSampleSource::Acquire(Buffers& data, size_t bound)
{
    assert(bound <= data.BlockSize());
    assert(data.BlockSize() <= data.Remaining());
    assert(AcceptsBuffers(data));
    assert(AcceptsBlockSize(data.BlockSize()));

    if (!mInitialized || mFetched < bound) {
        // Need to fill sufficent data in the buffers
        // Calculate the number of samples to get
        const auto fetch
            =limitSampleBufferSize(data.Remaining() - mFetched, Remaining());
        // guarantees write won't overflow
        assert(mFetched + fetch <= data.Remaining());
        // Fill the buffers
        auto buffers = stackAllocate(float*, mnChannels);
        if (mnChannels > 0) {
            buffers[0] = &data.GetWritePosition(0) + mFetched;
        }
        if (mnChannels > 1) {
            buffers[1] = &data.GetWritePosition(1) + mFetched;
        }
        mSequence.GetFloats(0, mnChannels, buffers, mPos, fetch);
        mPos += fetch;
        mFetched += fetch;
        mInitialized = true;
    }
    assert(data.Remaining() > 0);
    auto result = mLastProduced = std::min(bound,
                                           limitSampleBufferSize(data.Remaining(), Remaining()));
    // assert post
    assert(result <= bound);
    assert(result <= data.Remaining());
    assert(result <= Remaining());
    // true because the three terms of the min would be positive
    assert(bound == 0 || Remaining() == 0 || result > 0);
    return { result };
}

bool WideSampleSource::Release()
{
    mOutputRemaining -= mLastProduced;
    mFetched -= mLastProduced;
    mLastProduced = 0;
    assert(mOutputRemaining >= 0);
    return !mPollUser || mPollUser(mPos);
}
