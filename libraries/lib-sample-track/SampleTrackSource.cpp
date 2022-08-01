/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SampleTrackSource.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from PerTrackEffect.cpp

*******************************************************************//**

\class PerTrackEffect
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/


#include "SampleTrackSource.h"

#include "AudioGraphBuffers.h"
#include "SampleTrack.h"
#include <cassert>

SampleTrackSource::SampleTrackSource(
   const SampleTrack &left, const SampleTrack *pRight,
   sampleCount start, sampleCount len, Poller pollUser
)  : mLeft{ left }, mpRight{ pRight }, mPollUser{ move(pollUser) }
   , mPos{ start }, mOutputRemaining{ len  }
{
}

SampleTrackSource::~SampleTrackSource() = default;

bool SampleTrackSource::AcceptsBuffers(const Buffers &buffers) const
{
   return mOutputRemaining == 0 || buffers.Channels() > 0;
}

bool SampleTrackSource::AcceptsBlockSize(size_t) const
{
   return true;
}

sampleCount SampleTrackSource::Remaining() const
{
   return std::max<sampleCount>(0, mOutputRemaining);
}

std::optional<size_t> SampleTrackSource::Acquire(Buffers &data, size_t bound)
{
   assert(bound <= data.BlockSize());
   assert(data.BlockSize() <= data.Remaining());
   assert(AcceptsBuffers(data));
   assert(AcceptsBlockSize(data.BlockSize()));

   if (!mInitialized || mFetched < bound) {
      // Need to fill sufficent data in the buffers
      // Calculate the number of samples to get
      const auto fetch =
         limitSampleBufferSize(data.Remaining() - mFetched, Remaining());
      // guarantees write won't overflow
      assert(mFetched + fetch <= data.Remaining());
      // Fill the buffers
      mLeft.GetFloats(&data.GetWritePosition(0) + mFetched, mPos, fetch);
      if (mpRight && data.Channels() > 1)
         mpRight->GetFloats(&data.GetWritePosition(1) + mFetched, mPos, fetch);
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

bool SampleTrackSource::Release()
{
   mOutputRemaining -= mLastProduced;
   mFetched -= mLastProduced;
   mLastProduced = 0;
   assert(mOutputRemaining >= 0);
   return !mPollUser || mPollUser(mPos);
}
