/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrackCache.cpp

Paul Licameli split from WaveTrack.cpp

**********************************************************************/

#include "SampleTrackCache.h"
#include "SampleTrack.h"

SampleTrackCache::~SampleTrackCache()
{
}

void SampleTrackCache::SetTrack(const std::shared_ptr<const SampleTrack> &pTrack)
{
   if (mPTrack != pTrack) {
      if (pTrack) {
         mBufferSize = pTrack->GetMaxBlockSize();
         if (!mPTrack ||
             mPTrack->GetMaxBlockSize() != mBufferSize) {
            Free();
            mBuffers[0].data = Floats{ mBufferSize };
            mBuffers[1].data = Floats{ mBufferSize };
         }
      }
      else
         Free();
      mPTrack = pTrack;
      mNValidBuffers = 0;
   }
}

const float *SampleTrackCache::GetFloats(
   sampleCount start, size_t len, bool mayThrow)
{
   constexpr auto format = floatSample;
   if (format == floatSample && len > 0) {
      const auto end = start + len;

      bool fillFirst = (mNValidBuffers < 1);
      bool fillSecond = (mNValidBuffers < 2);

      // Discard cached results that we no longer need
      if (mNValidBuffers > 0 &&
          (end <= mBuffers[0].start ||
           start >= mBuffers[mNValidBuffers - 1].end())) {
         // Complete miss
         fillFirst = true;
         fillSecond = true;
      }
      else if (mNValidBuffers == 2 &&
               start >= mBuffers[1].start &&
               end > mBuffers[1].end()) {
         // Request starts in the second buffer and extends past it.
         // Discard the first buffer.
         // (But don't deallocate the buffer space.)
         mBuffers[0] .swap ( mBuffers[1] );
         fillSecond = true;
         mNValidBuffers = 1;
      }
      else if (mNValidBuffers > 0 &&
         start < mBuffers[0].start &&
         0 <= mPTrack->GetBlockStart(start)) {
         // Request is not a total miss but starts before the cache,
         // and there is a clip to fetch from.
         // Not the access pattern for drawing spectrogram or playback,
         // but maybe scrubbing causes this.
         // Move the first buffer into second place, and later
         // refill the first.
         // (This case might be useful when marching backwards through
         // the track, as with scrubbing.)
         mBuffers[0] .swap ( mBuffers[1] );
         fillFirst = true;
         fillSecond = false;
         // Cache is not in a consistent state yet
         mNValidBuffers = 0;
      }

      // Refill buffers as needed
      if (fillFirst) {
         const auto start0 = mPTrack->GetBlockStart(start);
         if (start0 >= 0) {
            const auto len0 = mPTrack->GetBestBlockSize(start0);
            wxASSERT(len0 <= mBufferSize);
            if (!mPTrack->GetFloats(
                  mBuffers[0].data.get(), start0, len0,
                  fillZero, mayThrow))
               return nullptr;
            mBuffers[0].start = start0;
            mBuffers[0].len = len0;
            if (!fillSecond &&
                mBuffers[0].end() != mBuffers[1].start)
               fillSecond = true;
            // Keep the partially updated state consistent:
            mNValidBuffers = fillSecond ? 1 : 2;
         }
         else {
            // Request may fall between the clips of a track.
            // Invalidate all.  WaveTrack::Get() will return zeroes.
            mNValidBuffers = 0;
            fillSecond = false;
         }
      }
      wxASSERT(!fillSecond || mNValidBuffers > 0);
      if (fillSecond) {
         mNValidBuffers = 1;
         const auto end0 = mBuffers[0].end();
         if (end > end0) {
            const auto start1 = mPTrack->GetBlockStart(end0);
            if (start1 == end0) {
               const auto len1 = mPTrack->GetBestBlockSize(start1);
               wxASSERT(len1 <= mBufferSize);
               if (!mPTrack->GetFloats(mBuffers[1].data.get(), start1, len1, fillZero, mayThrow))
                  return nullptr;
               mBuffers[1].start = start1;
               mBuffers[1].len = len1;
               mNValidBuffers = 2;
            }
         }
      }
      wxASSERT(mNValidBuffers < 2 || mBuffers[0].end() == mBuffers[1].start);

      samplePtr buffer = nullptr; // will point into mOverlapBuffer
      auto remaining = len;

      // Possibly get an initial portion that is uncached

      // This may be negative
      const auto initLen =
         mNValidBuffers < 1 ? sampleCount( len )
            : std::min(sampleCount( len ), mBuffers[0].start - start);

      if (initLen > 0) {
         // This might be fetching zeroes between clips
         mOverlapBuffer.Resize(len, format);
         // initLen is not more than len:
         auto sinitLen = initLen.as_size_t();
         if (!mPTrack->GetFloats(
            // See comment below about casting
            reinterpret_cast<float *>(mOverlapBuffer.ptr()),
            start, sinitLen, fillZero, mayThrow))
            return nullptr;
         wxASSERT( sinitLen <= remaining );
         remaining -= sinitLen;
         start += initLen;
         buffer = mOverlapBuffer.ptr() + sinitLen * SAMPLE_SIZE(format);
      }

      // Now satisfy the request from the buffers
      for (int ii = 0; ii < mNValidBuffers && remaining > 0; ++ii) {
         const auto starti = start - mBuffers[ii].start;
         // Treatment of initLen above establishes this loop invariant,
         // and statements below preserve it:
         wxASSERT(starti >= 0);

         // This may be negative
         const auto leni =
            std::min( sampleCount( remaining ), mBuffers[ii].len - starti );
         if (initLen <= 0 && leni == len) {
            // All is contiguous already.  We can completely avoid copying
            // leni is nonnegative, therefore start falls within mBuffers[ii],
            // so starti is bounded between 0 and buffer length
            return mBuffers[ii].data.get() + starti.as_size_t() ;
         }
         else if (leni > 0) {
            // leni is nonnegative, therefore start falls within mBuffers[ii]
            // But we can't satisfy all from one buffer, so copy
            if (!buffer) {
               mOverlapBuffer.Resize(len, format);
               buffer = mOverlapBuffer.ptr();
            }
            // leni is positive and not more than remaining
            const size_t size = sizeof(float) * leni.as_size_t();
            // starti is less than mBuffers[ii].len and nonnegative
            memcpy(buffer, mBuffers[ii].data.get() + starti.as_size_t(), size);
            wxASSERT( leni <= remaining );
            remaining -= leni.as_size_t();
            start += leni;
            buffer += size;
         }
      }

      if (remaining > 0) {
         // Very big request!
         // Fall back to direct fetch
         if (!buffer) {
            mOverlapBuffer.Resize(len, format);
            buffer = mOverlapBuffer.ptr();
         }
         // See comment below about casting
         if (!mPTrack->GetFloats( reinterpret_cast<float*>(buffer),
            start, remaining, fillZero, mayThrow))
            return 0;
      }

      // Overlap buffer was meant for the more general support of sample formats
      // besides float, which explains the cast
      return reinterpret_cast<const float*>(mOverlapBuffer.ptr());
   }
   else {
#if 0
      // Cache works only for float format.
      mOverlapBuffer.Resize(len, format);
      if (mPTrack->Get(mOverlapBuffer.ptr(), format, start, len, fillZero, mayThrow))
         return mOverlapBuffer.ptr();
#else
      // No longer handling other than float format.  Therefore len is 0.
#endif
      return nullptr;
   }
}

void SampleTrackCache::Free()
{
   mBuffers[0].Free();
   mBuffers[1].Free();
   mOverlapBuffer.Free();
   mNValidBuffers = 0;
}
