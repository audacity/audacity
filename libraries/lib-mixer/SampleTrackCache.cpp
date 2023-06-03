/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrackCache.cpp

Paul Licameli split from WaveTrack.cpp

**********************************************************************/
#include "SampleTrackCache.h"
#include "WideSampleSequence.h"
#include <cassert>
#include <cstring>

SampleTrackCache::~SampleTrackCache()
{
}

void SampleTrackCache::SetSequence(
   const std::shared_ptr<const WideSampleSequence> &pSequence)
{
   if (mpSequence != pSequence) {
      if (pSequence) {
         mBufferSize = pSequence->GetMaxBlockSize();
         if (!mpSequence ||
             mpSequence->GetMaxBlockSize() != mBufferSize) {
            Free();
            const auto nChannels = pSequence->NChannels();
            for (size_t ii : {0, 1}) {
               mBuffers[ii].areas.reinit(nChannels, true);
               for (size_t iChannel = 0; iChannel < nChannels; ++iChannel)
                  mBuffers[ii].areas[iChannel] = Floats{ mBufferSize };
            }
         }
      }
      else
         Free();
      mpSequence = pSequence;
      mNValidBuffers = 0;
   }
}

std::pair<const float *, const float *>
SampleTrackCache::GetFloatsWide(size_t nChannels,
   sampleCount start, const size_t len, bool mayThrow)
{
   float *overlaps[2]{};
   assert(nChannels <= GetSequence()->NChannels());
   assert(nChannels <= 2);

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
         0 <= mpSequence->GetBlockStart(start)) {
         // Request is not a total miss but starts before the cache,
         // and there is a clip to fetch from.
         // Not the access pattern for drawing spectrogram or playback,
         // but maybe scrubbing causes this.
         // Move the first buffer into second place, and later
         // refill the first.
         // (This case might be useful when marching backwards through
         // the sequence, as with scrubbing.)
         mBuffers[0] .swap ( mBuffers[1] );
         fillFirst = true;
         fillSecond = false;
         // Cache is not in a consistent state yet
         mNValidBuffers = 0;
      }

      // Refill buffers as needed
      if (fillFirst) {
         const auto start0 = mpSequence->GetBlockStart(start);
         if (start0 >= 0) {
            const auto len0 = mpSequence->GetBestBlockSize(start0);
            assert(len0 <= mBufferSize);
            if (!mpSequence->GetFloats(0, nChannels,
                  mBuffers[0].GetBuffers(), start0, len0,
                  fillZero, mayThrow))
               return { nullptr, nullptr };
            mBuffers[0].start = start0;
            mBuffers[0].len = len0;
            if (!fillSecond &&
                mBuffers[0].end() != mBuffers[1].start)
               fillSecond = true;
            // Keep the partially updated state consistent:
            mNValidBuffers = fillSecond ? 1 : 2;
         }
         else {
            // Request may fall between the clips of a sequence.
            // Invalidate all.  WaveTrack::Get() will return zeroes.
            mNValidBuffers = 0;
            fillSecond = false;
         }
      }
      assert(!fillSecond || mNValidBuffers > 0);
      if (fillSecond) {
         mNValidBuffers = 1;
         const auto end0 = mBuffers[0].end();
         if (end > end0) {
            const auto start1 = mpSequence->GetBlockStart(end0);
            if (start1 == end0) {
               const auto len1 = mpSequence->GetBestBlockSize(start1);
               assert(len1 <= mBufferSize);
               if (!mpSequence->GetFloats(0, nChannels,
                  mBuffers[1].GetBuffers(), start1, len1, fillZero, mayThrow))
                  return { nullptr, nullptr };
               mBuffers[1].start = start1;
               mBuffers[1].len = len1;
               mNValidBuffers = 2;
            }
         }
      }
      assert(mNValidBuffers < 2 || mBuffers[0].end() == mBuffers[1].start);

      samplePtr buffers[2]{}; // will point into mOverlapBuffers
      auto remaining = len;
      const auto resizeOverlapBuffers = [&]{
         for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
            auto &overlapBuffer = mOverlapBuffers[iChannel];
            overlapBuffer.Resize(len, format);
            // See comment below about casting
            overlaps[iChannel] = reinterpret_cast<float*>(overlapBuffer.ptr());
         }
      };
      const auto assignBuffers = [&]{
         size_t iChannel = 0;
         for (; iChannel < nChannels; ++iChannel)
            buffers[iChannel] = reinterpret_cast<samplePtr>(overlaps[iChannel]);
         for (; iChannel < 2; ++iChannel)
            buffers[iChannel] = nullptr;
      };

      // Possibly get an initial portion that is uncached

      // This may be negative
      const auto initLen =
         mNValidBuffers < 1 ? sampleCount( len )
            : std::min(sampleCount( len ), mBuffers[0].start - start);

      if (initLen > 0) {
         // This might be fetching zeroes between clips
         resizeOverlapBuffers();
         // initLen is not more than len:
         auto sinitLen = initLen.as_size_t();
         if (!mpSequence->GetFloats(0, nChannels, overlaps,
            start, sinitLen, fillZero, mayThrow))
            return { nullptr, nullptr };
         assert(sinitLen <= remaining);
         remaining -= sinitLen;
         start += initLen;
         for (size_t iChannel = 0; iChannel < nChannels; ++iChannel)
            buffers[iChannel] =
               reinterpret_cast<samplePtr>(overlaps[iChannel] + sinitLen);
      }

      // Now satisfy the request from the buffers
      for (int ii = 0; ii < mNValidBuffers && remaining > 0; ++ii) {
         const auto starti = start - mBuffers[ii].start;
         // Treatment of initLen above establishes this loop invariant,
         // and statements below preserve it:
         assert(starti >= 0);

         // This may be negative
         const auto leni =
            std::min( sampleCount( remaining ), mBuffers[ii].len - starti );
         if (initLen <= 0 && leni == len) {
            // All is contiguous already.  We can completely avoid copying
            // leni is nonnegative, therefore start falls within mBuffers[ii],
            // so starti is bounded between 0 and buffer length
            return mBuffers[ii].GetResults(nChannels, starti.as_size_t());
         }
         else if (leni > 0) {
            // leni is nonnegative, therefore start falls within mBuffers[ii]
            // But we can't satisfy all from one buffer, so copy
            if (!buffers[0]) {
               resizeOverlapBuffers();
               assignBuffers();
            }
            // leni is positive and not more than remaining
            const size_t size = sizeof(float) * leni.as_size_t();
            // starti is less than mBuffers[ii].len and nonnegative
            for (size_t iChannel = 0; iChannel < nChannels; ++iChannel)
               memcpy(buffers[iChannel],
                  mBuffers[ii].areas[iChannel].get() + starti.as_size_t(),
                  size);
            assert(leni <= remaining);
            remaining -= leni.as_size_t();
            start += leni;
            for (size_t iChannel = 0; iChannel < nChannels; ++iChannel)
               buffers[iChannel] += size;
         }
      }

      if (remaining > 0) {
         // Very big request!
         // Fall back to direct fetch
         if (!buffers[0])
            resizeOverlapBuffers();
         // See comment below about casting
         if (!mpSequence->GetFloats(0, nChannels,
            reinterpret_cast<float**>(buffers),
            start, remaining, fillZero, mayThrow))
            return { nullptr, nullptr };
      }

      return { overlaps[0], overlaps[1] };
   }
   else {
      // No longer handling other than float format.  Therefore len is 0.
      return { nullptr, nullptr };
   }
}

void SampleTrackCache::Free()
{
   mBuffers[0].Free();
   mBuffers[1].Free();
   mOverlapBuffers[0].Free();
   mOverlapBuffers[1].Free();
   mNValidBuffers = 0;
}
