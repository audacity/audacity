/**********************************************************************

  Audacity: A Digital Audio Editor

  @file MixerSource.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

  Paul Licameli split from Mix.cpp

**********************************************************************/
#include "MixerSource.h"

#include "AudioGraphBuffers.h"
#include "Envelope.h"
#include "SampleTrack.h"
#include "SampleTrackCache.h"
#include "Resample.h"
#include "float_cast.h"

namespace {
template<typename T, typename F> std::vector<T>
initVector(size_t dim1, const F &f)
{
   std::vector<T> result( dim1 );
   for (auto &row : result)
      f(row);
   return result;
}

template<typename T> std::vector<std::vector<T>>
initVector(size_t dim1, size_t dim2)
{
   return initVector<std::vector<T>>(dim1,
      [dim2](auto &row){ row.resize(dim2); });
}
}

void MixerSource::MakeResamplers()
{
   for (size_t j = 0; j < mnChannels; ++j)
      mResample[j] = std::make_unique<Resample>(
         mResampleParameters.mHighQuality,
         mResampleParameters.mMinFactor[j], mResampleParameters.mMaxFactor[j]);
}

namespace {
   //Note: The meaning of this function has changed (December 2012)
   //Previously this function did something that was close to the opposite (but not entirely accurate).
   /** @brief Compute the integral warp factor between two non-warped time points
    *
    * Calculate the relative length increase of the chosen segment from the original sound.
    * So if this time track has a low value (i.e. makes the sound slower), the NEW warped
    * sound will be *longer* than the original sound, so the return value of this function
    * is larger.
    * @param t0 The starting time to calculate from
    * @param t1 The ending time to calculate to
    * @return The relative length increase of the chosen segment from the original sound.
    */
double ComputeWarpFactor(const Envelope &env, double t0, double t1)
{
   return env.AverageOfInverse(t0, t1);
}

}

size_t MixerSource::MixVariableRates(
   unsigned iChannel, const size_t maxOut, float &floatBuffer)
{
   auto &cache = mInputTrack[iChannel];
   const auto pos = &mSamplePos[iChannel];
   const auto queue = mSampleQueue[iChannel].data();
   const auto queueStart = &mQueueStart[iChannel];
   const auto queueLen = &mQueueLen[iChannel];
   const auto pResample = mResample[iChannel].get();

   const auto pFloat = &floatBuffer;
   const auto track = cache.GetTrack().get();
   const double trackRate = track->GetRate();
   const auto &[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;
   const double initialWarp = mRate / mSpeed / trackRate;
   const double tstep = 1.0 / trackRate;
   auto sampleSize = SAMPLE_SIZE(floatSample);

   size_t out = 0;

   /* time is floating point. Sample rate is integer. The number of samples
    * has to be integer, but the multiplication gives a float result, which we
    * round to get an integer result. TODO: is this always right or can it be
    * off by one sometimes? Can we not get this information directly from the
    * clip (which must know) rather than convert the time?
    *
    * LLL:  Not at this time.  While WaveClips provide methods to retrieve the
    *       start and end sample, they do the same float->sampleCount conversion
    *       to calculate the position.
    */

   // Find the last sample
   double endTime = track->GetEndTime();
   double startTime = track->GetStartTime();
   const bool backwards = (mT1 < mT0);
   const double tEnd = backwards
      ? std::max(startTime, mT1)
      : std::min(endTime, mT1);
   const auto endPos = track->TimeToLongSamples(tEnd);
   // Find the time corresponding to the start of the queue, for use with time track
   double t = ((*pos).as_long_long() +
               (backwards ? *queueLen : - *queueLen)) / trackRate;

   while (out < maxOut) {
      if (*queueLen < (int)sProcessLen) {
         // Shift pending portion to start of the buffer
         memmove(queue, &queue[*queueStart], (*queueLen) * sampleSize);
         *queueStart = 0;

         auto getLen = limitSampleBufferSize(
            sQueueMaxLen - *queueLen,
            backwards ? *pos - endPos : endPos - *pos
         );

         // Nothing to do if past end of play interval
         if (getLen > 0) {
            if (backwards) {
               auto results =
                  cache.GetFloats(*pos - (getLen - 1), getLen, mMayThrow);
               if (results)
                  memcpy(&queue[*queueLen], results, sizeof(float) * getLen);
               else
                  memset(&queue[*queueLen], 0, sizeof(float) * getLen);

               track->GetEnvelopeValues(mEnvValues.data(),
                  getLen, (*pos - (getLen - 1)).as_double() / trackRate);
               *pos -= getLen;
            }
            else {
               auto results = cache.GetFloats(*pos, getLen, mMayThrow);
               if (results)
                  memcpy(&queue[*queueLen], results, sizeof(float) * getLen);
               else
                  memset(&queue[*queueLen], 0, sizeof(float) * getLen);

               track->GetEnvelopeValues(mEnvValues.data(),
                  getLen, (*pos).as_double() / trackRate);

               *pos += getLen;
            }

            for (decltype(getLen) i = 0; i < getLen; i++) {
               queue[(*queueLen) + i] *= mEnvValues[i];
            }

            if (backwards)
               ReverseSamples((samplePtr)&queue[0], floatSample,
                              *queueLen, getLen);

            *queueLen += getLen;
         }
      }

      auto thisProcessLen = sProcessLen;
      bool last = (*queueLen < (int)sProcessLen);
      if (last) {
         thisProcessLen = *queueLen;
      }

      double factor = initialWarp;
      if (mEnvelope)
      {
         //TODO-MB: The end time is wrong when the resampler doesn't use all input samples,
         //         as a result of this the warp factor may be slightly wrong, so AudioIO will stop too soon
         //         or too late (resulting in missing sound or inserted silence). This can't be fixed
         //         without changing the way the resampler works, because the number of input samples that will be used
         //         is unpredictable. Maybe it can be compensated later though.
         if (backwards)
            factor *= ComputeWarpFactor( *mEnvelope,
               t - (double)thisProcessLen / trackRate + tstep, t + tstep);
         else
            factor *= ComputeWarpFactor( *mEnvelope,
               t, t + (double)thisProcessLen / trackRate);
      }

      auto results = pResample->Process(factor,
         &queue[*queueStart],
         thisProcessLen,
         last,
         // PRL:  Bug2536: crash in soxr happened on Mac, sometimes, when
         // maxOut - out == 1 and &pFloat[out + 1] was an unmapped
         // address, because soxr, strangely, fetched an 8-byte (misaligned!)
         // value from &pFloat[out], but did nothing with it anyway,
         // in soxr_output_no_callback.
         // Now we make the bug go away by allocating a little more space in
         // the buffer than we need.
         &pFloat[out],
         maxOut - out);

      const auto input_used = results.first;
      *queueStart += input_used;
      *queueLen -= input_used;
      out += results.second;
      t += (input_used / trackRate) * (backwards ? -1 : 1);

      if (last) {
         break;
      }
   }

   assert(out <= maxOut);
   return out;
}

size_t MixerSource::MixSameRate(unsigned iChannel, const size_t maxOut,
   float &floatBuffer)
{
   // This function fetches samples from the input tracks, whatever their
   // formats, as floats; it may also apply envelope values.

   auto &cache = mInputTrack[iChannel];
   const auto pos = &mSamplePos[iChannel];

   const auto pFloat = &floatBuffer;
   const auto track = cache.GetTrack().get();
   const double t = ( *pos ).as_double() / track->GetRate();
   const double trackEndTime = track->GetEndTime();
   const double trackStartTime = track->GetStartTime();
   const auto &[mT0, mT1, _, __] = *mTimesAndSpeed;
   const bool backwards = (mT1 < mT0);
   const double tEnd = backwards
      ? std::max(trackStartTime, mT1)
      : std::min(trackEndTime, mT1);

   //don't process if we're at the end of the selection or track.
   if ((backwards ? t <= tEnd : t >= tEnd))
      return 0;
   //if we're about to approach the end of the track or selection, figure out how much we need to grab
   const auto slen = limitSampleBufferSize(
      maxOut,
      // PRL: maybe t and tEnd should be given as sampleCount instead to
      // avoid trouble subtracting one large value from another for a small
      // difference
      sampleCount{ (backwards ? t - tEnd : tEnd - t) * track->GetRate() + 0.5 }
   );

   if (backwards) {
      auto results = cache.GetFloats(*pos - (slen - 1), slen, mMayThrow);
      if (results)
         memcpy(pFloat, results, sizeof(float) * slen);
      else
         memset(pFloat, 0, sizeof(float) * slen);
      track->GetEnvelopeValues(mEnvValues.data(), slen, t - (slen - 1) / mRate);
      for (size_t i = 0; i < slen; i++)
         pFloat[i] *= mEnvValues[i]; // Track gain control will go here?
      ReverseSamples((samplePtr)pFloat, floatSample, 0, slen);

      *pos -= slen;
   }
   else {
      auto results = cache.GetFloats(*pos, slen, mMayThrow);
      if (results)
         memcpy(pFloat, results, sizeof(float) * slen);
      else
         memset(pFloat, 0, sizeof(float) * slen);
      track->GetEnvelopeValues(mEnvValues.data(), slen, t);
      for (size_t i = 0; i < slen; i++)
         pFloat[i] *= mEnvValues[i]; // Track gain control will go here?

      *pos += slen;
   }

   assert(slen <= maxOut);
   return slen;
}

void MixerSource::ZeroFill(
   size_t produced, size_t max, float &floatBuffer)
{
   assert(produced <= max);
   const auto pFloat = &floatBuffer;
   std::fill(pFloat + produced, pFloat + max, 0);
}

MixerSource::MixerSource(const SampleTrack &leader, size_t bufferSize,
   double rate, const MixerOptions::Warp &options, bool highQuality,
   bool mayThrow, std::shared_ptr<TimesAndSpeed> pTimesAndSpeed,
   const ArrayOf<bool> *pMap
)  : mpLeader{ leader.SharedPointer<const SampleTrack>() }
   , mnChannels{ TrackList::Channels(&leader).size() }
   , mRate{ rate }
   , mEnvelope{ options.envelope }
   , mMayThrow{ mayThrow }
   , mTimesAndSpeed{ move(pTimesAndSpeed) }
   , mInputTrack( mnChannels )
   , mSamplePos( mnChannels )
   , mSampleQueue{ initVector<float>(mnChannels, sQueueMaxLen) }
   , mQueueStart( mnChannels, 0 )
   , mQueueLen( mnChannels, 0 )
   , mResampleParameters{ highQuality, leader, rate, options }
   , mResample( mnChannels )
   , mEnvValues( std::max(sQueueMaxLen, bufferSize) )
   , mpMap{ pMap }
{
   size_t j = 0;
   for (auto channel : TrackList::Channels(&leader))
      mInputTrack[j++].SetTrack(channel->SharedPointer<const SampleTrack>());

   assert(mTimesAndSpeed);
   auto t0 = mTimesAndSpeed->mT0;
   for (j = 0; j < mnChannels; ++j) {
      mSamplePos[j] = GetChannel(j)->TimeToLongSamples(t0);
   }
   MakeResamplers();
}

MixerSource::~MixerSource() = default;

const SampleTrack *MixerSource::GetChannel(unsigned iChannel) const
{
   auto range = TrackList::Channels(mpLeader.get());
   auto iter = range.begin();
   std::advance(iter, iChannel);
   return *iter;
}

const bool *MixerSource::MixerSpec(unsigned iChannel) const
{
   return mpMap ? mpMap[iChannel].get() : nullptr;
}

bool MixerSource::AcceptsBuffers(const Buffers &buffers) const
{
   return AcceptsBlockSize(buffers.BufferSize());
}

bool MixerSource::AcceptsBlockSize(size_t blockSize) const
{
   return blockSize <= mEnvValues.size();
}

#define stackAllocate(T, count) static_cast<T*>(alloca(count * sizeof(T)))

std::optional<size_t> MixerSource::Acquire(Buffers &data, size_t bound)
{
   assert(AcceptsBuffers(data));
   assert(AcceptsBlockSize(data.BlockSize()));
   assert(bound <= data.BlockSize());
   assert(data.BlockSize() <= data.Remaining());

   auto &[mT0, mT1, _, mTime] = *mTimesAndSpeed;
   const bool backwards = (mT1 < mT0);
   // TODO: more-than-two-channels
   const auto maxChannels = mMaxChannels = data.Channels();
   const auto limit = std::min<size_t>(mnChannels, maxChannels);
   size_t maxTrack = 0;
   const auto mixed = stackAllocate(size_t, maxChannels);
   for (size_t j = 0; j < limit; ++j) {
      const auto pFloat = &data.GetWritePosition(j);
      auto &result = mixed[j];
      const auto track = GetChannel(j);
      result =
      (mResampleParameters.mVariableRates || track->GetRate() != mRate)
         ? MixVariableRates(j, bound, *pFloat)
         : MixSameRate(j, bound, *pFloat);
      maxTrack = std::max(maxTrack, result);
      auto newT = mSamplePos[j].as_double() / track->GetRate();
      if (backwards)
         mTime = std::min(mTime, newT);
      else
         mTime = std::max(mTime, newT);
   }
   // Another pass in case channels of a track did not produce equal numbers
   for (size_t j = 0; j < limit; ++j) {
      const auto pFloat = &data.GetWritePosition(j);
      const auto result = mixed[j];
      ZeroFill(result, maxTrack, *pFloat);
   }

   mLastProduced = maxTrack;
   assert(maxTrack <= bound);
   assert(maxTrack <= data.Remaining());
   assert(maxTrack <= Remaining());
   assert(data.Remaining() > 0);
   assert(bound == 0 || Remaining() == 0 || maxTrack > 0);
   return { mLastProduced };
}

// Does not return a strictly decreasing sequence of values such as to
// provide proof of termination.  Just an indication of whether done or not.
sampleCount MixerSource::Remaining() const
{
   // TODO:  make a more exact calculation of total remaining; see Terminates()
   return mLastProduced;
}

bool MixerSource::Release()
{
   mLastProduced = 0;
   return true;
}

bool MixerSource::Terminates() const
{
   // Not always terminating
   // TODO: return true sometimes, for mixers that never reposition
   // because they are not used in playback.  But then an exact calculation of
   // Remaining() is needed to satisfy the contract, and that is complicated
   // when there is resampling or a time warp.
   return false;
}

void MixerSource::Reposition(double time, bool skipping)
{
   for (size_t j = 0; j < mnChannels; ++j) {
      mSamplePos[j] = GetChannel(j)->TimeToLongSamples(time);
      mQueueStart[j] = 0;
      mQueueLen[j] = 0;
   }

   // Bug 2025:  libsoxr 0.1.3, first used in Audacity 2.3.0, crashes with
   // constant rate resampling if you try to reuse the resampler after it has
   // flushed.  Should that be considered a bug in sox?  This works around it.
   // (See also bug 1887, and the same work around in Mixer::Restart().)
   if (skipping)
      MakeResamplers();
}
