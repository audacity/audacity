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
   unsigned nChannels, const size_t maxOut, float *floatBuffers[])
{
   const auto &[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;
   const bool backwards = (mT1 < mT0);
   size_t result = 0;
   sampleCount newPos = 0;
   int newQueueStart = 0;
   int newQueueLen = 0;

   const auto seq = mInputSequence[0].GetSequence().get();
   const double sequenceRate = seq->GetRate();
   const double initialWarp = mRate / mSpeed / sequenceRate;
   const double tstep = 1.0 / sequenceRate;
   const auto sampleSize = SAMPLE_SIZE(floatSample);
   // Find the last sample
   const auto endPos = [&seq, mT1 = mT1, backwards]{
      double endTime = seq->GetEndTime();
      double startTime = seq->GetStartTime();
      const double tEnd = backwards
         ? std::max(startTime, mT1)
         : std::min(endTime, mT1);
      return seq->TimeToLongSamples(tEnd);
   }();

for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
   auto &cache = mInputSequence[iChannel];
   auto pos = mSamplePos;
   const auto queue = mSampleQueue[iChannel].data();
   auto queueStart = mQueueStart;
   auto queueLen = mQueueLen;
   const auto pResample = mResample[iChannel].get();

   const auto pFloat = floatBuffers[iChannel];
   const auto sequence = cache.GetSequence().get();

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

   // Find the time corresponding to the start of the queue, for use with time track
   double t = ((pos).as_long_long() +
               (backwards ? queueLen : - queueLen)) / sequenceRate;

   while (out < maxOut) {
      if (queueLen < (int)sProcessLen) {
         // Shift pending portion to start of the buffer
         memmove(queue, &queue[queueStart], (queueLen) * sampleSize);
         queueStart = 0;

         // How far to advance depends on endPos,
         // which is independent of channel
         auto getLen = limitSampleBufferSize(
            sQueueMaxLen - queueLen,
            backwards ? pos - endPos : endPos - pos
         );

         // Nothing to do if past end of play interval
         if (getLen > 0) {
            if (backwards) {
               auto results =
                  cache.GetFloats(pos - (getLen - 1), getLen, mMayThrow);
               if (results)
                  memcpy(&queue[queueLen], results, sizeof(float) * getLen);
               else
                  memset(&queue[queueLen], 0, sizeof(float) * getLen);
            }
            else {
               auto results = cache.GetFloats(pos, getLen, mMayThrow);
               if (results)
                  memcpy(&queue[queueLen], results, sizeof(float) * getLen);
               else
                  memset(&queue[queueLen], 0, sizeof(float) * getLen);
            }

            if (backwards) {
               sequence->GetEnvelopeValues(mEnvValues.data(),
                  getLen, (pos - (getLen - 1)).as_double() / sequenceRate);
               pos -= getLen;
            }
            else {
               sequence->GetEnvelopeValues(mEnvValues.data(),
                  getLen, (pos).as_double() / sequenceRate);

               pos += getLen;
            }

            for (decltype(getLen) i = 0; i < getLen; i++) {
               queue[(queueLen) + i] *= mEnvValues[i];
            }

            if (backwards)
               ReverseSamples((samplePtr)&queue[0], floatSample,
                              queueLen, getLen);

            queueLen += getLen;
         }
      }

      auto thisProcessLen = sProcessLen;
      bool last = (queueLen < (int)sProcessLen);
      if (last) {
         thisProcessLen = queueLen;
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
               t - (double)thisProcessLen / sequenceRate + tstep, t + tstep);
         else
            factor *= ComputeWarpFactor( *mEnvelope,
               t, t + (double)thisProcessLen / sequenceRate);
      }

      auto results = pResample->Process(factor,
         &queue[queueStart],
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
      queueStart += input_used;
      queueLen -= input_used;
      out += results.second;
      t += (input_used / sequenceRate) * (backwards ? -1 : 1);

      if (last) {
         break;
      }
   }

   assert(out <= maxOut);

   // If the loop executes repeatedly, these lines make the same assignments
   // each time, because the progress depends only on getLen which depends
   // on endPos.  (Assuming too that progress of the Resampler depends only
   // on the size of its inputs.)
   newPos = pos;
   newQueueStart = queueStart;
   newQueueLen = queueLen;
   result = out;
}
   mSamplePos = newPos;
   mQueueStart = newQueueStart;
   mQueueLen = newQueueLen;
   return result;
}

size_t MixerSource::MixSameRate(unsigned nChannels, const size_t maxOut,
   float *floatBuffers[])
{
   const auto &[mT0, mT1, _, __] = *mTimesAndSpeed;
   const bool backwards = (mT1 < mT0);
   size_t result = 0;
   auto newPos = backwards ? sampleCount::min() : sampleCount::max();
   const auto seq = mInputSequence[0].GetSequence().get();
   const auto sequenceRate = seq->GetRate();
   const double tEnd = [seq, mT1 = mT1, backwards]{
      const double sequenceEndTime = seq->GetEndTime();
      const double sequenceStartTime = seq->GetStartTime();
      return backwards
         ? std::max(sequenceStartTime, mT1)
         : std::min(sequenceEndTime, mT1);
   }();
for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
   // This function fetches samples from the input sequences, whatever their
   // formats, as floats; it may also apply envelope values.

   auto &cache = mInputSequence[iChannel];
   auto pos = mSamplePos;

   const auto pFloat = floatBuffers[iChannel];
   const auto sequence = cache.GetSequence().get();
   const double t = (pos).as_double() / sequenceRate;

   //don't process if we're at the end of the selection or sequence.
   if ((backwards ? t <= tEnd : t >= tEnd))
      return 0;
   //if we're about to approach the end of the sequence or selection, figure out how much we need to grab
   const auto slen = limitSampleBufferSize(
      maxOut,
      // PRL: maybe t and tEnd should be given as sampleCount instead to
      // avoid trouble subtracting one large value from another for a small
      // difference
      sampleCount{ (backwards ? t - tEnd : tEnd - t) * sequenceRate + 0.5 }
   );

   if (backwards) {
      auto results = cache.GetFloats(pos - (slen - 1), slen, mMayThrow);
      if (results)
         memcpy(pFloat, results, sizeof(float) * slen);
      else
         memset(pFloat, 0, sizeof(float) * slen);
   }
   else {
      auto results = cache.GetFloats(pos, slen, mMayThrow);
      if (results)
         memcpy(pFloat, results, sizeof(float) * slen);
      else
         memset(pFloat, 0, sizeof(float) * slen);
   }

   if (backwards) {
      sequence->GetEnvelopeValues(mEnvValues.data(), slen, t - (slen - 1) / mRate);
      for (size_t i = 0; i < slen; i++)
         pFloat[i] *= mEnvValues[i]; // Track gain control will go here?
      ReverseSamples((samplePtr)pFloat, floatSample, 0, slen);

      pos -= slen;
   }
   else {
      sequence->GetEnvelopeValues(mEnvValues.data(), slen, t);
      for (size_t i = 0; i < slen; i++)
         pFloat[i] *= mEnvValues[i]; // Track gain control will go here?

      pos += slen;
   }

   assert(slen <= maxOut);
   // If the loop executes repeatedly, these lines make the same assignments
   // each time, because the progress depends only on tEnd
   newPos = pos;
   result = slen;
}
   mSamplePos = newPos;
   return result;
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
   , mnChannels{ TrackList::NChannels(leader) }
   , mRate{ rate }
   , mEnvelope{ options.envelope }
   , mMayThrow{ mayThrow }
   , mTimesAndSpeed{ move(pTimesAndSpeed) }
   , mInputSequence( mnChannels )
   , mSampleQueue{ initVector<float>(mnChannels, sQueueMaxLen) }
   , mQueueStart{ 0 }
   , mQueueLen{ 0 }
   , mResampleParameters{ highQuality, leader, rate, options }
   , mResample( mnChannels )
   , mEnvValues( std::max(sQueueMaxLen, bufferSize) )
   , mpMap{ pMap }
{
   size_t j = 0;
   for (auto channel : TrackList::Channels(&leader))
      mInputSequence[j++]
         .SetSequence(channel->SharedPointer<const SampleTrack>());

   assert(mTimesAndSpeed);
   auto t0 = mTimesAndSpeed->mT0;
   mSamplePos = GetChannel(0)->TimeToLongSamples(t0);
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
   const auto pFloats = stackAllocate(float *, limit);
   for (size_t j = 0; j < limit; ++j)
      pFloats[j] = &data.GetWritePosition(j);
   const auto rate = GetChannel(0)->GetRate();
   auto result = (mResampleParameters.mVariableRates || rate != mRate)
      ? MixVariableRates(limit, bound, pFloats)
      : MixSameRate(limit, bound, pFloats);
   maxTrack = std::max(maxTrack, result);
   auto newT = mSamplePos.as_double() / rate;
   if (backwards)
      mTime = std::min(mTime, newT);
   else
      mTime = std::max(mTime, newT);
   for (size_t j = 0; j < limit; ++j) {
      mixed[j] = result;
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
   mSamplePos = GetChannel(0)->TimeToLongSamples(time);

   // Bug 2025:  libsoxr 0.1.3, first used in Audacity 2.3.0, crashes with
   // constant rate resampling if you try to reuse the resampler after it has
   // flushed.  Should that be considered a bug in sox?  This works around it.
   // (See also bug 1887, and the same work around in Mixer::Restart().)
   if (skipping)
      MakeResamplers();
}
