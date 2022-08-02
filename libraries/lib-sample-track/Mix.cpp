/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

*******************************************************************//**

\class Mixer
\brief Functions for doing the mixdown of the tracks.

*//*******************************************************************/


#include "Mix.h"

#include <algorithm>
#include <cmath>

#include "Envelope.h"
#include "SampleTrack.h"
#include "SampleTrackCache.h"
#include "Prefs.h"
#include "Resample.h"
#include "float_cast.h"

MixerOptions::Warp::Warp(const TrackList &list)
: envelope(DefaultWarp::Call(list)), minSpeed(0.0), maxSpeed(0.0)
{
}

MixerOptions::Warp::Warp(const BoundedEnvelope *e)
    : envelope(e), minSpeed(0.0), maxSpeed(0.0)
 {}

MixerOptions::Warp::Warp(double min, double max, double initial)
   : minSpeed{ std::max(0.0, std::min(min, max)) }
   , maxSpeed{ std::max(0.0, std::max(min, max)) }
   , initialSpeed{initial}
{
   assert(min >= 0);
   assert(max >= 0);
   assert(min <= max);
}

MixerOptions::ResampleParameters::ResampleParameters(
   const SampleTrackConstArray &inputTracks, double rate, const Warp &options)
{
   mMinFactor.reserve(inputTracks.size());
   mMaxFactor.reserve(inputTracks.size());
   for (const auto &pTrack : inputTracks) {
      double factor = (rate / pTrack->GetRate());
      if (const auto envelope = options.envelope) {
         // variable rate resampling
         mbVariableRates = true;
         mMinFactor.push_back(factor / envelope->GetRangeUpper());
         mMaxFactor.push_back(factor / envelope->GetRangeLower());
      }
      else if (options.minSpeed > 0.0 && options.maxSpeed > 0.0) {
         // variable rate resampling
         mbVariableRates = true;
         mMinFactor.push_back(factor / options.maxSpeed);
         mMaxFactor.push_back(factor / options.minSpeed);
      }
      else {
         // constant rate resampling
         mbVariableRates = false;
         mMinFactor.push_back(factor);
         mMaxFactor.push_back(factor);
      }
   }
}

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

Mixer::Mixer(const SampleTrackConstArray &inputTracks,
   const bool mayThrow,
   const WarpOptions &warpOptions,
   const double startTime, const double stopTime,
   const unsigned numOutChannels,
   const size_t outBufferSize, const bool outInterleaved,
   double outRate, sampleFormat outFormat,
   const bool highQuality, MixerSpec *const mixerSpec,
   const bool applyTrackGains
)  : mNumInputTracks{ inputTracks.size() }
   , mNumChannels{ numOutChannels }

   , mBufferSize{ outBufferSize }
   , mRate{ outRate }
   , mEnvelope{ warpOptions.envelope }
   , mResampleParameters{ inputTracks, mRate, warpOptions }

   , mApplyTrackGains{ applyTrackGains }
   , mMixerSpec{
      ( mixerSpec && mixerSpec->GetNumChannels() == mNumChannels &&
         mixerSpec->GetNumTracks() == mNumInputTracks
      )  ? mixerSpec
         : nullptr
   }
   , mHighQuality{ highQuality }
   , mFormat{ outFormat }
   , mInterleaved{ outInterleaved }

   , mMayThrow{ mayThrow }

   , mInputTrack( mNumInputTracks )
   , mTimesAndSpeed{ std::make_shared<TimesAndSpeed>( TimesAndSpeed{
      startTime, stopTime, warpOptions.initialSpeed, startTime
   } ) }

   // PRL:  Bug2536: see other comments below for the last, padding argument
   // TODO: more-than-two-channels
   , mFloatBuffers{ 2, mBufferSize, 1, 1 }

   // non-interleaved
   , mTemp{ initVector<float>(mNumChannels, mBufferSize) }
   , mBuffer{ initVector<SampleBuffer>(mInterleaved ? 1 : mNumChannels,
      [format = mFormat,
         size = mBufferSize * (mInterleaved ? mNumChannels : 1)
      ](auto &buffer){ buffer.Allocate(size, format); }
   )}

   , mResample( mNumInputTracks )
{
   for (size_t i = 0; i < mNumInputTracks; ++i)
      mInputTrack[i].SetTrack(inputTracks[i]);

   MakeResamplers();
   assert(BufferSize() == outBufferSize);

   for (size_t i = 0; i < mNumInputTracks;) {
      const auto leader = inputTracks[i].get();
      const auto nInChannels = TrackList::Channels(leader).size();
      if (!leader || i + nInChannels > mNumInputTracks) {
         assert(false);
         break;
      }
      auto increment = finally([&]{ i += nInChannels; });

      mSources.emplace_back( *leader, i, BufferSize(), mRate,
         mResampleParameters.mbVariableRates, mEnvelope, mMayThrow
         , mTimesAndSpeed
         , mInputTrack, mResample
         , mMixerSpec ? &mMixerSpec->mMap[i] : nullptr
      );
   }
}

Mixer::~Mixer()
{
}

void Mixer::MakeResamplers()
{
   for (size_t i = 0; i < mNumInputTracks; i++)
      mResample[i] = std::make_unique<Resample>(mHighQuality,
         mResampleParameters.mMinFactor[i], mResampleParameters.mMaxFactor[i]);
}

void Mixer::Clear()
{
   for (auto &buffer: mTemp)
      std::fill(buffer.begin(), buffer.end(), 0);
}

static void MixBuffers(unsigned numChannels,
   const unsigned char *channelFlags, const float *gains,
   const float &src, std::vector<std::vector<float>> &dests, int len)
{
   const auto pSrc = &src;
   for (unsigned int c = 0; c < numChannels; c++) {
      if (!channelFlags[c])
         continue;
      float *dest = dests[c].data();
      float gain = gains[c];
      for (int j = 0; j < len; ++j)
         *dest++ += pSrc[j] * gain;   // the actual mixing process
   }
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
   const auto ii = i + iChannel;

   auto &cache = mInputTrack[ii];
   const auto pos = &mSamplePos[iChannel];
   const auto queue = mSampleQueue[iChannel].data();
   const auto queueStart = &mQueueStart[iChannel];
   const auto queueLen = &mQueueLen[iChannel];
   const auto pResample = mResample[ii].get();

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
   const auto ii = i + iChannel;

   auto &cache = mInputTrack[ii];
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

MixerSource::MixerSource(const SampleTrack &leader, size_t i,
   size_t bufferSize,
   double rate, bool variableRates,
   const BoundedEnvelope *const pEnvelope,
   bool mayThrow
   , std::shared_ptr<TimesAndSpeed> pTimesAndSpeed
   , std::vector<SampleTrackCache> &mInputTrack
   , std::vector<std::unique_ptr<Resample>> &mResample
   , const ArrayOf<bool> *pMap
)  : mpLeader{ leader.SharedPointer<const SampleTrack>() }
   , i{ i }
   , mnChannels{ TrackList::Channels(&leader).size() }
   , mRate{ rate }
   , mVariableRates{ variableRates }
   , mEnvelope{ pEnvelope }
   , mMayThrow{ mayThrow }
   , mTimesAndSpeed{ move(pTimesAndSpeed) }
   , mInputTrack{ mInputTrack }
   , mSamplePos( mnChannels )
   , mSampleQueue{ initVector<float>(mnChannels, sQueueMaxLen) }
   , mQueueStart( mnChannels, 0 )
   , mQueueLen( mnChannels, 0 )
   , mResample{ mResample }
   , mEnvValues( std::max(sQueueMaxLen, bufferSize) )
   , mpMap{ pMap }
{
   assert(mTimesAndSpeed);
   auto t0 = mTimesAndSpeed->mT0;
   for (size_t j = 0; j < mnChannels; ++j)
      mSamplePos[j] = GetChannel(j)->TimeToLongSamples(t0);
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
   auto &[mT0, mT1, _, mTime] = *mTimesAndSpeed;
   const bool backwards = (mT1 < mT0);
   // TODO: more-than-two-channels
   const auto maxChannels = data.Channels();
   const auto limit = std::min<size_t>(mnChannels, maxChannels);
   size_t maxTrack = 0;
   const auto mixed = stackAllocate(size_t, maxChannels);
   for (size_t j = 0; j < limit; ++j) {
      const auto pFloat = &data.GetWritePosition(j);
      auto &result = mixed[j];
      const auto track = GetChannel(j);
      result =
      (mVariableRates || track->GetRate() != mRate)
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
   return { maxTrack };
}

sampleCount MixerSource::Remaining() const
{
   return { 1 };
}

bool MixerSource::Release()
{
   return true;
}

size_t Mixer::Process(const size_t maxToProcess)
{
   assert(maxToProcess <= BufferSize());

   // MB: this is wrong! mT represented warped time, and mTime is too inaccurate to use
   // it here. It's also unnecessary I think.
   //if (mT >= mT1)
   //   return 0;

   size_t maxOut = 0;
   const auto channelFlags = stackAllocate(unsigned char, mNumChannels);
   const auto gains = stackAllocate(float, mNumChannels);
   if (!mApplyTrackGains)
      std::fill(gains, gains + mNumChannels, 1.0f);

   // Decides which output buffers an input channel accumulates into
   auto findChannelFlags = [&channelFlags, numChannels = mNumChannels]
   (const bool *map, Track::ChannelType channel){
      const auto end = channelFlags + numChannels;
      std::fill(channelFlags, end, 0);
      if (map)
         // ignore left and right when downmixing is customized
         std::copy(map, map + numChannels, channelFlags);
      else switch(channel) {
      case Track::MonoChannel:
      default:
         std::fill(channelFlags, end, 1);
         break;
      case Track::LeftChannel:
         channelFlags[0] = 1;
         break;
      case Track::RightChannel:
         if (numChannels >= 2)
            channelFlags[1] = 1;
         else
            channelFlags[0] = 1;
         break;
      }
      return channelFlags;
   };

   auto &[mT0, mT1, _, mTime] = *mTimesAndSpeed;
   auto oldTime = mTime;
   // backwards (as possibly in scrubbing)
   const auto backwards = (mT0 > mT1);

   Clear();
   // TODO: more-than-two-channels
   auto maxChannels = mFloatBuffers.Channels();

   for (auto &source : mSources) {
      auto oResult = source.Acquire(mFloatBuffers, maxToProcess);
      if (!oResult)
         return 0;
      maxOut = std::max(maxOut, *oResult);

      // Insert effect stages here!  Passing them all channels of the track

      const auto limit = std::min<size_t>(source.Channels(), maxChannels);
      for (size_t j = 0; j < limit; ++j) {
         const auto pFloat = (const float *)mFloatBuffers.GetReadPosition(j);
         const auto track = source.GetChannel(j);
         if (mApplyTrackGains)
            for (size_t c = 0; c < mNumChannels; ++c)
               gains[c] = track->GetChannelGain(c);
         const auto flags =
            findChannelFlags(source.MixerSpec(j), track->GetChannel());
         MixBuffers(mNumChannels,
            flags, gains, *pFloat, mTemp, *oResult);
      }
   }

   if (backwards)
      mTime = std::clamp(mTime, mT1, oldTime);
   else
      mTime = std::clamp(mTime, oldTime, mT1);

   const auto dstStride = (mInterleaved ? mNumChannels : 1);
   for (size_t c = 0; c < mNumChannels; ++c)
      CopySamples((constSamplePtr)mTemp[c].data(), floatSample,
         (mInterleaved
            ? mBuffer[0].ptr() + (c * SAMPLE_SIZE(mFormat))
            : mBuffer[c].ptr()
         ),
         mFormat, maxOut,
         mHighQuality ? gHighQualityDither : gLowQualityDither,
         1, dstStride);

   // MB: this doesn't take warping into account, replaced with code based on mSamplePos
   //mT += (maxOut / mRate);

   assert(maxOut <= maxToProcess);
   return maxOut;
}

constSamplePtr Mixer::GetBuffer()
{
   return mBuffer[0].ptr();
}

constSamplePtr Mixer::GetBuffer(int channel)
{
   return mBuffer[channel].ptr();
}

double Mixer::MixGetCurrentTime()
{
   return mTimesAndSpeed->mTime;
}

#if 0
// Was used before 3.1.0 whenever looping play restarted
// No longer used
void Mixer::Restart()
{
   mTime = mT0;

   for(size_t i=0; i<mNumInputTracks; i++)
      mSamplePos[i] = mInputTrack[i].GetTrack()->TimeToLongSamples(mT0);

   for(size_t i=0; i<mNumInputTracks; i++) {
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }

   // Bug 1887:  libsoxr 0.1.3, first used in Audacity 2.3.0, crashes with
   // constant rate resampling if you try to reuse the resampler after it has
   // flushed.  Should that be considered a bug in sox?  This works around it:
   MakeResamplers();
}
#endif

void MixerSource::Reposition(double time)
{
   for (size_t j = 0; j < mnChannels; ++j) {
      mSamplePos[j] = GetChannel(j)->TimeToLongSamples(time);
      mQueueStart[j] = 0;
      mQueueLen[j] = 0;
   }
}

void Mixer::Reposition(double t, bool bSkipping)
{
   auto &[mT0, mT1, _, mTime] = *mTimesAndSpeed;
   mTime = t;
   const bool backwards = (mT1 < mT0);
   if (backwards)
      mTime = std::clamp(mTime, mT1, mT0);
   else
      mTime = std::clamp(mTime, mT0, mT1);

   for (auto &source : mSources)
      source.Reposition(mTime);

   // Bug 2025:  libsoxr 0.1.3, first used in Audacity 2.3.0, crashes with
   // constant rate resampling if you try to reuse the resampler after it has
   // flushed.  Should that be considered a bug in sox?  This works around it.
   // (See also bug 1887, and the same work around in Mixer::Restart().)
   if( bSkipping )
      MakeResamplers();
}

void Mixer::SetTimesAndSpeed(double t0, double t1, double speed, bool bSkipping)
{
   wxASSERT(std::isfinite(speed));
   auto &[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;
   mT0 = t0;
   mT1 = t1;
   mSpeed = fabs(speed);
   Reposition(t0, bSkipping);
}

void Mixer::SetSpeedForKeyboardScrubbing(double speed, double startTime)
{
   wxASSERT(std::isfinite(speed));
   auto &[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;

   // Check if the direction has changed
   if ((speed > 0.0 && mT1 < mT0) || (speed < 0.0 && mT1 > mT0)) {
      // It's safe to use 0 and std::numeric_limits<double>::max(),
      // because Mixer::MixVariableRates() doesn't sample past the start
      // or end of the audio in a track.
      if (speed > 0.0 && mT1 < mT0) {
         mT0 = 0;
         mT1 = std::numeric_limits<double>::max();
      }
      else {
         mT0 = std::numeric_limits<double>::max();
         mT1 = 0;
      }

      Reposition(startTime, true);
   }

   mSpeed = fabs(speed);
}

MixerOptions::Downmix::Downmix(unsigned numTracks, unsigned maxNumChannels)
{
   mNumTracks = mNumChannels = numTracks;
   mMaxNumChannels = maxNumChannels;

   if( mNumChannels > mMaxNumChannels )
         mNumChannels = mMaxNumChannels;

   Alloc();

   for( unsigned int i = 0; i < mNumTracks; i++ )
      for( unsigned int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = ( i == j );
}

MixerOptions::Downmix::Downmix(const Downmix &mixerSpec)
{
   mNumTracks = mixerSpec.mNumTracks;
   mMaxNumChannels = mixerSpec.mMaxNumChannels;
   mNumChannels = mixerSpec.mNumChannels;

   Alloc();

   for( unsigned int i = 0; i < mNumTracks; i++ )
      for( unsigned int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];
}

void MixerOptions::Downmix::Alloc()
{
   mMap.reinit(mNumTracks, mMaxNumChannels);
}

MixerOptions::Downmix::~Downmix()
{
}

bool MixerOptions::Downmix::SetNumChannels(unsigned newNumChannels)
{
   if( mNumChannels == newNumChannels )
      return true;

   if( newNumChannels > mMaxNumChannels )
      return false;

   for( unsigned int i = 0; i < mNumTracks; i++ )
   {
      for( unsigned int j = newNumChannels; j < mNumChannels; j++ )
         mMap[ i ][ j ] = false;

      for( unsigned int j = mNumChannels; j < newNumChannels; j++ )
         mMap[ i ][ j ] = false;
   }

   mNumChannels = newNumChannels;
   return true;
}

auto MixerOptions::Downmix::operator=(const Downmix &mixerSpec) -> Downmix &
{
   mNumTracks = mixerSpec.mNumTracks;
   mNumChannels = mixerSpec.mNumChannels;
   mMaxNumChannels = mixerSpec.mMaxNumChannels;

   Alloc();

   for( unsigned int i = 0; i < mNumTracks; i++ )
      for( unsigned int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];

   return *this;
}
