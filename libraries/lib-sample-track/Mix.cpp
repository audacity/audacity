/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

*******************************************************************//**

\class Mixer
\brief Functions for doing the mixdown of the tracks.

*//****************************************************************//**

\class MixerSpec
\brief Class used with Mixer.

*//*******************************************************************/


#include "Mix.h"

#include <cmath>

#include "Envelope.h"
#include "SampleTrack.h"
#include "SampleTrackCache.h"
#include "Prefs.h"
#include "Resample.h"
#include "float_cast.h"

Mixer::WarpOptions::WarpOptions(const TrackList &list)
: envelope(DefaultWarp::Call(list)), minSpeed(0.0), maxSpeed(0.0)
{
}

Mixer::WarpOptions::WarpOptions(const BoundedEnvelope *e)
    : envelope(e), minSpeed(0.0), maxSpeed(0.0)
 {}

Mixer::WarpOptions::WarpOptions(double min, double max, double initial)
   : minSpeed{min}, maxSpeed{max}, initialSpeed{initial}
{
   if (minSpeed < 0)
   {
      wxASSERT(false);
      minSpeed = 0;
   }
   if (maxSpeed < 0)
   {
      wxASSERT(false);
      maxSpeed = 0;
   }
   if (minSpeed > maxSpeed)
   {
      wxASSERT(false);
      std::swap(minSpeed, maxSpeed);
   }
}

Mixer::Mixer(const SampleTrackConstArray &inputTracks,
             bool mayThrow,
             const WarpOptions &warpOptions,
             double startTime, double stopTime,
             unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
             double outRate, sampleFormat outFormat,
             bool highQuality, MixerSpec *mixerSpec, bool applyTrackGains)
   : mNumInputTracks { inputTracks.size() }

   , mApplyTrackGains{ applyTrackGains }

   // This is the number of samples grabbed in one go from a track
   // and placed in a queue, when mixing with resampling.
   // (Should we use SampleTrack::GetBestBlockSize instead?)
   , mQueueMaxLen{ 65536 }
   , mSampleQueue{ mNumInputTracks, mQueueMaxLen }

   , mNumChannels{ numOutChannels }
   , mGains{ mNumChannels }

   , mFormat{ outFormat }
   , mRate{ outRate }

   , mMayThrow{ mayThrow }
{
   mHighQuality = highQuality;
   mInputTrack.reinit(mNumInputTracks);

   // mSamplePos holds for each track the next sample position not
   // yet processed.
   mSamplePos.reinit(mNumInputTracks);
   for(size_t i=0; i<mNumInputTracks; i++) {
      mInputTrack[i].SetTrack(inputTracks[i]);
      mSamplePos[i] = inputTracks[i]->TimeToLongSamples(startTime);
   }
   mEnvelope = warpOptions.envelope;
   mT0 = startTime;
   mT1 = stopTime;
   mTime = startTime;
   mBufferSize = outBufferSize;
   mInterleaved = outInterleaved;
   mSpeed = warpOptions.initialSpeed;
   if( mixerSpec && mixerSpec->GetNumChannels() == mNumChannels &&
         mixerSpec->GetNumTracks() == mNumInputTracks )
      mMixerSpec = mixerSpec;
   else
      mMixerSpec = NULL;

   if (mInterleaved) {
      mNumBuffers = 1;
      mInterleavedBufferSize = mBufferSize * mNumChannels;
   }
   else {
      mNumBuffers = mNumChannels;
      mInterleavedBufferSize = mBufferSize;
   }

   mBuffer.reinit(mNumBuffers);
   mTemp.reinit(mNumBuffers);
   for (unsigned int c = 0; c < mNumBuffers; c++) {
      mBuffer[c].Allocate(mInterleavedBufferSize, mFormat);
      mTemp[c].reinit(mInterleavedBufferSize);
   }
   // PRL:  Bug2536: see other comments below
   mFloatBuffer = Floats{ mInterleavedBufferSize + 1 };

   // But cut the queue into blocks of this finer size
   // for variable rate resampling.  Each block is resampled at some
   // constant rate.
   mProcessLen = 1024;

   // Position in each queue of the start of the next block to resample.
   mQueueStart.reinit(mNumInputTracks);

   // For each queue, the number of available samples after the queue start.
   mQueueLen.reinit(mNumInputTracks);
   mResample.reinit(mNumInputTracks);
   mMinFactor.resize(mNumInputTracks);
   mMaxFactor.resize(mNumInputTracks);
   for (size_t i = 0; i<mNumInputTracks; i++) {
      double factor = (mRate / mInputTrack[i].GetTrack()->GetRate());
      if (mEnvelope) {
         // variable rate resampling
         mbVariableRates = true;
         mMinFactor[i] = factor / mEnvelope->GetRangeUpper();
         mMaxFactor[i] = factor / mEnvelope->GetRangeLower();
      }
      else if (warpOptions.minSpeed > 0.0 && warpOptions.maxSpeed > 0.0) {
         // variable rate resampling
         mbVariableRates = true;
         mMinFactor[i] = factor / warpOptions.maxSpeed;
         mMaxFactor[i] = factor / warpOptions.minSpeed;
      }
      else {
         // constant rate resampling
         mbVariableRates = false;
         mMinFactor[i] = mMaxFactor[i] = factor;
      }

      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }

   MakeResamplers();

   const auto envLen = std::max(mQueueMaxLen, mInterleavedBufferSize);
   mEnvValues.reinit(envLen);
}

Mixer::~Mixer()
{
}

void Mixer::MakeResamplers()
{
   for (size_t i = 0; i < mNumInputTracks; i++)
      mResample[i] = std::make_unique<Resample>(mHighQuality, mMinFactor[i], mMaxFactor[i]);
}

void Mixer::Clear()
{
   for (unsigned int c = 0; c < mNumBuffers; c++) {
      memset(mTemp[c].get(), 0, mInterleavedBufferSize * sizeof(float));
   }
}

static void MixBuffers(unsigned numChannels, int *channelFlags, float *gains,
                const float *src, Floats *dests,
                int len, bool interleaved)
{
   for (unsigned int c = 0; c < numChannels; c++) {
      if (!channelFlags[c])
         continue;

      float *dest;
      unsigned skip;

      if (interleaved) {
         dest = dests[0].get() + c;
         skip = numChannels;
      } else {
         dest = dests[c].get();
         skip = 1;
      }

      float gain = gains[c];
      for (int j = 0; j < len; j++) {
         *dest += src[j] * gain;   // the actual mixing process
         dest += skip;
      }
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

size_t Mixer::MixVariableRates(int *channelFlags, SampleTrackCache &cache,
                                    sampleCount *pos, float *queue,
                                    int *queueStart, int *queueLen,
                                    Resample * pResample)
{
   const auto track = cache.GetTrack().get();
   const double trackRate = track->GetRate();
   const double initialWarp = mRate / mSpeed / trackRate;
   const double tstep = 1.0 / trackRate;
   auto sampleSize = SAMPLE_SIZE(floatSample);

   decltype(mMaxOut) out = 0;

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

   while (out < mMaxOut) {
      if (*queueLen < (int)mProcessLen) {
         // Shift pending portion to start of the buffer
         memmove(queue, &queue[*queueStart], (*queueLen) * sampleSize);
         *queueStart = 0;

         auto getLen = limitSampleBufferSize(
            mQueueMaxLen - *queueLen,
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

               track->GetEnvelopeValues(mEnvValues.get(),
                                        getLen,
                                        (*pos - (getLen- 1)).as_double() / trackRate);
               *pos -= getLen;
            }
            else {
               auto results = cache.GetFloats(*pos, getLen, mMayThrow);
               if (results)
                  memcpy(&queue[*queueLen], results, sizeof(float) * getLen);
               else
                  memset(&queue[*queueLen], 0, sizeof(float) * getLen);

               track->GetEnvelopeValues(mEnvValues.get(),
                                        getLen,
                                        (*pos).as_double() / trackRate);

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

      auto thisProcessLen = mProcessLen;
      bool last = (*queueLen < (int)mProcessLen);
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
         // mMaxOut - out == 1 and &mFloatBuffer[out + 1] was an unmapped
         // address, because soxr, strangely, fetched an 8-byte (misaligned!)
         // value from &mFloatBuffer[out], but did nothing with it anyway,
         // in soxr_output_no_callback.
         // Now we make the bug go away by allocating a little more space in
         // the buffer than we need.
         &mFloatBuffer[out],
         mMaxOut - out);

      const auto input_used = results.first;
      *queueStart += input_used;
      *queueLen -= input_used;
      out += results.second;
      t += (input_used / trackRate) * (backwards ? -1 : 1);

      if (last) {
         break;
      }
   }

   for (size_t c = 0; c < mNumChannels; c++) {
      if (mApplyTrackGains) {
         mGains[c] = track->GetChannelGain(c);
      }
      else {
         mGains[c] = 1.0;
      }
   }

   MixBuffers(mNumChannels,
              channelFlags,
              mGains.get(),
              mFloatBuffer.get(),
              mTemp.get(),
              out,
              mInterleaved);

   return out;
}

size_t Mixer::MixSameRate(int *channelFlags, SampleTrackCache &cache,
                               sampleCount *pos)
{
   const auto track = cache.GetTrack().get();
   const double t = ( *pos ).as_double() / track->GetRate();
   const double trackEndTime = track->GetEndTime();
   const double trackStartTime = track->GetStartTime();
   const bool backwards = (mT1 < mT0);
   const double tEnd = backwards
      ? std::max(trackStartTime, mT1)
      : std::min(trackEndTime, mT1);

   //don't process if we're at the end of the selection or track.
   if ((backwards ? t <= tEnd : t >= tEnd))
      return 0;
   //if we're about to approach the end of the track or selection, figure out how much we need to grab
   auto slen = limitSampleBufferSize(
      mMaxOut,
      // PRL: maybe t and tEnd should be given as sampleCount instead to
      // avoid trouble subtracting one large value from another for a small
      // difference
      sampleCount{ (backwards ? t - tEnd : tEnd - t) * track->GetRate() + 0.5 }
   );

   if (backwards) {
      auto results = cache.GetFloats(*pos - (slen - 1), slen, mMayThrow);
      if (results)
         memcpy(mFloatBuffer.get(), results, sizeof(float) * slen);
      else
         memset(mFloatBuffer.get(), 0, sizeof(float) * slen);
      track->GetEnvelopeValues(mEnvValues.get(), slen, t - (slen - 1) / mRate);
      for(decltype(slen) i = 0; i < slen; i++)
         mFloatBuffer[i] *= mEnvValues[i]; // Track gain control will go here?
      ReverseSamples((samplePtr)mFloatBuffer.get(), floatSample, 0, slen);

      *pos -= slen;
   }
   else {
      auto results = cache.GetFloats(*pos, slen, mMayThrow);
      if (results)
         memcpy(mFloatBuffer.get(), results, sizeof(float) * slen);
      else
         memset(mFloatBuffer.get(), 0, sizeof(float) * slen);
      track->GetEnvelopeValues(mEnvValues.get(), slen, t);
      for(decltype(slen) i = 0; i < slen; i++)
         mFloatBuffer[i] *= mEnvValues[i]; // Track gain control will go here?

      *pos += slen;
   }

   for(size_t c=0; c<mNumChannels; c++)
      if (mApplyTrackGains)
         mGains[c] = track->GetChannelGain(c);
      else
         mGains[c] = 1.0;

   MixBuffers(mNumChannels, channelFlags, mGains.get(),
              mFloatBuffer.get(), mTemp.get(), slen, mInterleaved);

   return slen;
}

size_t Mixer::Process(size_t maxToProcess)
{
   // MB: this is wrong! mT represented warped time, and mTime is too inaccurate to use
   // it here. It's also unnecessary I think.
   //if (mT >= mT1)
   //   return 0;

   decltype(Process(0)) maxOut = 0;
   ArrayOf<int> channelFlags{ mNumChannels };

   mMaxOut = maxToProcess;

   Clear();
   for(size_t i=0; i<mNumInputTracks; i++) {
      const auto track = mInputTrack[i].GetTrack().get();
      for(size_t j=0; j<mNumChannels; j++)
         channelFlags[j] = 0;

      if( mMixerSpec ) {
         //ignore left and right when downmixing is not required
         for(size_t j = 0; j < mNumChannels; j++ )
            channelFlags[ j ] = mMixerSpec->mMap[ i ][ j ] ? 1 : 0;
      }
      else {
         switch(track->GetChannel()) {
         case Track::MonoChannel:
         default:
            for(size_t j=0; j<mNumChannels; j++)
               channelFlags[j] = 1;
            break;
         case Track::LeftChannel:
            channelFlags[0] = 1;
            break;
         case Track::RightChannel:
            if (mNumChannels >= 2)
               channelFlags[1] = 1;
            else
               channelFlags[0] = 1;
            break;
         }
      }
      if (mbVariableRates || track->GetRate() != mRate)
         maxOut = std::max(maxOut,
            MixVariableRates(channelFlags.get(), mInputTrack[i],
               &mSamplePos[i], mSampleQueue[i].get(),
               &mQueueStart[i], &mQueueLen[i], mResample[i].get()));
      else
         maxOut = std::max(maxOut,
            MixSameRate(channelFlags.get(), mInputTrack[i], &mSamplePos[i]));

      double t = mSamplePos[i].as_double() / (double)track->GetRate();
      if (mT0 > mT1)
         // backwards (as possibly in scrubbing)
         mTime = std::max(std::min(t, mTime), mT1);
      else
         // forwards (the usual)
         mTime = std::min(std::max(t, mTime), mT1);
   }
   if(mInterleaved) {
      for(size_t c=0; c<mNumChannels; c++) {
         CopySamples((constSamplePtr)(mTemp[0].get() + c),
            floatSample,
            mBuffer[0].ptr() + (c * SAMPLE_SIZE(mFormat)),
            mFormat,
            maxOut,
            mHighQuality ? gHighQualityDither : gLowQualityDither,
            mNumChannels,
            mNumChannels);
      }
   }
   else {
      for(size_t c=0; c<mNumBuffers; c++) {
         CopySamples((constSamplePtr)mTemp[c].get(),
            floatSample,
            mBuffer[c].ptr(),
            mFormat,
            maxOut,
            mHighQuality ? gHighQualityDither : gLowQualityDither);
      }
   }
   // MB: this doesn't take warping into account, replaced with code based on mSamplePos
   //mT += (maxOut / mRate);

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
   return mTime;
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

void Mixer::Reposition(double t, bool bSkipping)
{
   mTime = t;
   const bool backwards = (mT1 < mT0);
   if (backwards)
      mTime = std::max(mT1, (std::min(mT0, mTime)));
   else
      mTime = std::max(mT0, (std::min(mT1, mTime)));

   for(size_t i=0; i<mNumInputTracks; i++) {
      mSamplePos[i] = mInputTrack[i].GetTrack()->TimeToLongSamples(mTime);
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }

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
   mT0 = t0;
   mT1 = t1;
   mSpeed = fabs(speed);
   Reposition(t0, bSkipping);
}

void Mixer::SetSpeedForKeyboardScrubbing(double speed, double startTime)
{
   wxASSERT(std::isfinite(speed));

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

MixerSpec::MixerSpec( unsigned numTracks, unsigned maxNumChannels )
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

MixerSpec::MixerSpec( const MixerSpec &mixerSpec )
{
   mNumTracks = mixerSpec.mNumTracks;
   mMaxNumChannels = mixerSpec.mMaxNumChannels;
   mNumChannels = mixerSpec.mNumChannels;

   Alloc();

   for( unsigned int i = 0; i < mNumTracks; i++ )
      for( unsigned int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];
}

void MixerSpec::Alloc()
{
   mMap.reinit(mNumTracks, mMaxNumChannels);
}

MixerSpec::~MixerSpec()
{
}

bool MixerSpec::SetNumChannels( unsigned newNumChannels )
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

MixerSpec& MixerSpec::operator=( const MixerSpec &mixerSpec )
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

