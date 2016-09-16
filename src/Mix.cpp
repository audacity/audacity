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


#include "Audacity.h"
#include "Mix.h"

#include <math.h>

#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>

#include "WaveTrack.h"
#include "DirManager.h"
#include "Internat.h"
#include "Prefs.h"
#include "Project.h"
#include "Resample.h"
#include "TimeTrack.h"
#include "float_cast.h"

//TODO-MB: wouldn't it make more sense to DELETE the time track after 'mix and render'?
void MixAndRender(TrackList *tracks, TrackFactory *trackFactory,
                  double rate, sampleFormat format,
                  double startTime, double endTime,
                  WaveTrack::Holder &uLeft, WaveTrack::Holder &uRight)
{
   uLeft.reset(), uRight.reset();

   // This function was formerly known as "Quick Mix".
   const Track *t;
   bool mono = false;   /* flag if output can be mono without loosing anything*/
   bool oneinput = false;  /* flag set to true if there is only one input track
                              (mono or stereo) */

   TrackListIterator iter(tracks);
   SelectedTrackListOfKindIterator usefulIter(Track::Wave, tracks);
   // this only iterates tracks which are relevant to this function, i.e.
   // selected WaveTracks. The tracklist is (confusingly) the list of all
   // tracks in the project

   int numWaves = 0; /* number of wave tracks in the selection */
   int numMono = 0;  /* number of mono, centre-panned wave tracks in selection*/
   t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         numWaves++;
         float pan = ((WaveTrack*)t)->GetPan();
         if (t->GetChannel() == Track::MonoChannel && pan == 0)
            numMono++;
      }
      t = iter.Next();
   }

   if (numMono == numWaves)
      mono = true;

   /* the next loop will do two things at once:
    * 1. build an array of all the wave tracks were are trying to process
    * 2. determine when the set of WaveTracks starts and ends, in case we
    *    need to work out for ourselves when to start and stop rendering.
    */

   double mixStartTime = 0.0;    /* start time of first track to start */
   bool gotstart = false;  // flag indicates we have found a start time
   double mixEndTime = 0.0;   /* end time of last track to end */
   double tstart, tend;    // start and end times for one track.

   WaveTrackConstArray waveArray;
   t = iter.First();

   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         waveArray.push_back(static_cast<const WaveTrack *>(t));
         tstart = t->GetStartTime();
         tend = t->GetEndTime();
         if (tend > mixEndTime)
            mixEndTime = tend;
         // try and get the start time. If the track is empty we will get 0,
         // which is ambiguous because it could just mean the track starts at
         // the beginning of the project, as well as empty track. The give-away
         // is that an empty track also ends at zero.

         if (tstart != tend) {
            // we don't get empty tracks here
            if (!gotstart) {
               // no previous start, use this one unconditionally
               mixStartTime = tstart;
               gotstart = true;
            } else if (tstart < mixStartTime)
               mixStartTime = tstart;  // have a start, only make it smaller
         }  // end if start and end are different
      }  // end if track is a selected WaveTrack.
      /** @TODO: could we not use a SelectedTrackListOfKindIterator here? */
      t = iter.Next();
   }

   /* create the destination track (NEW track) */
   if ((numWaves == 1) || ((numWaves == 2) && (usefulIter.First()->GetLink() != NULL)))
      oneinput = true;
   // only one input track (either 1 mono or one linked stereo pair)

   auto mixLeft = trackFactory->NewWaveTrack(format, rate);
   if (oneinput)
      mixLeft->SetName(usefulIter.First()->GetName()); /* set name of output track to be the same as the sole input track */
   else
      mixLeft->SetName(_("Mix"));
   mixLeft->SetOffset(mixStartTime);
   decltype(mixLeft) mixRight{};
   if (mono) {
      mixLeft->SetChannel(Track::MonoChannel);
   }
   else {
      mixRight = trackFactory->NewWaveTrack(format, rate);
      if (oneinput) {
         if (usefulIter.First()->GetLink() != NULL)   // we have linked track
            mixLeft->SetName(usefulIter.First()->GetLink()->GetName()); /* set name to match input track's right channel!*/
         else
            mixLeft->SetName(usefulIter.First()->GetName());   /* set name to that of sole input channel */
      }
      else
         mixRight->SetName(_("Mix"));
      mixLeft->SetChannel(Track::LeftChannel);
      mixRight->SetChannel(Track::RightChannel);
      mixRight->SetOffset(mixStartTime);
      mixLeft->SetLinked(true);
   }



   auto maxBlockLen = mixLeft->GetIdealBlockSize();

   // If the caller didn't specify a time range, use the whole range in which
   // any input track had clips in it.
   if (startTime == endTime) {
      startTime = mixStartTime;
      endTime = mixEndTime;
   }

   Mixer mixer(waveArray,
      Mixer::WarpOptions(tracks->GetTimeTrack()),
      startTime, endTime, mono ? 1 : 2, maxBlockLen, false,
      rate, format);

   ::wxSafeYield();

   int updateResult = eProgressSuccess;
   {
      ProgressDialog progress(_("Mix and Render"),
         _("Mixing and rendering tracks"));

      while (updateResult == eProgressSuccess) {
         auto blockLen = mixer.Process(maxBlockLen);

         if (blockLen == 0)
            break;

         if (mono) {
            samplePtr buffer = mixer.GetBuffer();
            mixLeft->Append(buffer, format, blockLen);
         }
         else {
            samplePtr buffer;
            buffer = mixer.GetBuffer(0);
            mixLeft->Append(buffer, format, blockLen);
            buffer = mixer.GetBuffer(1);
            mixRight->Append(buffer, format, blockLen);
         }

         updateResult = progress.Update(mixer.MixGetCurrentTime() - startTime, endTime - startTime);
      }
   }

   mixLeft->Flush();
   if (!mono)
      mixRight->Flush();
   if (updateResult == eProgressCancelled || updateResult == eProgressFailed)
   {
      return;
   }
   else {
      uLeft = std::move(mixLeft),
         uRight = std::move(mixRight);
#if 0
   int elapsedMS = wxGetElapsedTime();
   double elapsedTime = elapsedMS * 0.001;
   double maxTracks = totalTime / (elapsedTime / numWaves);

   // Note: these shouldn't be translated - they're for debugging
   // and profiling only.
   printf("      Tracks: %d\n", numWaves);
   printf("  Mix length: %f sec\n", totalTime);
   printf("Elapsed time: %f sec\n", elapsedTime);
   printf("Max number of tracks to mix in real time: %f\n", maxTracks);
#endif
   }
}

Mixer::WarpOptions::WarpOptions(double min, double max)
   : timeTrack(0), minSpeed(min), maxSpeed(max)
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

Mixer::Mixer(const WaveTrackConstArray &inputTracks,
             const WarpOptions &warpOptions,
             double startTime, double stopTime,
             unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
             double outRate, sampleFormat outFormat,
             bool highQuality, MixerSpec *mixerSpec)
{
   int i;

   const auto numInputTracks = inputTracks.size();
   mHighQuality = highQuality;
   mNumInputTracks = numInputTracks;
   mInputTrack = new WaveTrackCache[mNumInputTracks];

   // mSamplePos holds for each track the next sample position not
   // yet processed.
   mSamplePos = new sampleCount[mNumInputTracks];
   for(i=0; i<mNumInputTracks; i++) {
      mInputTrack[i].SetTrack(inputTracks[i]);
      mSamplePos[i] = inputTracks[i]->TimeToLongSamples(startTime);
   }
   mTimeTrack = warpOptions.timeTrack;
   mT0 = startTime;
   mT1 = stopTime;
   mTime = startTime;
   mNumChannels = numOutChannels;
   mBufferSize = outBufferSize;
   mInterleaved = outInterleaved;
   mRate = outRate;
   mSpeed = 1.0;
   mFormat = outFormat;
   mApplyTrackGains = true;
   mGains = new float[mNumChannels];
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

   mBuffer = new SampleBuffer[mNumBuffers];
   mTemp = new SampleBuffer[mNumBuffers];
   for (int c = 0; c < mNumBuffers; c++) {
      mBuffer[c].Allocate(mInterleavedBufferSize, mFormat);
      mTemp[c].Allocate(mInterleavedBufferSize, floatSample);
   }
   mFloatBuffer = new float[mInterleavedBufferSize];

   // This is the number of samples grabbed in one go from a track
   // and placed in a queue, when mixing with resampling.
   // (Should we use WaveTrack::GetBestBlockSize instead?)
   mQueueMaxLen = 65536;

   // But cut the queue into blocks of this finer size
   // for variable rate resampling.  Each block is resampled at some
   // constant rate.
   mProcessLen = 1024;

   // Position in each queue of the start of the next block to resample.
   mQueueStart = new int[mNumInputTracks];

   // For each queue, the number of available samples after the queue start.
   mQueueLen = new int[mNumInputTracks];
   mSampleQueue = new float *[mNumInputTracks];
   mResample = new Resample*[mNumInputTracks];
   for(i=0; i<mNumInputTracks; i++) {
      double factor = (mRate / mInputTrack[i].GetTrack()->GetRate());
      double minFactor, maxFactor;
      if (mTimeTrack) {
         // variable rate resampling
         mbVariableRates = true;
         minFactor = factor / mTimeTrack->GetRangeUpper();
         maxFactor = factor / mTimeTrack->GetRangeLower();
      }
      else if (warpOptions.minSpeed > 0.0 && warpOptions.maxSpeed > 0.0) {
         // variable rate resampling
         mbVariableRates = true;
         minFactor = factor / warpOptions.maxSpeed;
         maxFactor = factor / warpOptions.minSpeed;
      }
      else {
         // constant rate resampling
         mbVariableRates = false;
         minFactor = maxFactor = factor;
      }

      mResample[i] = new Resample(mHighQuality, minFactor, maxFactor);
      mSampleQueue[i] = new float[mQueueMaxLen];
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }

   const auto envLen = std::max(mQueueMaxLen, mInterleavedBufferSize);
   mEnvValues = new double[envLen];
}

Mixer::~Mixer()
{
   int i;

   delete[] mBuffer;
   delete[] mTemp;
   delete[] mInputTrack;
   delete[] mEnvValues;
   delete[] mFloatBuffer;
   delete[] mGains;
   delete[] mSamplePos;

   for(i=0; i<mNumInputTracks; i++) {
      delete mResample[i];
      delete[] mSampleQueue[i];
   }
   delete[] mResample;
   delete[] mSampleQueue;
   delete[] mQueueStart;
   delete[] mQueueLen;
}

void Mixer::ApplyTrackGains(bool apply)
{
   mApplyTrackGains = apply;
}

void Mixer::Clear()
{
   for (int c = 0; c < mNumBuffers; c++) {
      memset(mTemp[c].ptr(), 0, mInterleavedBufferSize * SAMPLE_SIZE(floatSample));
   }
}

void MixBuffers(unsigned numChannels, int *channelFlags, float *gains,
                samplePtr src, SampleBuffer *dests,
                int len, bool interleaved)
{
   for (int c = 0; c < numChannels; c++) {
      if (!channelFlags[c])
         continue;

      samplePtr destPtr;
      unsigned skip;

      if (interleaved) {
         destPtr = dests[0].ptr() + c*SAMPLE_SIZE(floatSample);
         skip = numChannels;
      } else {
         destPtr = dests[c].ptr();
         skip = 1;
      }

      float gain = gains[c];
      float *dest = (float *)destPtr;
      float *temp = (float *)src;
      for (int j = 0; j < len; j++) {
         *dest += temp[j] * gain;   // the actual mixing process
         dest += skip;
      }
   }
}

size_t Mixer::MixVariableRates(int *channelFlags, WaveTrackCache &cache,
                                    sampleCount *pos, float *queue,
                                    int *queueStart, int *queueLen,
                                    Resample * pResample)
{
   const WaveTrack *const track = cache.GetTrack();
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
      if (*queueLen < mProcessLen) {
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
               auto results = cache.Get(floatSample, *pos - (getLen - 1), getLen);
               memcpy(&queue[*queueLen], results, sizeof(float) * getLen);

               track->GetEnvelopeValues(mEnvValues,
                                        getLen,
                                        (*pos - (getLen- 1)).as_double() / trackRate);
               *pos -= getLen;
            }
            else {
               auto results = cache.Get(floatSample, *pos, getLen);
               memcpy(&queue[*queueLen], results, sizeof(float) * getLen);

               track->GetEnvelopeValues(mEnvValues,
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
      bool last = (*queueLen < mProcessLen);
      if (last) {
         thisProcessLen = *queueLen;
      }

      double factor = initialWarp;
      if (mTimeTrack)
      {
         //TODO-MB: The end time is wrong when the resampler doesn't use all input samples,
         //         as a result of this the warp factor may be slightly wrong, so AudioIO will stop too soon
         //         or too late (resulting in missing sound or inserted silence). This can't be fixed
         //         without changing the way the resampler works, because the number of input samples that will be used
         //         is unpredictable. Maybe it can be compensated later though.
         if (backwards)
            factor *= mTimeTrack->ComputeWarpFactor
               (t - (double)thisProcessLen / trackRate + tstep, t + tstep);
         else
            factor *= mTimeTrack->ComputeWarpFactor
               (t, t + (double)thisProcessLen / trackRate);
      }

      auto results = pResample->Process(factor,
                                      &queue[*queueStart],
                                      thisProcessLen,
                                      last,
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

   for (int c = 0; c < mNumChannels; c++) {
      if (mApplyTrackGains) {
         mGains[c] = track->GetChannelGain(c);
      }
      else {
         mGains[c] = 1.0;
      }
   }

   MixBuffers(mNumChannels,
              channelFlags,
              mGains,
              (samplePtr)mFloatBuffer,
              mTemp,
              out,
              mInterleaved);

   return out;
}

size_t Mixer::MixSameRate(int *channelFlags, WaveTrackCache &cache,
                               sampleCount *pos)
{
   const WaveTrack *const track = cache.GetTrack();
   auto slen = mMaxOut;
   int c;
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
   slen = std::min<decltype(slen)>( slen,
      ((backwards ? t - tEnd : tEnd - t) * track->GetRate() + 0.5)
   );
   slen = std::min(slen, mMaxOut);

   if (backwards) {
      auto results = cache.Get(floatSample, *pos - (slen - 1), slen);
      memcpy(mFloatBuffer, results, sizeof(float) * slen);
      track->GetEnvelopeValues(mEnvValues, slen, t - (slen - 1) / mRate);
      for(decltype(slen) i = 0; i < slen; i++)
         mFloatBuffer[i] *= mEnvValues[i]; // Track gain control will go here?
      ReverseSamples((samplePtr)mFloatBuffer, floatSample, 0, slen);

      *pos -= slen;
   }
   else {
      auto results = cache.Get(floatSample, *pos, slen);
      memcpy(mFloatBuffer, results, sizeof(float) * slen);
      track->GetEnvelopeValues(mEnvValues, slen, t);
      for(decltype(slen) i = 0; i < slen; i++)
         mFloatBuffer[i] *= mEnvValues[i]; // Track gain control will go here?

      *pos += slen;
   }

   for(c=0; c<mNumChannels; c++)
      if (mApplyTrackGains)
         mGains[c] = track->GetChannelGain(c);
      else
         mGains[c] = 1.0;

   MixBuffers(mNumChannels, channelFlags, mGains,
              (samplePtr)mFloatBuffer, mTemp, slen, mInterleaved);

   return slen;
}

size_t Mixer::Process(size_t maxToProcess)
{
   // MB: this is wrong! mT represented warped time, and mTime is too inaccurate to use
   // it here. It's also unnecessary I think.
   //if (mT >= mT1)
   //   return 0;

   int i, j;
   decltype(Process(0)) maxOut = 0;
   int *channelFlags = new int[mNumChannels];

   mMaxOut = maxToProcess;

   Clear();
   for(i=0; i<mNumInputTracks; i++) {
      const WaveTrack *const track = mInputTrack[i].GetTrack();
      for(j=0; j<mNumChannels; j++)
         channelFlags[j] = 0;

      if( mMixerSpec ) {
         //ignore left and right when downmixing is not required
         for( j = 0; j < mNumChannels; j++ )
            channelFlags[ j ] = mMixerSpec->mMap[ i ][ j ] ? 1 : 0;
      }
      else {
         switch(track->GetChannel()) {
         case Track::MonoChannel:
         default:
            for(j=0; j<mNumChannels; j++)
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
            MixVariableRates(channelFlags, mInputTrack[i],
               &mSamplePos[i], mSampleQueue[i],
               &mQueueStart[i], &mQueueLen[i], mResample[i]));
      else
         maxOut = std::max(maxOut,
            MixSameRate(channelFlags, mInputTrack[i], &mSamplePos[i]));

      double t = mSamplePos[i].as_double() / (double)track->GetRate();
      if (mT0 > mT1)
         // backwards (as possibly in scrubbing)
         mTime = std::max(std::min(t, mTime), mT1);
      else
         // forwards (the usual)
         mTime = std::min(std::max(t, mTime), mT1);
   }
   if(mInterleaved) {
      for(int c=0; c<mNumChannels; c++) {
         CopySamples(mTemp[0].ptr() + (c * SAMPLE_SIZE(floatSample)),
            floatSample,
            mBuffer[0].ptr() + (c * SAMPLE_SIZE(mFormat)),
            mFormat,
            maxOut,
            mHighQuality,
            mNumChannels,
            mNumChannels);
      }
   }
   else {
      for(int c=0; c<mNumBuffers; c++) {
         CopySamples(mTemp[c].ptr(),
            floatSample,
            mBuffer[c].ptr(),
            mFormat,
            maxOut,
            mHighQuality);
      }
   }
   // MB: this doesn't take warping into account, replaced with code based on mSamplePos
   //mT += (maxOut / mRate);

   delete [] channelFlags;

   return maxOut;
}

samplePtr Mixer::GetBuffer()
{
   return mBuffer[0].ptr();
}

samplePtr Mixer::GetBuffer(int channel)
{
   return mBuffer[channel].ptr();
}

double Mixer::MixGetCurrentTime()
{
   return mTime;
}

void Mixer::Restart()
{
   int i;

   mTime = mT0;

   for(i=0; i<mNumInputTracks; i++)
      mSamplePos[i] = mInputTrack[i].GetTrack()->TimeToLongSamples(mT0);

   for(i=0; i<mNumInputTracks; i++) {
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }
}

void Mixer::Reposition(double t)
{
   int i;

   mTime = t;
   const bool backwards = (mT1 < mT0);
   if (backwards)
      mTime = std::max(mT1, (std::min(mT0, mTime)));
   else
      mTime = std::max(mT0, (std::min(mT1, mTime)));

   for(i=0; i<mNumInputTracks; i++) {
      mSamplePos[i] = mInputTrack[i].GetTrack()->TimeToLongSamples(mTime);
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }
}

void Mixer::SetTimesAndSpeed(double t0, double t1, double speed)
{
   wxASSERT(std::isfinite(speed));
   mT0 = t0;
   mT1 = t1;
   mSpeed = fabs(speed);
   Reposition(t0);
}

MixerSpec::MixerSpec( unsigned numTracks, unsigned maxNumChannels )
{
   mNumTracks = mNumChannels = numTracks;
   mMaxNumChannels = maxNumChannels;

   if( mNumChannels > mMaxNumChannels )
         mNumChannels = mMaxNumChannels;

   Alloc();

   for( int i = 0; i < mNumTracks; i++ )
      for( int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = ( i == j );
}

MixerSpec::MixerSpec( const MixerSpec &mixerSpec )
{
   mNumTracks = mixerSpec.mNumTracks;
   mMaxNumChannels = mixerSpec.mMaxNumChannels;
   mNumChannels = mixerSpec.mNumChannels;

   Alloc();

   for( int i = 0; i < mNumTracks; i++ )
      for( int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];
}

void MixerSpec::Alloc()
{
   mMap = new bool*[ mNumTracks ];
   for( int i = 0; i < mNumTracks; i++ )
      mMap[ i ] = new bool[ mMaxNumChannels ];
}

MixerSpec::~MixerSpec()
{
   Free();
}

void MixerSpec::Free()
{
   for( int i = 0; i < mNumTracks; i++ )
      delete[] mMap[ i ];

   delete[] mMap;
}

bool MixerSpec::SetNumChannels( unsigned newNumChannels )
{
   if( mNumChannels == newNumChannels )
      return true;

   if( newNumChannels > mMaxNumChannels )
      return false;

   for( int i = 0; i < mNumTracks; i++ )
   {
      for( int j = newNumChannels; j < mNumChannels; j++ )
         mMap[ i ][ j ] = false;

      for( int j = mNumChannels; j < newNumChannels; j++ )
         mMap[ i ][ j ] = false;
   }

   mNumChannels = newNumChannels;
   return true;
}

MixerSpec& MixerSpec::operator=( const MixerSpec &mixerSpec )
{
   Free();

   mNumTracks = mixerSpec.mNumTracks;
   mNumChannels = mixerSpec.mNumChannels;
   mMaxNumChannels = mixerSpec.mMaxNumChannels;

   Alloc();

   for( int i = 0; i < mNumTracks; i++ )
      for( int j = 0; j < mNumChannels; j++ )
         mMap[ i ][ j ] = mixerSpec.mMap[ i ][ j ];

   return *this;
}

