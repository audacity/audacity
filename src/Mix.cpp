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
#include "Envelope.h"
#include "Internat.h"
#include "Prefs.h"
#include "Project.h"
#include "Resample.h"
#include "float_cast.h"

//TODO-MB: wouldn't it make more sense to delete the time track after 'mix and render'?
bool MixAndRender(TrackList *tracks, TrackFactory *trackFactory,
                  double rate, sampleFormat format,
                  double startTime, double endTime,
                  WaveTrack **newLeft, WaveTrack **newRight)
{
   // This function was formerly known as "Quick Mix".
   WaveTrack **waveArray;
   Track *t;
   int numWaves = 0; /* number of wave tracks in the selection */
   int numMono = 0;  /* number of mono, centre-panned wave tracks in selection*/
   bool mono = false;   /* flag if output can be mono without loosing anything*/
   bool oneinput = false;  /* flag set to true if there is only one input track
                              (mono or stereo) */
   int w;

   TrackListIterator iter(tracks);
   SelectedTrackListOfKindIterator usefulIter(Track::Wave, tracks);
   // this only iterates tracks which are relevant to this function, i.e.
   // selected WaveTracks. The tracklist is (confusingly) the list of all
   // tracks in the project

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

   waveArray = new WaveTrack *[numWaves];
   w = 0;
   t = iter.First();

   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         waveArray[w++] = (WaveTrack *) t;
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

   /* create the destination track (new track) */
   if ((numWaves == 1) || ((numWaves == 2) && (usefulIter.First()->GetLink() != NULL)))
      oneinput = true;
   // only one input track (either 1 mono or one linked stereo pair)

   WaveTrack *mixLeft = trackFactory->NewWaveTrack(format, rate);
   if (oneinput)
      mixLeft->SetName(usefulIter.First()->GetName()); /* set name of output track to be the same as the sole input track */
   else
      mixLeft->SetName(_("Mix"));
   mixLeft->SetOffset(mixStartTime);
   WaveTrack *mixRight = 0;
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



   int maxBlockLen = mixLeft->GetIdealBlockSize();

   // If the caller didn't specify a time range, use the whole range in which
   // any input track had clips in it.
   if (startTime == endTime) {
      startTime = mixStartTime;
      endTime = mixEndTime;
   }

   Mixer *mixer = new Mixer(numWaves, waveArray, tracks->GetTimeTrack(),
                            startTime, endTime, mono ? 1 : 2, maxBlockLen, false,
                            rate, format);

   ::wxSafeYield();
   ProgressDialog *progress = new ProgressDialog(_("Mix and Render"),
                                                 _("Mixing and rendering tracks"));

   int updateResult = eProgressSuccess;
   while(updateResult == eProgressSuccess) {
      sampleCount blockLen = mixer->Process(maxBlockLen);

      if (blockLen == 0)
         break;

      if (mono) {
         samplePtr buffer = mixer->GetBuffer();
         mixLeft->Append(buffer, format, blockLen);
      }
      else {
         samplePtr buffer;
         buffer = mixer->GetBuffer(0);
         mixLeft->Append(buffer, format, blockLen);
         buffer = mixer->GetBuffer(1);
         mixRight->Append(buffer, format, blockLen);
      }

      updateResult = progress->Update(mixer->MixGetCurrentTime() - startTime, endTime - startTime);
   }

   delete progress;

   mixLeft->Flush();
   if (!mono)
      mixRight->Flush();
   if (updateResult == eProgressCancelled || updateResult == eProgressFailed)
   {
      delete mixLeft;
      if (!mono)
         delete mixRight;
   } else {
      *newLeft = mixLeft;
      if (!mono)
         *newRight = mixRight;

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

   delete[] waveArray;
   delete mixer;

   return (updateResult == eProgressSuccess || updateResult == eProgressStopped);
}

Mixer::Mixer(int numInputTracks, WaveTrack **inputTracks,
             TimeTrack *timeTrack,
             double startTime, double stopTime,
             int numOutChannels, int outBufferSize, bool outInterleaved,
             double outRate, sampleFormat outFormat,
             bool highQuality, MixerSpec *mixerSpec)
{
   int i;

   mHighQuality = highQuality;
   mNumInputTracks = numInputTracks;
   mInputTrack = new WaveTrack*[mNumInputTracks];
   mSamplePos = new sampleCount[mNumInputTracks];
   for(i=0; i<mNumInputTracks; i++) {
      mInputTrack[i] = inputTracks[i];
      mSamplePos[i] = inputTracks[i]->TimeToLongSamples(startTime);
   }
   mTimeTrack = timeTrack;
   mT0 = startTime;
   mT1 = stopTime;
   mTime = startTime;
   mNumChannels = numOutChannels;
   mBufferSize = outBufferSize;
   mInterleaved = outInterleaved;
   mRate = outRate;
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

   mBuffer = new samplePtr[mNumBuffers];
   mTemp = new samplePtr[mNumBuffers];
   for (int c = 0; c < mNumBuffers; c++) {
      mBuffer[c] = NewSamples(mInterleavedBufferSize, mFormat);
      mTemp[c] = NewSamples(mInterleavedBufferSize, floatSample);
   }
   mFloatBuffer = new float[mInterleavedBufferSize];

   mQueueMaxLen = 65536;
   mProcessLen = 1024;

   mQueueStart = new int[mNumInputTracks];
   mQueueLen = new int[mNumInputTracks];
   mSampleQueue = new float *[mNumInputTracks];
   mResample = new Resample*[mNumInputTracks];
   for(i=0; i<mNumInputTracks; i++) {
      double factor = (mRate / mInputTrack[i]->GetRate());
      if (timeTrack) {
         // variable rate resampling
         mResample[i] = new Resample(mHighQuality,
                                      factor / timeTrack->GetRangeUpper(),
                                      factor / timeTrack->GetRangeLower());
      } else {
         mResample[i] = new Resample(mHighQuality, factor, factor); // constant rate resampling
      }
      mSampleQueue[i] = new float[mQueueMaxLen];
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }

   int envLen = mInterleavedBufferSize;
   if (mQueueMaxLen > envLen)
      envLen = mQueueMaxLen;
   mEnvValues = new double[envLen];
}

Mixer::~Mixer()
{
   int i;

   for (i = 0; i < mNumBuffers; i++) {
      DeleteSamples(mBuffer[i]);
      DeleteSamples(mTemp[i]);
   }
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
      memset(mTemp[c], 0, mInterleavedBufferSize * SAMPLE_SIZE(floatSample));
   }
}

void MixBuffers(int numChannels, int *channelFlags, float *gains,
                samplePtr src, samplePtr *dests,
                int len, bool interleaved)
{
   for (int c = 0; c < numChannels; c++) {
      if (!channelFlags[c])
         continue;

      samplePtr destPtr;
      int skip;

      if (interleaved) {
         destPtr = dests[0] + c*SAMPLE_SIZE(floatSample);
         skip = numChannels;
      } else {
         destPtr = dests[c];
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

sampleCount Mixer::MixVariableRates(int *channelFlags, WaveTrack *track,
                                    sampleCount *pos, float *queue,
                                    int *queueStart, int *queueLen,
                                    Resample * pResample)
{
   double trackRate = track->GetRate();
   double initialWarp = mRate / trackRate;
   double tstep = 1.0 / trackRate;
   double t = (*pos - *queueLen) / trackRate;
   int sampleSize = SAMPLE_SIZE(floatSample);

   sampleCount out = 0;

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
   sampleCount endPos;
   double endTime = track->GetEndTime();
   if (endTime > mT1) {
      endPos = track->TimeToLongSamples(mT1);
   }
   else {
      endPos = track->TimeToLongSamples(endTime);
   }

   while (out < mMaxOut) {
      if (*queueLen < mProcessLen) {
         memmove(queue, &queue[*queueStart], (*queueLen) * sampleSize);
         *queueStart = 0;

         int getLen = mQueueMaxLen - *queueLen;

         // Constrain
         if (*pos + getLen > endPos) {
            getLen = endPos - *pos;
         }

         // Nothing to do if past end of track
         if (getLen > 0) {
            track->Get((samplePtr)&queue[*queueLen],
                       floatSample,
                       *pos,
                       getLen);

            track->GetEnvelopeValues(mEnvValues,
                                     getLen,
                                     (*pos) / trackRate,
                                     tstep);

            for (int i = 0; i < getLen; i++) {
               queue[(*queueLen) + i] *= mEnvValues[i];
            }

            *queueLen += getLen;
            *pos += getLen;
         }
      }

      sampleCount thisProcessLen = mProcessLen;
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
         //         is unpredictable. Maybe it can be compensated lated though.
         factor *= mTimeTrack->ComputeWarpFactor(t, t + (double)thisProcessLen / trackRate);
      }

      int input_used;
      int outgen = pResample->Process(factor,
                                      &queue[*queueStart],
                                      thisProcessLen,
                                      last,
                                      &input_used,
                                      &mFloatBuffer[out],
                                      mMaxOut - out);

      if (outgen < 0) {
         return 0;
      }

      *queueStart += input_used;
      *queueLen -= input_used;
      out += outgen;
      t += (input_used / trackRate);

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

sampleCount Mixer::MixSameRate(int *channelFlags, WaveTrack *track,
                               sampleCount *pos)
{
   int slen = mMaxOut;
   int c;
   double t = *pos / track->GetRate();
   double trackEndTime = track->GetEndTime();
   double tEnd = trackEndTime > mT1 ? mT1 : trackEndTime;

   //don't process if we're at the end of the selection or track.
   if (t>=tEnd)
      return 0;
   //if we're about to approach the end of the track or selection, figure out how much we need to grab
   if (t + slen/track->GetRate() > tEnd)
      slen = (int)((tEnd - t) * track->GetRate() + 0.5);

   if (slen > mMaxOut)
      slen = mMaxOut;

   track->Get((samplePtr)mFloatBuffer, floatSample, *pos, slen);
   track->GetEnvelopeValues(mEnvValues, slen, t, 1.0 / mRate);
   for(int i=0; i<slen; i++)
      mFloatBuffer[i] *= mEnvValues[i]; // Track gain control will go here?

   for(c=0; c<mNumChannels; c++)
      if (mApplyTrackGains)
         mGains[c] = track->GetChannelGain(c);
      else
         mGains[c] = 1.0;

   MixBuffers(mNumChannels, channelFlags, mGains,
              (samplePtr)mFloatBuffer, mTemp, slen, mInterleaved);

   *pos += slen;

   return slen;
}

sampleCount Mixer::Process(sampleCount maxToProcess)
{
   // MB: this is wrong! mT represented warped time, and mTime is too inaccurate to use
   // it here. It's also unnecessary I think.
   //if (mT >= mT1)
   //   return 0;

   int i, j;
   sampleCount out;
   sampleCount maxOut = 0;
   int *channelFlags = new int[mNumChannels];

   mMaxOut = maxToProcess;

   Clear();
   for(i=0; i<mNumInputTracks; i++) {
      WaveTrack *track = mInputTrack[i];
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

      if (mTimeTrack || track->GetRate() != mRate)
         out = MixVariableRates(channelFlags, track,
                                &mSamplePos[i], mSampleQueue[i],
                                &mQueueStart[i], &mQueueLen[i], mResample[i]);
      else
         out = MixSameRate(channelFlags, track, &mSamplePos[i]);

      if (out > maxOut)
         maxOut = out;

      double t = (double)mSamplePos[i] / (double)track->GetRate();
      if(t > mTime)
         mTime = std::min(t, mT1);

   }
   if(mInterleaved) {
      for(int c=0; c<mNumChannels; c++) {
         CopySamples(mTemp[0] + (c * SAMPLE_SIZE(floatSample)),
                     floatSample,
                     mBuffer[0] + (c * SAMPLE_SIZE(mFormat)),
                     mFormat,
                     maxOut,
                     mHighQuality,
                     mNumChannels,
                     mNumChannels);
      }
   }
   else {
      for(int c=0; c<mNumBuffers; c++) {
            CopySamples(mTemp[c],
                        floatSample,
                        mBuffer[c],
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
   return mBuffer[0];
}

samplePtr Mixer::GetBuffer(int channel)
{
   return mBuffer[channel];
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
      mSamplePos[i] = mInputTrack[i]->TimeToLongSamples(mT0);

   for(i=0; i<mNumInputTracks; i++) {
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }
}

void Mixer::Reposition(double t)
{
   int i;

   mTime = t;
   if( mTime < mT0 )
      mTime = mT0;
   if( mTime > mT1 )
      mTime = mT1;

   for(i=0; i<mNumInputTracks; i++) {
      mSamplePos[i] = mInputTrack[i]->TimeToLongSamples(mTime);
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }
}

MixerSpec::MixerSpec( int numTracks, int maxNumChannels )
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

bool MixerSpec::SetNumChannels( int newNumChannels )
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

