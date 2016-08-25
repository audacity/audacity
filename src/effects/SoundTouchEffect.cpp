/**********************************************************************

Audacity: A Digital Audio Editor

SoundTouchEffect.cpp

Dominic Mazzoni, Vaughan Johnson

This abstract class contains all of the common code for an
effect that uses SoundTouch to do its processing (ChangeTempo
                                                  and ChangePitch).

**********************************************************************/

#include "../Audacity.h"

#if USE_SOUNDTOUCH

#include <math.h>

#include "../LabelTrack.h"
#include "../WaveTrack.h"
#include "../Project.h"
#include "SoundTouchEffect.h"
#include "TimeWarper.h"
#include "../NoteTrack.h"

bool EffectSoundTouch::ProcessLabelTrack(LabelTrack *lt)
{
//   SetTimeWarper(std::make_unique<RegionTimeWarper>(mCurT0, mCurT1,
 //           std::make_unique<LinearTimeWarper>(mCurT0, mCurT0,
   //            mCurT1, mCurT0 + (mCurT1-mCurT0)*mFactor)));
   lt->WarpLabels(*GetTimeWarper());
   return true;
}

#ifdef USE_MIDI
bool EffectSoundTouch::ProcessNoteTrack(NoteTrack *nt)
{
   nt->WarpAndTransposeNotes(mCurT0, mCurT1, *GetTimeWarper(), mSemitones);
   return true;
}
#endif

bool EffectSoundTouch::Process()
{
   // Assumes that mSoundTouch has already been initialized
   // by the subclass for subclass-specific parameters. The
   // time warper should also be set.

   // Check if this effect will alter the selection length; if so, we need
   // to operate on sync-lock selected tracks.
   bool mustSync = true;
   if (mT1 == GetTimeWarper()->Warp(mT1)) {
      mustSync = false;
   }

   //Iterate over each track
   // Needs Track::All for sync-lock grouping.
   this->CopyInputTracks(Track::All);
   bool bGoodResult = true;

   TrackListIterator iter(mOutputTracks.get());
   Track* t;
   mCurTrackNum = 0;
   m_maxNewLength = 0.0;

   t = iter.First();
   while (t != NULL) {
      if (t->GetKind() == Track::Label &&
            (t->GetSelected() || (mustSync && t->IsSyncLockSelected())) )
      {
         if (!ProcessLabelTrack(static_cast<LabelTrack*>(t)))
         {
            bGoodResult = false;
            break;
         }
      }
#ifdef USE_MIDI
      else if (t->GetKind() == Track::Note &&
               (t->GetSelected() || (mustSync && t->IsSyncLockSelected())))
      {
         if (!ProcessNoteTrack(static_cast<NoteTrack*>(t)))
         {
            bGoodResult = false;
            break;
         }
      }
#endif
      else if (t->GetKind() == Track::Wave && t->GetSelected())
      {
         WaveTrack* leftTrack = (WaveTrack*)t;
         //Get start and end times from track
         mCurT0 = leftTrack->GetStartTime();
         mCurT1 = leftTrack->GetEndTime();

         //Set the current bounds to whichever left marker is
         //greater and whichever right marker is less
         mCurT0 = wxMax(mT0, mCurT0);
         mCurT1 = wxMin(mT1, mCurT1);

         // Process only if the right marker is to the right of the left marker
         if (mCurT1 > mCurT0) {

            if (leftTrack->GetLinked()) {
               double t;
               // Assume linked track is wave
               WaveTrack* rightTrack = static_cast<WaveTrack*>(iter.Next());

               //Adjust bounds by the right tracks markers
               t = rightTrack->GetStartTime();
               t = wxMax(mT0, t);
               mCurT0 = wxMin(mCurT0, t);
               t = rightTrack->GetEndTime();
               t = wxMin(mT1, t);
               mCurT1 = wxMax(mCurT1, t);

               //Transform the marker timepoints to samples
               auto start = leftTrack->TimeToLongSamples(mCurT0);
               auto end = leftTrack->TimeToLongSamples(mCurT1);

               //Inform soundtouch there's 2 channels
               mSoundTouch->setChannels(2);

               //ProcessStereo() (implemented below) processes a stereo track
               if (!ProcessStereo(leftTrack, rightTrack, start, end))
               {
                  bGoodResult = false;
                  break;
               }
               mCurTrackNum++; // Increment for rightTrack, too.
            } else {
               //Transform the marker timepoints to samples
               auto start = leftTrack->TimeToLongSamples(mCurT0);
               auto end = leftTrack->TimeToLongSamples(mCurT1);

               //Inform soundtouch there's a single channel
               mSoundTouch->setChannels(1);

               //ProcessOne() (implemented below) processes a single track
               if (!ProcessOne(leftTrack, start, end))
               {
                  bGoodResult = false;
                  break;
               }
            }
         }
         mCurTrackNum++;
      }
      else if (mustSync && t->IsSyncLockSelected()) {
         t->SyncLockAdjust(mT1, GetTimeWarper()->Warp(mT1));
      }

      //Iterate to the next track
      t = iter.Next();
   }

   if (bGoodResult)
      ReplaceProcessedTracks(bGoodResult);

   mSoundTouch.reset();

//   mT0 = mCurT0;
//   mT1 = mCurT0 + m_maxNewLength; // Update selection.

   return bGoodResult;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessSoundTouch on these blocks
bool EffectSoundTouch::ProcessOne(WaveTrack *track,
                                  sampleCount start, sampleCount end)
{
   mSoundTouch->setSampleRate((unsigned int)(track->GetRate()+0.5));

   auto outputTrack = mFactory->NewWaveTrack(track->GetSampleFormat(), track->GetRate());

   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer = new float[track->GetMaxBlockSize()];

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   auto s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      const auto block =
         limitSampleBufferSize( track->GetBestBlockSize(s), end - s );

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr) buffer, floatSample, s, block);

      //Add samples to SoundTouch
      mSoundTouch->putSamples(buffer, block);

      //Get back samples from SoundTouch
      unsigned int outputCount = mSoundTouch->numSamples();
      if (outputCount > 0) {
         float *buffer2 = new float[outputCount];
         mSoundTouch->receiveSamples(buffer2, outputCount);
         outputTrack->Append((samplePtr)buffer2, floatSample, outputCount);
         delete[] buffer2;
      }

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (TrackProgress(mCurTrackNum, (s - start).as_double() / len))
         return false;
   }

   // Tell SoundTouch to finish processing any remaining samples
   mSoundTouch->flush();   // this should only be used for changeTempo - it dumps data otherwise with pRateTransposer->clear();

   unsigned int outputCount = mSoundTouch->numSamples();
   if (outputCount > 0) {
      float *buffer2 = new float[outputCount];
      mSoundTouch->receiveSamples(buffer2, outputCount);
      outputTrack->Append((samplePtr)buffer2, floatSample, outputCount);
      delete[] buffer2;
   }

   // Flush the output WaveTrack (since it's buffered, too)
   outputTrack->Flush();

   // Clean up the buffer
   delete[]buffer;

   // Take the output track and insert it in place of the original
   // sample data
   track->ClearAndPaste(mCurT0, mCurT1, outputTrack.get(), true, false, GetTimeWarper());

   double newLength = outputTrack->GetEndTime();
   m_maxNewLength = wxMax(m_maxNewLength, newLength);

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectSoundTouch::ProcessStereo(WaveTrack* leftTrack, WaveTrack* rightTrack,
                                     sampleCount start, sampleCount end)
{
   mSoundTouch->setSampleRate((unsigned int)(leftTrack->GetRate()+0.5));

   auto outputLeftTrack = mFactory->NewWaveTrack(leftTrack->GetSampleFormat(),
                                                       leftTrack->GetRate());
   auto outputRightTrack = mFactory->NewWaveTrack(rightTrack->GetSampleFormat(),
                                                        rightTrack->GetRate());

   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   double len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   // Make soundTouchBuffer twice as big as MaxBlockSize for each channel,
   // because Soundtouch wants them interleaved, i.e., each
   // Soundtouch sample is left-right pair.
   auto maxBlockSize = leftTrack->GetMaxBlockSize();
   float* leftBuffer = new float[maxBlockSize];
   float* rightBuffer = new float[maxBlockSize];
   float* soundTouchBuffer = new float[maxBlockSize * 2];

   // Go through the track one stereo buffer at a time.
   // sourceSampleCount counts the sample at which the current buffer starts,
   // per channel.
   auto sourceSampleCount = start;
   while (sourceSampleCount < end) {
      //Get a block of samples (smaller than the size of the buffer)
      //Adjust the block size if it is the final block in the track
      auto blockSize = limitSampleBufferSize(
         leftTrack->GetBestBlockSize(sourceSampleCount),
         end - sourceSampleCount
      );

      // Get the samples from the tracks and put them in the buffers.
      leftTrack->Get((samplePtr)(leftBuffer), floatSample, sourceSampleCount, blockSize);
      rightTrack->Get((samplePtr)(rightBuffer), floatSample, sourceSampleCount, blockSize);

      // Interleave into soundTouchBuffer.
      for (decltype(blockSize) index = 0; index < blockSize; index++) {
         soundTouchBuffer[index*2]       = leftBuffer[index];
         soundTouchBuffer[(index*2)+1]   = rightBuffer[index];
      }

      //Add samples to SoundTouch
      mSoundTouch->putSamples(soundTouchBuffer, blockSize);

      //Get back samples from SoundTouch
      unsigned int outputCount = mSoundTouch->numSamples();
      if (outputCount > 0)
         this->ProcessStereoResults(outputCount, outputLeftTrack.get(), outputRightTrack.get());

      //Increment sourceSampleCount one blockfull of samples
      sourceSampleCount += blockSize;

      //Update the Progress meter
      // mCurTrackNum is left track. Include right track.
      int nWhichTrack = mCurTrackNum;
      double frac = (sourceSampleCount - start).as_double() / len;
      if (frac < 0.5)
         frac *= 2.0; // Show twice as far for each track, because we're doing 2 at once.
      else
      {
         nWhichTrack++;
         frac -= 0.5;
         frac *= 2.0; // Show twice as far for each track, because we're doing 2 at once.
      }
      if (TrackProgress(nWhichTrack, frac))
         return false;
   }

   // Tell SoundTouch to finish processing any remaining samples
   mSoundTouch->flush();

   unsigned int outputCount = mSoundTouch->numSamples();
   if (outputCount > 0)
      this->ProcessStereoResults(outputCount, outputLeftTrack.get(), outputRightTrack.get());

   // Flush the output WaveTracks (since they're buffered, too)
   outputLeftTrack->Flush();
   outputRightTrack->Flush();

   // Clean up the buffers.
   delete [] leftBuffer;
   delete [] rightBuffer;
   delete [] soundTouchBuffer;

   // Take the output tracks and insert in place of the original
   // sample data.
   leftTrack->ClearAndPaste(mCurT0, mCurT1, outputLeftTrack.get(), true, false, GetTimeWarper());
   rightTrack->ClearAndPaste(mCurT0, mCurT1, outputRightTrack.get(), true, false, GetTimeWarper());

   // Track the longest result length
   double newLength = outputLeftTrack->GetEndTime();
   m_maxNewLength = wxMax(m_maxNewLength, newLength);
   newLength = outputRightTrack->GetEndTime();
   m_maxNewLength = wxMax(m_maxNewLength, newLength);

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectSoundTouch::ProcessStereoResults(const unsigned int outputCount,
                                            WaveTrack* outputLeftTrack,
                                            WaveTrack* outputRightTrack)
{
   float* outputSoundTouchBuffer = new float[outputCount*2];
   mSoundTouch->receiveSamples(outputSoundTouchBuffer, outputCount);

   // Dis-interleave outputSoundTouchBuffer into separate track buffers.
   float* outputLeftBuffer = new float[outputCount];
   float* outputRightBuffer = new float[outputCount];
   for (unsigned int index = 0; index < outputCount; index++)
   {
      outputLeftBuffer[index] = outputSoundTouchBuffer[index*2];
      outputRightBuffer[index] = outputSoundTouchBuffer[(index*2)+1];
   }

   outputLeftTrack->Append((samplePtr)outputLeftBuffer, floatSample, outputCount);
   outputRightTrack->Append((samplePtr)outputRightBuffer, floatSample, outputCount);

   delete[] outputSoundTouchBuffer;
   delete[] outputLeftBuffer;
   delete[] outputRightBuffer;

   return true;
}

#endif // USE_SOUNDTOUCH
