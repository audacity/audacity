/**********************************************************************

Audacity: A Digital Audio Editor

SoundTouchEffect.cpp

Dominic Mazzoni, Vaughan Johnson

This abstract class contains all of the common code for an
effect that uses SoundTouch to do its processing (ChangeTempo
                                                  and ChangePitch).

**********************************************************************/



#if USE_SOUNDTOUCH
#include "SoundTouchEffect.h"

#include <math.h>

#include "../LabelTrack.h"
#include "../WaveClip.h"
#include "../WaveTrack.h"
#include "../NoteTrack.h"
#include "TimeWarper.h"

// Soundtouch defines these as well, which are also in generated configmac.h
// and configunix.h, so get rid of them before including,
// to avoid compiler warnings, and be sure to do this
// after all other #includes, to avoid any mischief that might result
// from doing the un-definitions before seeing any wx headers.
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_BUGREPORT
#undef PACKAGE
#undef VERSION
#include "SoundTouch.h"

#ifdef USE_MIDI
EffectSoundTouch::EffectSoundTouch()
{
   mSemitones = 0;
}
#endif

EffectSoundTouch::~EffectSoundTouch()
{
}

bool EffectSoundTouch::ProcessLabelTrack(
   LabelTrack *lt, const TimeWarper &warper)
{
//   SetTimeWarper(std::make_unique<RegionTimeWarper>(mCurT0, mCurT1,
 //           std::make_unique<LinearTimeWarper>(mCurT0, mCurT0,
   //            mCurT1, mCurT0 + (mCurT1-mCurT0)*mFactor)));
   lt->WarpLabels(warper);
   return true;
}

#ifdef USE_MIDI
bool EffectSoundTouch::ProcessNoteTrack(NoteTrack *nt, const TimeWarper &warper)
{
   nt->WarpAndTransposeNotes(mCurT0, mCurT1, warper, mSemitones);
   return true;
}
#endif

bool EffectSoundTouch::ProcessWithTimeWarper(InitFunction initer,
                                             const TimeWarper &warper,
                                             bool preserveLength)
{
   // Assumes that mSoundTouch has already been initialized
   // by the subclass for subclass-specific parameters. The
   // time warper should also be set.

   // Check if this effect will alter the selection length; if so, we need
   // to operate on sync-lock selected tracks.
   bool mustSync = true;
   if (mT1 == warper.Warp(mT1)) {
      mustSync = false;
   }

   //Iterate over each track
   // Needs all for sync-lock grouping.
   this->CopyInputTracks(true);
   bool bGoodResult = true;

   mPreserveLength = preserveLength;
   mCurTrackNum = 0;
   m_maxNewLength = 0.0;

   mOutputTracks->Leaders().VisitWhile( bGoodResult,
      [&]( LabelTrack *lt, const Track::Fallthrough &fallthrough ) {
         if ( !(lt->GetSelected() || (mustSync && lt->IsSyncLockSelected())) )
            return fallthrough();
         if (!ProcessLabelTrack(lt, warper))
            bGoodResult = false;
      },
#ifdef USE_MIDI
      [&]( NoteTrack *nt, const Track::Fallthrough &fallthrough ) {
         if ( !(nt->GetSelected() || (mustSync && nt->IsSyncLockSelected())) )
            return fallthrough();
         if (!ProcessNoteTrack(nt, warper))
            bGoodResult = false;
      },
#endif
      [&]( WaveTrack *leftTrack, const Track::Fallthrough &fallthrough ) {
         if (!leftTrack->GetSelected())
            return fallthrough();

         //Get start and end times from selection
         mCurT0 = mT0;
         mCurT1 = mT1;

         //Set the current bounds to whichever left marker is
         //greater and whichever right marker is less
         mCurT0 = wxMax(mT0, mCurT0);
         mCurT1 = wxMin(mT1, mCurT1);

         // Process only if the right marker is to the right of the left marker
         if (mCurT1 > mCurT0) {
            mSoundTouch = std::make_unique<soundtouch::SoundTouch>();
            initer(mSoundTouch.get());

            // TODO: more-than-two-channels
            auto channels = TrackList::Channels(leftTrack);
            auto rightTrack = (channels.size() > 1)
               ? * ++ channels.first
               : nullptr;
            if ( rightTrack ) {
               double t;

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
               if (!ProcessStereo(leftTrack, rightTrack, start, end, warper))
                  bGoodResult = false;
               mCurTrackNum++; // Increment for rightTrack, too.
            } else {
               //Transform the marker timepoints to samples
               auto start = leftTrack->TimeToLongSamples(mCurT0);
               auto end = leftTrack->TimeToLongSamples(mCurT1);

               //Inform soundtouch there's a single channel
               mSoundTouch->setChannels(1);

               //ProcessOne() (implemented below) processes a single track
               if (!ProcessOne(leftTrack, start, end, warper))
                  bGoodResult = false;
            }

            mSoundTouch.reset();
         }
         mCurTrackNum++;
      },
      [&]( Track *t ) {
         if (mustSync && t->IsSyncLockSelected()) {
            t->SyncLockAdjust(mT1, warper.Warp(mT1));
         }
      }
   );

   if (bGoodResult) {
      ReplaceProcessedTracks(bGoodResult);
   }

   return bGoodResult;
}

void EffectSoundTouch::End()
{
   mSoundTouch.reset();
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessSoundTouch on these blocks
bool EffectSoundTouch::ProcessOne(WaveTrack *track,
                                  sampleCount start, sampleCount end,
                                  const TimeWarper &warper)
{
   mSoundTouch->setSampleRate((unsigned int)(track->GetRate()+0.5));

   auto outputTrack = track->EmptyCopy();

   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   {
      //Initiate a processing buffer.  This buffer will (most likely)
      //be shorter than the length of the track being processed.
      Floats buffer{ track->GetMaxBlockSize() };

      //Go through the track one buffer at a time. s counts which
      //sample the current buffer starts at.
      auto s = start;
      while (s < end) {
         //Get a block of samples (smaller than the size of the buffer)
         const auto block = wxMin(8192,
            limitSampleBufferSize( track->GetBestBlockSize(s), end - s ));

         //Get the samples from the track and put them in the buffer
         track->GetFloats(buffer.get(), s, block);

         //Add samples to SoundTouch
         mSoundTouch->putSamples(buffer.get(), block);

         //Get back samples from SoundTouch
         unsigned int outputCount = mSoundTouch->numSamples();
         if (outputCount > 0) {
            Floats buffer2{ outputCount };
            mSoundTouch->receiveSamples(buffer2.get(), outputCount);
            outputTrack->Append((samplePtr)buffer2.get(), floatSample, outputCount);
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
         Floats buffer2{ outputCount };
         mSoundTouch->receiveSamples(buffer2.get(), outputCount);
         outputTrack->Append((samplePtr)buffer2.get(), floatSample, outputCount);
      }

      // Flush the output WaveTrack (since it's buffered, too)
      outputTrack->Flush();
   }

   // Transfer output samples to the original
   Finalize(track, outputTrack.get(), warper);

   double newLength = outputTrack->GetEndTime();
   m_maxNewLength = wxMax(m_maxNewLength, newLength);

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectSoundTouch::ProcessStereo(
   WaveTrack* leftTrack, WaveTrack* rightTrack,
   sampleCount start, sampleCount end, const TimeWarper &warper)
{
   mSoundTouch->setSampleRate((unsigned int)(leftTrack->GetRate() + 0.5));

   auto outputLeftTrack = leftTrack->EmptyCopy();
   auto outputRightTrack = rightTrack->EmptyCopy();

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
   {
      Floats leftBuffer{ maxBlockSize };
      Floats rightBuffer{ maxBlockSize };
      Floats soundTouchBuffer{ maxBlockSize * 2 };

      // Go through the track one stereo buffer at a time.
      // sourceSampleCount counts the sample at which the current buffer starts,
      // per channel.
      auto sourceSampleCount = start;
      while (sourceSampleCount < end) {
         auto blockSize = limitSampleBufferSize(
            leftTrack->GetBestBlockSize(sourceSampleCount),
            end - sourceSampleCount
         );

         // Get the samples from the tracks and put them in the buffers.
         leftTrack->GetFloats((leftBuffer.get()), sourceSampleCount, blockSize);
         rightTrack->GetFloats((rightBuffer.get()), sourceSampleCount, blockSize);

         // Interleave into soundTouchBuffer.
         for (decltype(blockSize) index = 0; index < blockSize; index++) {
            soundTouchBuffer[index * 2] = leftBuffer[index];
            soundTouchBuffer[(index * 2) + 1] = rightBuffer[index];
         }

         //Add samples to SoundTouch
         mSoundTouch->putSamples(soundTouchBuffer.get(), blockSize);

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
   }

   // Transfer output samples to the original
   Finalize(leftTrack, outputLeftTrack.get(), warper);
   Finalize(rightTrack, outputRightTrack.get(), warper);


   // Track the longest result length
   double newLength = outputLeftTrack->GetEndTime();
   m_maxNewLength = wxMax(m_maxNewLength, newLength);
   newLength = outputRightTrack->GetEndTime();
   m_maxNewLength = wxMax(m_maxNewLength, newLength);

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectSoundTouch::ProcessStereoResults(const size_t outputCount,
                                            WaveTrack* outputLeftTrack,
                                            WaveTrack* outputRightTrack)
{
   Floats outputSoundTouchBuffer{ outputCount * 2 };
   mSoundTouch->receiveSamples(outputSoundTouchBuffer.get(), outputCount);

   // Dis-interleave outputSoundTouchBuffer into separate track buffers.
   Floats outputLeftBuffer{ outputCount };
   Floats outputRightBuffer{ outputCount };
   for (unsigned int index = 0; index < outputCount; index++)
   {
      outputLeftBuffer[index] = outputSoundTouchBuffer[index*2];
      outputRightBuffer[index] = outputSoundTouchBuffer[(index*2)+1];
   }

   outputLeftTrack->Append((samplePtr)outputLeftBuffer.get(), floatSample, outputCount);
   outputRightTrack->Append((samplePtr)outputRightBuffer.get(), floatSample, outputCount);

   return true;
}

void EffectSoundTouch::Finalize(WaveTrack* orig, WaveTrack* out, const TimeWarper &warper)
{
   if (mPreserveLength) {
      auto newLen = out->GetPlaySamplesCount();
      auto oldLen = out->TimeToLongSamples(mCurT1) - out->TimeToLongSamples(mCurT0);

      // Pad output track to original length since SoundTouch may remove samples
      if (newLen < oldLen) {
         out->InsertSilence(out->LongSamplesToTime(newLen - 1),
                                 out->LongSamplesToTime(oldLen - newLen));
      }
      // Trim output track to original length since SoundTouch may add extra samples
      else if (newLen > oldLen) {
         out->Trim(0, out->LongSamplesToTime(oldLen));
      }
   }

   // Silenced samples will be inserted in gaps between clips, so capture where these
   // gaps are for later deletion
   std::vector<std::pair<double, double>> gaps;
   double last = mCurT0;
   auto clips = orig->SortedClipArray();
   auto front = clips.front();
   auto back = clips.back();
   for (auto &clip : clips) {
      auto st = clip->GetPlayStartTime();
      auto et = clip->GetPlayEndTime();

      if (st >= mCurT0 || et < mCurT1) {
         if (mCurT0 < st && clip == front) {
            gaps.push_back(std::make_pair(mCurT0, st));
         }
         else if (last < st && mCurT0 <= last ) {
            gaps.push_back(std::make_pair(last, st));
         }

         if (et < mCurT1 && clip == back) {
            gaps.push_back(std::make_pair(et, mCurT1));
         }
      }
      last = et;
   }

   // Take the output track and insert it in place of the original sample data
   orig->ClearAndPaste(mCurT0, mCurT1, out, true, true, &warper);

   // Finally, recreate the gaps
   for (auto gap : gaps) {
      auto st = orig->LongSamplesToTime(orig->TimeToLongSamples(gap.first));
      auto et = orig->LongSamplesToTime(orig->TimeToLongSamples(gap.second));
      if (st >= mCurT0 && et <= mCurT1 && st != et)
      {
         orig->SplitDelete(warper.Warp(st), warper.Warp(et));
      }
   }
}

#endif // USE_SOUNDTOUCH
