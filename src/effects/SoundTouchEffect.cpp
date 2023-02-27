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
#include "EffectOutputTracks.h"

#include <math.h>

#include "LabelTrack.h"
#include "SyncLock.h"
#include "WaveClip.h"
#include "WaveTrack.h"
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
//   SetTimeWarper(std::make_unique<RegionTimeWarper>(mT0, mT1,
 //           std::make_unique<LinearTimeWarper>(mT0, mT0,
   //            mT1, mT0 + (mT1 - mT0) * mFactor)));
   lt->WarpLabels(warper);
   return true;
}

#ifdef USE_MIDI
bool EffectSoundTouch::ProcessNoteTrack(NoteTrack *nt, const TimeWarper &warper)
{
   nt->WarpAndTransposeNotes(mT0, mT1, warper, mSemitones);
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
   EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } }, true };
   bool bGoodResult = true;

   mPreserveLength = preserveLength;
   mCurTrackNum = 0;
   m_maxNewLength = 0.0;

   outputs.Get().Any().VisitWhile(bGoodResult,
      [&](auto &&fallthrough){ return [&](LabelTrack &lt) {
         if ( !(lt.GetSelected() ||
                (mustSync && SyncLock::IsSyncLockSelected(&lt))) )
            return fallthrough();
         if (!ProcessLabelTrack(&lt, warper))
            bGoodResult = false;
      }; },
#ifdef USE_MIDI
      [&](auto &&fallthrough){ return [&](NoteTrack &nt) {
         if ( !(nt.GetSelected() || (mustSync && SyncLock::IsSyncLockSelected(&nt))) )
            return fallthrough();
         if (!ProcessNoteTrack(&nt, warper))
            bGoodResult = false;
      }; },
#endif
      [&](auto &&fallthrough){ return [&](WaveTrack &orig) {
         if (!orig.GetSelected())
            return fallthrough();

         // Process only if the right marker is to the right of the left marker
         if (mT1 > mT0) {
            //Transform the marker timepoints to samples
            const auto start = orig.TimeToLongSamples(mT0);
            const auto end = orig.TimeToLongSamples(mT1);

            const auto tempList = orig.WideEmptyCopy();
            auto &out = **tempList->Any<WaveTrack>().begin();

            const auto pSoundTouch = std::make_unique<soundtouch::SoundTouch>();
            initer(pSoundTouch.get());

            // TODO: more-than-two-channels
            auto channels = orig.Channels();
            if (channels.size() > 1) {

               //Inform soundtouch there's 2 channels
               pSoundTouch->setChannels(2);

               //ProcessStereo() (implemented below) processes a stereo track
               if (!ProcessStereo(pSoundTouch.get(),
                  orig, out, start, end, warper))
                  bGoodResult = false;
               mCurTrackNum++; // Increment for rightTrack, too.
            } else {
               //Inform soundtouch there's a single channel
               pSoundTouch->setChannels(1);

               //ProcessOne() (implemented below) processes a single track
               if (!ProcessOne(
                  pSoundTouch.get(), orig, out, start, end, warper))
                  bGoodResult = false;
            }
            // pSoundTouch is destroyed here
         }
         mCurTrackNum++;
      }; },
      [&](Track &t) {
         // Outer loop is over leaders, so fall-through must check for
         // multiple channels
         if (mustSync && SyncLock::IsSyncLockSelected(&t))
            t.SyncLockAdjust(mT1, warper.Warp(mT1));
      }
   );

   if (bGoodResult)
      outputs.Commit();

   return bGoodResult;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessSoundTouch on these blocks
bool EffectSoundTouch::ProcessOne(soundtouch::SoundTouch *pSoundTouch,
   WaveTrack &orig, WaveTrack &out,
   sampleCount start, sampleCount end,
   const TimeWarper &warper)
{
   pSoundTouch->setSampleRate(
      static_cast<unsigned int>((orig.GetRate() + 0.5)));

   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   auto len = (end - start).as_double();

   {
      //Initiate a processing buffer.  This buffer will (most likely)
      //be shorter than the length of the track being processed.
      Floats buffer{ orig.GetMaxBlockSize() };

      //Go through the track one buffer at a time. s counts which
      //sample the current buffer starts at.
      auto s = start;
      while (s < end) {
         //Get a block of samples (smaller than the size of the buffer)
         const auto block = std::min<size_t>(8192,
            limitSampleBufferSize(orig.GetBestBlockSize(s), end - s));

         //Get the samples from the track and put them in the buffer
         orig.GetFloats(buffer.get(), s, block);

         //Add samples to SoundTouch
         pSoundTouch->putSamples(buffer.get(), block);

         //Get back samples from SoundTouch
         unsigned int outputCount = pSoundTouch->numSamples();
         if (outputCount > 0) {
            Floats buffer2{ outputCount };
            pSoundTouch->receiveSamples(buffer2.get(), outputCount);
            out.Append((samplePtr)buffer2.get(), floatSample, outputCount);
         }

         //Increment s one blockfull of samples
         s += block;

         //Update the Progress meter
         if (TrackProgress(mCurTrackNum, (s - start).as_double() / len))
            return false;
      }

      // Tell SoundTouch to finish processing any remaining samples
      pSoundTouch->flush();   // this should only be used for changeTempo - it dumps data otherwise with pRateTransposer->clear();

      unsigned int outputCount = pSoundTouch->numSamples();
      if (outputCount > 0) {
         Floats buffer2{ outputCount };
         pSoundTouch->receiveSamples(buffer2.get(), outputCount);
         out.Append((samplePtr)buffer2.get(), floatSample, outputCount);
      }

      out.Flush();
   }

   // Transfer output samples to the original
   Finalize(orig, out, warper);

   double newLength = out.GetEndTime();
   m_maxNewLength = std::max(m_maxNewLength, newLength);

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectSoundTouch::ProcessStereo(soundtouch::SoundTouch *pSoundTouch,
   WaveTrack &orig, WaveTrack &outputTrack,
   sampleCount start, sampleCount end, const TimeWarper &warper)
{
   pSoundTouch->setSampleRate(
      static_cast<unsigned int>(orig.GetRate() + 0.5));

   auto channels = orig.Channels();
   auto &leftTrack = **channels.first++;
   auto &rightTrack = **channels.first;

   auto newChannels = outputTrack.Channels();
   auto &outputLeftTrack = **newChannels.first++;
   auto &outputRightTrack = **newChannels.first;

   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later
   double len = (end - start).as_double();

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   // Make soundTouchBuffer twice as big as MaxBlockSize for each channel,
   // because Soundtouch wants them interleaved, i.e., each
   // Soundtouch sample is left-right pair.
   auto maxBlockSize = orig.GetMaxBlockSize();
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
            orig.GetBestBlockSize(sourceSampleCount),
            end - sourceSampleCount
         );

         // Get the samples from the tracks and put them in the buffers.
         leftTrack.GetFloats((leftBuffer.get()), sourceSampleCount, blockSize);
         rightTrack
            .GetFloats((rightBuffer.get()), sourceSampleCount, blockSize);

         // Interleave into soundTouchBuffer.
         for (decltype(blockSize) index = 0; index < blockSize; index++) {
            soundTouchBuffer[index * 2] = leftBuffer[index];
            soundTouchBuffer[(index * 2) + 1] = rightBuffer[index];
         }

         //Add samples to SoundTouch
         pSoundTouch->putSamples(soundTouchBuffer.get(), blockSize);

         //Get back samples from SoundTouch
         unsigned int outputCount = pSoundTouch->numSamples();
         if (outputCount > 0)
            this->ProcessStereoResults(pSoundTouch,
               outputCount, outputLeftTrack, outputRightTrack);

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
      pSoundTouch->flush();

      unsigned int outputCount = pSoundTouch->numSamples();
      if (outputCount > 0)
         this->ProcessStereoResults(pSoundTouch,
            outputCount, outputLeftTrack, outputRightTrack);

      outputTrack.Flush();
   }

   // Transfer output samples to the original
   Finalize(orig, outputTrack, warper);


   // Track the longest result length
   double newLength = outputTrack.GetEndTime();
   m_maxNewLength = std::max(m_maxNewLength, newLength);

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectSoundTouch::ProcessStereoResults(soundtouch::SoundTouch *pSoundTouch,
   const size_t outputCount,
   WaveChannel &outputLeftTrack,
   WaveChannel &outputRightTrack)
{
   Floats outputSoundTouchBuffer{ outputCount * 2 };
   pSoundTouch->receiveSamples(outputSoundTouchBuffer.get(), outputCount);

   // Dis-interleave outputSoundTouchBuffer into separate track buffers.
   Floats outputLeftBuffer{ outputCount };
   Floats outputRightBuffer{ outputCount };
   for (unsigned int index = 0; index < outputCount; ++index) {
      outputLeftBuffer[index] = outputSoundTouchBuffer[index * 2];
      outputRightBuffer[index] = outputSoundTouchBuffer[(index * 2) + 1];
   }

   outputLeftTrack.Append(
      (samplePtr)outputLeftBuffer.get(), floatSample, outputCount);
   outputRightTrack.Append(
      (samplePtr)outputRightBuffer.get(), floatSample, outputCount);

   return true;
}

void EffectSoundTouch::Finalize(
   WaveTrack &orig, WaveTrack &out, const TimeWarper &warper)
{
   assert(orig.IsLeader());
   assert(out.IsLeader());
   assert(out.NChannels() == orig.NChannels());
   if (mPreserveLength) {
      auto newLen = out.GetVisibleSampleCount();
      auto oldLen = out.TimeToLongSamples(mT1) - out.TimeToLongSamples(mT0);

      // Pad output track to original length since SoundTouch may remove samples
      if (newLen < oldLen) {
         const auto t = out.LongSamplesToTime(newLen - 1);
         const auto len = out.LongSamplesToTime(oldLen - newLen);
         out.InsertSilence(t, len);
      }
      // Trim output track to original length since SoundTouch may add extra samples
      else if (newLen > oldLen) {
         const auto t1 = out.LongSamplesToTime(oldLen);
         out.Trim(0, t1);
      }
   }

   // Silenced samples will be inserted in gaps between clips, so capture where
   // these gaps are for later deletion
   std::vector<std::pair<double, double>> gaps;
   double last = mT0;
   auto clips = orig.SortedClipArray();
   auto front = clips.front();
   auto back = clips.back();
   for (auto &clip : clips) {
      auto st = clip->GetPlayStartTime();
      auto et = clip->GetPlayEndTime();

      if (st >= mT0 || et < mT1) {
         if (mT0 < st && clip == front) {
            gaps.push_back(std::make_pair(mT0, st));
         }
         else if (last < st && mT0 <= last ) {
            gaps.push_back(std::make_pair(last, st));
         }

         if (et < mT1 && clip == back) {
            gaps.push_back(std::make_pair(et, mT1));
         }
      }
      last = et;
   }

   // Take the output track and insert it in place of the original sample data
   orig.ClearAndPaste(mT0, mT1, out, true, true, &warper);

   // Finally, recreate the gaps
   for (auto gap : gaps) {
      const auto st = orig.SnapToSample(gap.first);
      const auto et = orig.SnapToSample(gap.second);
      if (st >= mT0 && et <= mT1 && st != et)
         orig.SplitDelete(warper.Warp(st), warper.Warp(et));
   }
}

#endif // USE_SOUNDTOUCH
