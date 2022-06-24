/**********************************************************************

  Audacity: A Digital Audio Editor

  PerTrackEffect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from Effect.cpp

*******************************************************************//**

\class PerTrackEffect
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/


#include "PerTrackEffect.h"

#include "TimeWarper.h"
#include "../SyncLock.h"
#include "ViewInfo.h"
#include "../WaveTrack.h"

PerTrackEffect::Instance::~Instance() = default;

bool PerTrackEffect::Instance::Process(EffectSettings &settings)
{
   return mProcessor.Process(*this, settings);
}

bool PerTrackEffect::Instance::ProcessInitialize(EffectSettings &,
   double, sampleCount, ChannelNames)
{
   return true;
}

bool PerTrackEffect::Instance::ProcessFinalize() /* noexcept */
{
   return true;
}

sampleCount PerTrackEffect::Instance::GetLatency(
   const EffectSettings &, double)
{
   return 0;
}

PerTrackEffect::~PerTrackEffect() = default;

bool PerTrackEffect::DoPass1() const
{
   return true;
}

bool PerTrackEffect::DoPass2() const
{
   return false;
}

bool PerTrackEffect::Process(
   EffectInstance &instance, EffectSettings &settings) const
{
   auto pThis = const_cast<PerTrackEffect *>(this);
   pThis->CopyInputTracks(true);
   bool bGoodResult = true;
   // mPass = 1;
   if (DoPass1()) {
      auto &myInstance = dynamic_cast<Instance&>(instance);
      bGoodResult = pThis->ProcessPass(myInstance, settings);
      // mPass = 2;
      if (bGoodResult && DoPass2())
         bGoodResult = pThis->ProcessPass(myInstance, settings);
   }
   pThis->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

bool PerTrackEffect::ProcessPass(Instance &instance, EffectSettings &settings)
{
   const auto duration = settings.extra.GetDuration();
   bool bGoodResult = true;
   bool isGenerator = GetType() == EffectTypeGenerate;

   FloatBuffers inBuffer, outBuffer;
   ArrayOf<float *> inBufPos, outBufPos;
   ChannelName map[3];
   size_t bufferSize = 0;
   size_t blockSize = 0;
   int count = 0;
   bool clear = false;

   // It's possible that the number of channels the effect expects changed based on
   // the parameters (the Audacity Reverb effect does when the stereo width is 0).
   const auto numAudioIn = GetAudioInCount();
   const auto numAudioOut = GetAudioOutCount();
   const bool multichannel = numAudioIn > 1;
   auto range = multichannel
      ? mOutputTracks->Leaders()
      : mOutputTracks->Any();
   range.VisitWhile( bGoodResult,
      [&](WaveTrack *left, const Track::Fallthrough &fallthrough) {
         if (!left->GetSelected())
            return fallthrough();

         sampleCount len = 0;
         sampleCount start = 0;
         unsigned numChannels = 0;
         WaveTrack *right{};

         // Iterate either over one track which could be any channel,
         // or if multichannel, then over all channels of left,
         // which is a leader.
         for (auto channel :
              TrackList::Channels(left).StartingWith(left)) {
            if (channel->GetChannel() == Track::LeftChannel)
               map[numChannels] = ChannelNameFrontLeft;
            else if (channel->GetChannel() == Track::RightChannel)
               map[numChannels] = ChannelNameFrontRight;
            else
               map[numChannels] = ChannelNameMono;
            ++ numChannels;
            map[numChannels] = ChannelNameEOL;
            if (! multichannel)
               break;
            if (numChannels == 2) {
               // TODO: more-than-two-channels
               right = channel;
               clear = false;
               // Ignore other channels
               break;
            }
         }

         if (!isGenerator) {
            GetBounds(*left, right, &start, &len);
            mSampleCnt = len;
         }
         else
            mSampleCnt = left->TimeToLongSamples(duration);

         const auto sampleRate = left->GetRate();

         // Get the block size the client wants to use
         auto max = left->GetMaxBlockSize() * 2;
         blockSize = instance.SetBlockSize(max);

         // Calculate the buffer size to be at least the max rounded up to the clients
         // selected block size.
         const auto prevBufferSize = bufferSize;
         bufferSize = ((max + (blockSize - 1)) / blockSize) * blockSize;

         // If the buffer size has changed, then (re)allocate the buffers
         if (prevBufferSize != bufferSize) {
            // Always create the number of input buffers the client expects even if we don't have
            // the same number of channels.
            inBufPos.reinit( numAudioIn );
            inBuffer.reinit( numAudioIn, bufferSize );

            // We won't be using more than the first 2 buffers, so clear the rest (if any)
            for (size_t i = 2; i < numAudioIn; i++)
               for (size_t j = 0; j < bufferSize; j++)
                  inBuffer[i][j] = 0.0;

            // Always create the number of output buffers the client expects even if we don't have
            // the same number of channels.
            outBufPos.reinit( numAudioOut );
            // Output buffers get an extra blockSize worth to give extra room if
            // the plugin adds latency
            outBuffer.reinit( numAudioOut, bufferSize + blockSize );
         }

         // (Re)Set the input buffer positions
         for (size_t i = 0; i < numAudioIn; i++)
            inBufPos[i] = inBuffer[i].get();

         // (Re)Set the output buffer positions
         for (size_t i = 0; i < numAudioOut; i++)
            outBufPos[i] = outBuffer[i].get();

         // Clear unused input buffers
         if (!right && !clear && numAudioIn > 1) {
            for (size_t j = 0; j < bufferSize; j++)
               inBuffer[1][j] = 0.0;
            clear = true;
         }

         // Go process the track(s)
         bGoodResult = ProcessTrack(instance, settings, sampleRate,
            count, map, left, right, start, len,
            inBuffer, outBuffer, inBufPos, outBufPos, bufferSize, blockSize,
            numChannels);
         if (!bGoodResult)
            return;

         count++;
      },
      [&](Track *t) {
         if (SyncLock::IsSyncLockSelected(t))
            t->SyncLockAdjust(mT1, mT0 + duration);
      }
   );

   if (bGoodResult && GetType() == EffectTypeGenerate)
      mT1 = mT0 + duration;

   return bGoodResult;
}

bool PerTrackEffect::ProcessTrack(Instance &instance, EffectSettings &settings,
   double sampleRate, int count, ChannelNames map,
   WaveTrack *left, WaveTrack *right,
   sampleCount start, sampleCount len,
   FloatBuffers &inBuffer, FloatBuffers &outBuffer,
   ArrayOf< float * > &inBufPos, ArrayOf< float *> &outBufPos,
   size_t bufferSize, size_t blockSize,
   unsigned numChannels) const
{
   bool rc = true;

   // Give the plugin a chance to initialize
   if (!instance.ProcessInitialize(settings, sampleRate, len, map))
      return false;

   { // Start scope for cleanup
   auto cleanup = finally( [&] {
      // Allow the plugin to cleanup
      if (!instance.ProcessFinalize())
         // In case of non-exceptional flow of control, set rc
         rc = false;
   } );

   // For each input block of samples, we pass it to the effect along with a
   // variable output location.  This output location is simply a pointer into a
   // much larger buffer.  This reduces the number of calls required to add the
   // samples to the output track.
   //
   // Upon return from the effect, the output samples are "moved to the left" by
   // the number of samples in the current latency setting, effectively removing any
   // delay introduced by the effect.
   //
   // At the same time the total number of delayed samples are gathered and when
   // there is no further input data to process, the loop continues to call the
   // effect with an empty input buffer until the effect has had a chance to
   // return all of the remaining delayed samples.
   auto inPos = start;
   auto outPos = start;
   auto inputRemaining = len;
   decltype(instance.GetLatency(settings, sampleRate))
      curDelay = 0, delayRemaining = 0;
   decltype(blockSize) curBlockSize = 0;
   decltype(bufferSize) inputBufferCnt = 0;
   decltype(bufferSize) outputBufferCnt = 0;
   bool cleared = false;
   auto chans = std::min<unsigned>(GetAudioOutCount(), numChannels);
   std::shared_ptr<WaveTrack> genLeft, genRight;
   decltype(len) genLength = 0;
   bool isGenerator = GetType() == EffectTypeGenerate;
   bool isProcessor = GetType() == EffectTypeProcess;
   double genDur = 0;
   if (isGenerator) {
      const auto duration = settings.extra.GetDuration();
      if (IsPreviewing()) {
         gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &genDur, 6.0);
         genDur = std::min(duration, CalcPreviewInputLength(settings, genDur));
      }
      else
         genDur = duration;
      genLength = sampleCount((left->GetRate() * genDur) + 0.5);  // round to nearest sample
      delayRemaining = genLength;
      cleared = true;

      // Create temporary tracks
      genLeft = left->EmptyCopy();

      if (right)
         genRight = right->EmptyCopy();
   }

   // Call the effect until we run out of input or delayed samples
   while (inputRemaining != 0 || delayRemaining != 0) {
      // Still working on the input samples
      if (inputRemaining != 0) {
         // Need to refill the input buffers
         if (inputBufferCnt == 0) {
            // Calculate the number of samples to get
            inputBufferCnt =
               limitSampleBufferSize( bufferSize, inputRemaining );

            // Fill the input buffers
            left->GetFloats(inBuffer[0].get(), inPos, inputBufferCnt);
            if (right)
               right->GetFloats(inBuffer[1].get(), inPos, inputBufferCnt);

            // Reset the input buffer positions
            for (size_t i = 0; i < numChannels; i++)
               inBufPos[i] = inBuffer[i].get();
         }

         // Calculate the number of samples to process
         curBlockSize = blockSize;
         if (curBlockSize > inputRemaining) {
            // We've reached the last block...set current block size to what's left
            // inputRemaining is positive and bounded by a size_t
            curBlockSize = inputRemaining.as_size_t();
            inputRemaining = 0;

            // Clear the remainder of the buffers so that a full block can be passed
            // to the effect
            auto cnt = blockSize - curBlockSize;
            for (size_t i = 0; i < numChannels; i++)
               for (decltype(cnt) j = 0 ; j < cnt; j++)
                  inBufPos[i][j + curBlockSize] = 0.0;

            // Might be able to use up some of the delayed samples
            if (delayRemaining != 0) {
               // Don't use more than needed
               cnt = limitSampleBufferSize(cnt, delayRemaining);
               delayRemaining -= cnt;
               curBlockSize += cnt;
            }
         }
      }
      // We've exhausted the input samples and are now working on the delay
      else if (delayRemaining != 0) {
         // Calculate the number of samples to process
         curBlockSize = limitSampleBufferSize( blockSize, delayRemaining );
         delayRemaining -= curBlockSize;

         // From this point on, we only want to feed zeros to the plugin
         if (!cleared) {
            // Reset the input buffer positions
            for (size_t i = 0; i < numChannels; i++) {
               inBufPos[i] = inBuffer[i].get();
               // And clear
               for (size_t j = 0; j < blockSize; j++)
                  inBuffer[i][j] = 0.0;
            }
            cleared = true;
         }
      }

      // Finally call the plugin to process the block
      decltype(curBlockSize) processed;
      try {
         processed = instance.ProcessBlock(
            settings, inBufPos.get(), outBufPos.get(), curBlockSize);
      }
      catch( const AudacityException & WXUNUSED(e) ) {
         // PRL: Bug 437:
         // Pass this along to our application-level handler
         throw;
      }
      catch(...) {
         // PRL:
         // Exceptions for other reasons, maybe in third-party code...
         // Continue treating them as we used to, but I wonder if these
         // should now be treated the same way.
         return false;
      }
      if (processed != curBlockSize)
         return false;
      wxUnusedVar(processed);

      // Bump to next input buffer position
      if (inputRemaining != 0) {
         for (size_t i = 0; i < numChannels; i++)
            inBufPos[i] += curBlockSize;
         inputRemaining -= curBlockSize;
         inputBufferCnt -= curBlockSize;
      }

      // "ls" and "rs" serve as the input sample index for the left and
      // right channels when processing the input samples.  If we flip
      // over to processing delayed samples, they simply become counters
      // for the progress display.
      inPos += curBlockSize;

      // Get the current number of delayed samples and accumulate
      if (isProcessor) {
         {
            auto delay = instance.GetLatency(settings, sampleRate);
            curDelay += delay;
            delayRemaining += delay;
         }

         // If the plugin has delayed the output by more samples than our current
         // block size, then we leave the output pointers alone.  This effectively
         // removes those delayed samples from the output buffer.
         if (curDelay >= curBlockSize) {
            curDelay -= curBlockSize;
            curBlockSize = 0;
         }
         // We have some delayed samples, at the beginning of the output samples,
         // so overlay them by shifting the remaining output samples.
         else if (curDelay > 0) {
            // curDelay is bounded by curBlockSize:
            auto delay = curDelay.as_size_t();
            curBlockSize -= delay;
            for (size_t i = 0; i < chans; i++)
               memmove(outBufPos[i], outBufPos[i] + delay, sizeof(float) * curBlockSize);
            curDelay = 0;
         }
      }

      // Adjust the number of samples in the output buffers
      outputBufferCnt += curBlockSize;

      // Still have room in the output buffers
      if (outputBufferCnt < bufferSize) {
         // Bump to next output buffer position
         for (size_t i = 0; i < chans; i++)
            outBufPos[i] += curBlockSize;
      }
      // Output buffers have filled
      else {
         if (isProcessor) {
            // Write them out
            left->Set((samplePtr) outBuffer[0].get(), floatSample, outPos, outputBufferCnt);
            if (right) {
               if (chans >= 2)
                  right->Set((samplePtr) outBuffer[1].get(), floatSample, outPos, outputBufferCnt);
               else
                  right->Set((samplePtr) outBuffer[0].get(), floatSample, outPos, outputBufferCnt);
            }
         }
         else if (isGenerator) {
            genLeft->Append((samplePtr) outBuffer[0].get(), floatSample, outputBufferCnt);
            if (genRight)
               genRight->Append((samplePtr) outBuffer[1].get(), floatSample, outputBufferCnt);
         }

         // Reset the output buffer positions
         for (size_t i = 0; i < chans; i++)
            outBufPos[i] = outBuffer[i].get();

         // Bump to the next track position
         outPos += outputBufferCnt;
         outputBufferCnt = 0;
      }

      if (numChannels > 1) {
         if (TrackGroupProgress(count,
               (inPos - start).as_double() /
               (isGenerator ? genLength : len).as_double())) {
            rc = false;
            break;
         }
      }
      else {
         if (TrackProgress(count,
               (inPos - start).as_double() /
               (isGenerator ? genLength : len).as_double())) {
            rc = false;
            break;
         }
      }
   }

   // Put any remaining output
   if (rc && outputBufferCnt) {
      if (isProcessor) {
         left->Set((samplePtr) outBuffer[0].get(), floatSample, outPos, outputBufferCnt);
         if (right) {
            if (chans >= 2)
               right->Set((samplePtr) outBuffer[1].get(), floatSample, outPos, outputBufferCnt);
            else
               right->Set((samplePtr) outBuffer[0].get(), floatSample, outPos, outputBufferCnt);
         }
      }
      else if (isGenerator) {
         genLeft->Append((samplePtr) outBuffer[0].get(), floatSample, outputBufferCnt);
         if (genRight)
            genRight->Append((samplePtr) outBuffer[1].get(), floatSample, outputBufferCnt);
      }
   }

   if (rc && isGenerator) {
      auto pProject = FindProject();

      // Transfer the data from the temporary tracks to the actual ones
      genLeft->Flush();
      // mT1 gives us the NEW selection. We want to replace up to GetSel1().
      auto &selectedRegion = ViewInfo::Get( *pProject ).selectedRegion;
      auto t1 = selectedRegion.t1();
      PasteTimeWarper warper{ t1, mT0 + genLeft->GetEndTime() };
      left->ClearAndPaste(mT0, t1, genLeft.get(), true, true,
         &warper);

      if (genRight) {
         genRight->Flush();
         right->ClearAndPaste(mT0, selectedRegion.t1(),
            genRight.get(), true, true, nullptr /* &warper */);
      }
   }

   } // End scope for cleanup
   return rc;
}
