/**********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectStage.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from PerTrackEffect.cpp

**********************************************************************/


#include "EffectStage.h"
#include "AudacityException.h"
#include "AudioGraphBuffers.h"
#include <cassert>

AudioGraph::EffectStage::EffectStage(Source &upstream, Buffers &inBuffers,
   Instance &instance, EffectSettings &settings, double sampleRate,
   std::optional<sampleCount> genLength, ChannelNames map
)  : mUpstream{ upstream }, mInBuffers{ inBuffers }
   , mInstance{ instance }, mSettings{ settings }, mSampleRate{ sampleRate }
   , mIsProcessor{ !genLength.has_value() }
   , mDelayRemaining{ genLength ? *genLength : sampleCount::max() }
{
   assert(upstream.AcceptsBlockSize(inBuffers.BlockSize()));
   // Give the plugin a chance to initialize
   if (!mInstance.ProcessInitialize(mSettings, mSampleRate, map))
      throw std::exception{};
   assert(this->AcceptsBlockSize(inBuffers.BlockSize()));

   // Establish invariant
   mInBuffers.Rewind();
   if (!mIsProcessor)
      // maybe not needed, but harmless
      FinishUpstream();
}

AudioGraph::EffectStage::~EffectStage()
{
   // Allow the plugin to cleanup
   mInstance.ProcessFinalize();
}

bool AudioGraph::EffectStage::AcceptsBuffers(const Buffers &buffers) const
{
   return true;
}

bool AudioGraph::EffectStage::AcceptsBlockSize(size_t size) const
{
   // Test the equality of input and output block sizes
   return mInBuffers.BlockSize() == size;
}

std::optional<size_t>
AudioGraph::EffectStage::Acquire(Buffers &data, size_t bound)
{
   assert(AcceptsBuffers(data));
   assert(AcceptsBlockSize(data.BlockSize()));
   // pre, needed for Process() and Discard()
   assert(bound <= std::min(data.BlockSize(), data.Remaining()));

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

   // Invariant satisfies pre for mUpstream.Acquire() and for Process()
   assert(mInBuffers.BlockSize() <= mInBuffers.Remaining());

   size_t curBlockSize = 0;

   if (auto oCurBlockSize = FetchProcessAndAdvance(data, bound, false)
      ; !oCurBlockSize
   )
      return {};
   else {
      curBlockSize = *oCurBlockSize;
      if (mIsProcessor && !mLatencyDone) {
         // Come here only in the first call to Acquire()
         // Some effects (like ladspa/lv2 swh plug-ins) don't report latency
         // until at least one block of samples is processed.  Find latency
         // once only for the track and assume it doesn't vary
         mDelayRemaining = mDelay =
            mInstance.GetLatency(mSettings, mSampleRate);
         // Discard all the latency
         auto delay = mDelay;
         while (delay > 0 && curBlockSize > 0) {
            auto discard = limitSampleBufferSize(curBlockSize, delay);
            data.Discard(discard, curBlockSize);
            delay -= discard;
            curBlockSize -= discard;
            if (curBlockSize == 0) {
               if (auto oCurBlockSize =
                  FetchProcessAndAdvance(data, bound, false)
               )
                  curBlockSize = *oCurBlockSize;
               else
                  return {};
            }
            mLastProduced -= discard;
         }
         while (delay > 0) {
            assert(curBlockSize == 0);
            // Finish one-time delay in case it exceeds entire upstream length
            // Upstream must have been exhausted
            assert(mUpstream.Remaining() == 0);
            // Feed zeroes to the effect
            auto zeroes = limitSampleBufferSize(data.BlockSize(), delay);
            FetchProcessAndAdvance(data, zeroes, true);
            delay -= zeroes;
         }
         mLatencyDone = true;
      }
   }

   if (curBlockSize < bound) {
      // Continue feeding zeroes; this code block will produce as many zeroes
      // at the end as were discarded at the beginning
      auto zeroes =
         limitSampleBufferSize(bound - curBlockSize, mDelayRemaining);
      FetchProcessAndAdvance(data, zeroes, true, curBlockSize);
      mDelayRemaining -= zeroes;
   }

   auto result = mLastProduced + mLastZeroes;
   // assert the post
   assert(data.Remaining() > 0);
   assert(result <= bound);
   assert(result <= data.Remaining());
   assert(result <= Remaining());
   assert(bound == 0 || Remaining() == 0 || result > 0);
   return { result };
}

std::optional<size_t> AudioGraph::EffectStage::FetchProcessAndAdvance(
   Buffers &data, size_t bound, bool doZeroes, size_t outBufferOffset)
{
   std::optional<size_t> oCurBlockSize;
   // Generator always supplies zeroes in
   doZeroes = doZeroes || !mIsProcessor;
   if (!doZeroes)
      oCurBlockSize = mUpstream.Acquire(mInBuffers, bound);
   else
      oCurBlockSize = { bound };
   if (!oCurBlockSize)
      return {};

   const auto curBlockSize = *oCurBlockSize;
   if (curBlockSize == 0) {
      assert(doZeroes || mUpstream.Remaining() == 0); // post of Acquire()
      FinishUpstream();
   }
   else {
      // Called only in Acquire()
      // invariant or post of mUpstream.Acquire() satisfies pre of Process()
      // because curBlockSize <= bound <= mInBuffers.blockSize()
      //    == data.BlockSize()
      // and mInBuffers.BlockSize() <= mInBuffers.Remaining() by invariant
      // and data.BlockSize() <= data.Remaining() by pre of Acquire()
      if (!Process(data, curBlockSize, outBufferOffset))
         return {};

      if (doZeroes)
         // Either a generator or doing the tail; will count down delay
         mLastZeroes = limitSampleBufferSize(curBlockSize, DelayRemaining());
      else {
         // Will count down the upstream
         mLastProduced += curBlockSize;
         mUpstream.Release();
         mInBuffers.Advance(curBlockSize);
         if (mInBuffers.Remaining() < mInBuffers.BlockSize())
            // Maintain invariant minimum availability
            mInBuffers.Rotate();
      }
   }
   return oCurBlockSize;
}

bool AudioGraph::EffectStage::Process(
   const Buffers &data, size_t curBlockSize, size_t outBufferOffset) const
{
   size_t processed{};
   try {
      auto outPositions = data.Positions();
      std::vector<float *> advancedPositions;
      if (outBufferOffset > 0) {
         auto channels = data.Channels();
         advancedPositions.reserve(channels);
         for (size_t ii = 0; ii < channels; ++ii)
            advancedPositions.push_back(outPositions[ii] + outBufferOffset);
         outPositions = advancedPositions.data();
      }
      processed = mInstance.ProcessBlock(mSettings,
         mInBuffers.Positions(), outPositions, curBlockSize);
   }
   catch (const AudacityException &) {
      // PRL: Bug 437:
      // Pass this along to our application-level handler
      throw;
   }
   catch (...) {
      // PRL:
      // Exceptions for other reasons, maybe in third-party code...
      // Continue treating them as we used to, but I wonder if these
      // should now be treated the same way.
      return false;
   }

   return (processed == curBlockSize);
}

sampleCount AudioGraph::EffectStage::Remaining() const
{
   // Not correct until at least one call to Acquire() so that mDelay is
   // assigned.  mDelay does not change thereafter; mDelayRemaining decreases
   // from that value to 0.
   return mLastProduced + mUpstream.Remaining() - mDelay + DelayRemaining();
}

bool AudioGraph::EffectStage::Release()
{
   // Progress toward termination (Remaining() == 0),
   // if mLastProduced + mLastZeroes > 0,
   // which is what Acquire() last returned
   mDelayRemaining -= mLastZeroes;
   mLastProduced = mLastZeroes = 0;
   return true;
}

void AudioGraph::EffectStage::FinishUpstream()
{
   if (!mCleared) {
      // From this point on, we only want to feed zeros to the plugin
      mInBuffers.Rewind();
      const auto blockSize = mInBuffers.BlockSize();
      for (size_t ii = 0; ii < mInBuffers.Channels(); ++ii) {
         auto p = &mInBuffers.GetWritePosition(ii);
         std::fill(p, p + blockSize, 0);
      }
      mCleared = true;
   }
}
