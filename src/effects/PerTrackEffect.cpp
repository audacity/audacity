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

AudioGraph::Source::~Source() = default;

AudioGraph::Sink::~Sink() = default;

PerTrackEffect::Instance::~Instance() = default;

bool PerTrackEffect::Instance::Process(EffectSettings &settings)
{
   return mProcessor.Process(*this, settings);
}

bool PerTrackEffect::Instance::ProcessInitialize(EffectSettings &,
   double, ChannelNames)
{
   return true;
}

bool PerTrackEffect::Instance::ProcessFinalize() noexcept
{
   return true;
}

sampleCount PerTrackEffect::Instance::GetLatency(
   const EffectSettings &, double) const
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

AudioGraph::Buffers::Buffers(size_t blockSize)
   : mBufferSize{ blockSize }, mBlockSize{ blockSize }
{
   assert(blockSize > 0);
   assert(IsRewound());
}

void AudioGraph::Buffers::Reinit(
   unsigned nChannels, size_t blockSize, size_t nBlocks)
{
   assert(blockSize > 0);
   assert(nBlocks > 0);
   mBuffers.resize(nChannels);
   mPositions.resize(nChannels);
   const auto bufferSize = blockSize * nBlocks;
   for (auto &buffer : mBuffers)
      buffer.resize(bufferSize);
   mBufferSize = bufferSize;
   mBlockSize = blockSize;
   Rewind();
}

void AudioGraph::Buffers::Discard(size_t drop, size_t keep)
{
#ifndef NDEBUG
   sampleCount oldRemaining = Remaining();
#endif
   // Assert the pre
   assert(drop + keep <= Remaining());

#if 1
   // Bounds checking version not assuming the pre, new in 3.2
   if (mBuffers.empty())
      return;

   // First buffer
   auto iterP = mPositions.begin();
   auto iterB = mBuffers.begin();
   auto position = *iterP;
   auto data = iterB->data();
   auto end = data + iterB->size();

   // Defend against excessive input values
   end = std::max(data, std::min(end, position + drop + keep));
   position = std::min(end, position);
   drop = std::min<size_t>(end - position, drop);
   // i.e. usually keep * sizeof(float) :
   const size_t size = ((end - position) - drop) * sizeof(float);
   memmove(position, position + drop, size);

   // other buffers; assuming equal sizes and relative positions (invariants)
   for (const auto endB = mBuffers.end(); ++iterB != endB;) {
      position = *++iterP;
      memmove(position, position + drop, size);
   }
#else
   // Version that assumes the precondition,
   // which pre-3.2 did without known errors
   for (auto position : mPositions)
      memmove(position, position + drop, keep * sizeof(float));
#endif
   // Assert the post
   assert(oldRemaining == Remaining());
}

void AudioGraph::Buffers::Advance(size_t count)
{
#ifndef NDEBUG
   sampleCount oldRemaining = Remaining();
#endif
   // Assert the pre
   assert(count <= Remaining());

#if 1
   // Bounds checking version not assuming the pre, new in 3.2
   if (mBuffers.empty())
      return;

   // First buffer; defend against excessive count
   auto iterP = mPositions.begin();
   auto iterB = mBuffers.begin();
   auto &position = *iterP;
   auto data = iterB->data();
   auto end = data + iterB->size();
   // invariant assumed, and preserved
   assert(data <= position && position <= end);
   count = std::min<size_t>(end - position, count);
   position += count;
   assert(data <= position && position <= end);

   // other buffers; assuming equal sizes and relative positions (invariants)
   for (const auto endB = mBuffers.end(); ++iterB != endB;) {
      auto &position = *++iterP;
      // invariant assumed, and preserved
      assert(iterB->data() <= position);
      assert(position <= iterB->data() + iterB->size());
   
      position += count;

      assert(iterB->data() <= position);
      assert(position <= iterB->data() + iterB->size());
   }
#else
   // Version that assumes the precondition,
   // which pre-3.2 did without known errors
   for (auto &position : mPositions)
      position += count;
#endif
   // Assert the post
   assert(Remaining() == oldRemaining - count);
}

void AudioGraph::Buffers::Rewind()
{
   auto iterP = mPositions.begin();
   for (auto &buffer : mBuffers)
      *iterP++ = buffer.data();
   assert(IsRewound());
}

constSamplePtr AudioGraph::Buffers::GetReadPosition(unsigned iChannel) const
{
   iChannel = std::min(iChannel, Channels() - 1);
   auto buffer = mBuffers[iChannel].data();
   return reinterpret_cast<constSamplePtr>(buffer);
}

float &AudioGraph::Buffers::GetWritePosition(unsigned iChannel)
{
   assert(iChannel < Channels());
   return mBuffers[iChannel].data()[ Position() ];
}

void AudioGraph::Buffers::ClearBuffer(unsigned iChannel, size_t n)
{
   if (iChannel < mPositions.size()) {
      auto p = mPositions[iChannel];
      auto &buffer = mBuffers[iChannel];
      auto end = buffer.data() + buffer.size();
      p = std::min(end, p);
      n = std::min<size_t>(end - p, n);
      std::fill(p, p + n, 0);
   }
}

SampleTrackSource::SampleTrackSource(
   const SampleTrack &left, const SampleTrack *pRight,
   sampleCount start, sampleCount len, Poller pollUser
)  : mLeft{ left }, mpRight{ pRight }, mPollUser{ move(pollUser) }
   , mPos{ start }, mOutputRemaining{ len  }
{
}

SampleTrackSource::~SampleTrackSource() = default;

bool SampleTrackSource::AcceptsBuffers(const Buffers &buffers) const
{
   return mOutputRemaining == 0 || buffers.Channels() > 0;
}

bool SampleTrackSource::AcceptsBlockSize(size_t) const
{
   return true;
}

sampleCount SampleTrackSource::Remaining() const
{
   return std::max<sampleCount>(0, mOutputRemaining);
}

std::optional<size_t> SampleTrackSource::Acquire(Buffers &data)
{
   assert(AcceptsBuffers(data));
   assert(AcceptsBlockSize(data.BlockSize()));

   if (!mInitialized || !data.Remaining()) {
      // Need to refill the buffers
      data.Rewind();
      // Calculate the number of samples to get
      const auto outputBufferCnt =
         limitSampleBufferSize(data.Remaining(), Remaining());
      // guarantees write won't overflow
      assert(outputBufferCnt <= data.Remaining());
      // Fill the buffers
      mLeft.GetFloats(&data.GetWritePosition(0), mPos, outputBufferCnt);
      if (mpRight && data.Channels() > 1)
         mpRight->GetFloats(&data.GetWritePosition(1), mPos, outputBufferCnt);
      mPos += outputBufferCnt;
      mInitialized = true;
   }
   assert(data.Remaining() > 0);
   auto result = mLastProduced = std::min(data.BlockSize(),
      limitSampleBufferSize(data.Remaining(), Remaining()));
   // assert post
   assert(result <= data.BlockSize());
   assert(result <= data.Remaining());
   assert(result <= Remaining());
   // true because the three terms of the min would be positive
   assert(Remaining() == 0 || result > 0);
   return { result };
}

bool SampleTrackSource::Release()
{
   mOutputRemaining -= mLastProduced;
   mLastProduced = 0;
   assert(mOutputRemaining >= 0);
   return !mPollUser || mPollUser(mPos);
}

EffectStage::EffectStage(Buffers &inBuffers,
   Instance &instance, EffectSettings &settings, double sampleRate,
   std::optional<sampleCount> genLength, ChannelNames map
)  : mInBuffers{ inBuffers }
   , mInstance{ instance }, mSettings{ settings }, mSampleRate{ sampleRate }
   , mIsProcessor{ !genLength.has_value() }
   //! Need to guarantee Remaining() is initially positive for processor
   , mDelayRemaining{ genLength ? *genLength : sampleCount::max() }
{
   // Give the plugin a chance to initialize
   if (!mInstance.ProcessInitialize(mSettings, mSampleRate, map))
      throw std::exception{};
}

EffectStage::~EffectStage()
{
   // Allow the plugin to cleanup
   mInstance.ProcessFinalize();
}

std::optional<size_t> EffectStage::Acquire(Buffers &data)
{
   data.Rewind();
   assert(data.Remaining() > 0);

   const auto blockSize = data.BlockSize();
   auto result = limitSampleBufferSize(
      std::min(blockSize, data.Remaining()), Remaining());
   if (mIsProcessor && !mCleared) {
      // From this point on, we only want to feed zeros to the plugin
      for (size_t ii = 0; ii < data.Channels(); ++ii) {
         auto p = &data.GetWritePosition(ii);
         std::fill(p, p + blockSize, 0);
      }
      mCleared = true;
   }
   // true because the three terms of the min would be positive
   assert(Remaining() == 0 || result > 0);
   return { result };
}

bool EffectStage::Process(
   const Buffers &data, size_t curBlockSize) const
{
   size_t processed{};
   try {
      processed = mInstance.ProcessBlock(mSettings,
         mInBuffers.Positions(), data.Positions(), curBlockSize);
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

sampleCount EffectStage::Remaining() const
{
   return std::max<sampleCount>(0, mDelayRemaining);
}

size_t EffectStage::ComputeDiscard(size_t curBlockSize)
{
   // Get the current number of delayed samples and accumulate
   if (mIsProcessor && !mLatencyDone) {
      // Some effects (like ladspa/lv2 swh plug-ins) don't report latency
      // until at least one block of samples is processed.  Find latency
      // once only for the track and assume it doesn't vary
      mDelayRemaining = mInstance.GetLatency(mSettings, mSampleRate);
      mLatencyDone = true;
   }
   return mIsProcessor
      ? limitSampleBufferSize(curBlockSize, Remaining())
      : 0;
}

void EffectStage::Release(const size_t curBlockSize)
{
   // Progress toward termination (Remaining() == 0) if curBlockSize > 0
   mDelayRemaining -= limitSampleBufferSize(curBlockSize, Remaining());
}

WaveTrackSink::WaveTrackSink(WaveTrack &left, WaveTrack *pRight,
   sampleCount start, bool isGenerator, bool isProcessor
)  : mLeft{ left }, mpRight{ pRight }
   , mGenLeft{ isGenerator ? left.EmptyCopy() : nullptr }
   , mGenRight{ pRight && isGenerator ? pRight->EmptyCopy() : nullptr }
   , mIsProcessor{ isProcessor }
   , mOutPos{ start }
{
}

WaveTrackSink::~WaveTrackSink() = default;

bool WaveTrackSink::AcceptsBuffers(const Buffers &buffers) const
{
   return buffers.Channels() > 0;
}

bool WaveTrackSink::Acquire(Buffers &data)
{
   if (data.BlockSize() <= data.Remaining()) {
      // post is satisfied
   }
   else
      // Output buffers have (mostly) filled
      // (less than one block remains; maybe nonzero because of samples
      // discarded for initial latency correction)
      DoConsume(data);
   return true;
}

bool WaveTrackSink::Release(const Buffers &, size_t)
{
   // May become non-trivial later
   return true;
}

void WaveTrackSink::DoConsume(Buffers &data)
{
   // Satisfy pre of GetReadPosition()
   assert(data.Channels() > 0);
   const auto inputBufferCnt = data.Position();
   if (inputBufferCnt > 0) {
      // Some data still unwritten
      if (mIsProcessor) {
         mLeft.Set(data.GetReadPosition(0),
            floatSample, mOutPos, inputBufferCnt);
         if (mpRight)
            mpRight->Set(data.GetReadPosition(1),
               floatSample, mOutPos, inputBufferCnt);
      }
      else if (mGenLeft) {
         mGenLeft->Append(data.GetReadPosition(0),
            floatSample, inputBufferCnt);
         if (mGenRight)
            mGenRight->Append(data.GetReadPosition(1),
               floatSample, inputBufferCnt);
      }
      // Satisfy post
      data.Rewind();
      // Bump to the next track position
      mOutPos += inputBufferCnt;
   }
   else {
      // Position is zero, therefore Remaining() is a positive multiple of
      // block size
   }
   // assert the post
   assert(data.BlockSize() <= data.Remaining());
}

bool PerTrackEffect::ProcessPass(Instance &instance, EffectSettings &settings)
{
   const auto duration = settings.extra.GetDuration();
   bool bGoodResult = true;
   bool isGenerator = GetType() == EffectTypeGenerate;
   bool isProcessor = GetType() == EffectTypeProcess;

   Buffers inBuffers{ 1 }, outBuffers{ 1 };
   ChannelName map[3];
   size_t prevBufferSize = 0;
   int count = 0;
   bool clear = false;

   // It's possible that the number of channels the effect expects changed based on
   // the parameters (the Audacity Reverb effect does when the stereo width is 0).
   const auto numAudioIn = GetAudioInCount();
   const auto numAudioOut = GetAudioOutCount();
   if (numAudioOut < 1)
      return false;

   const bool multichannel = numAudioIn > 1;
   auto range = multichannel
      ? mOutputTracks->Leaders()
      : mOutputTracks->Any();
   range.VisitWhile( bGoodResult,
      [&](WaveTrack *pLeft, const Track::Fallthrough &fallthrough) {
         // Track range visitor functions receive a pointer that is never null
         auto &left = *pLeft;
         if (!left.GetSelected())
            return fallthrough();

         sampleCount len = 0;
         sampleCount start = 0;
         unsigned numChannels = 0;
         WaveTrack *pRight{};

         // Iterate either over one track which could be any channel,
         // or if multichannel, then over all channels of left,
         // which is a leader.
         for (auto channel :
              TrackList::Channels(pLeft).StartingWith(pLeft)) {
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
            assert(numAudioIn > 1); // multichannel is true
            if (numChannels == 2) {
               // TODO: more-than-two-channels
               pRight = channel;
               clear = false;
               // Ignore other channels
               break;
            }
         }

         if (!isGenerator) {
            GetBounds(left, pRight, &start, &len);
            mSampleCnt = len;
            if (len > 0 && numAudioIn < 1) {
               bGoodResult = false;
               return;
            }
         }
         else
            mSampleCnt = left.TimeToLongSamples(duration);

         const auto sampleRate = left.GetRate();

         // Get the block size the client wants to use
         auto max = left.GetMaxBlockSize() * 2;
         const auto blockSize = instance.SetBlockSize(max);
         if (blockSize == 0) {
            bGoodResult = false;
            return;
         }

         // Calculate the buffer size to be at least the max rounded up to the clients
         // selected block size.
         const auto bufferSize =
            ((max + (blockSize - 1)) / blockSize) * blockSize;
         if (bufferSize == 0) {
            bGoodResult = false;
            return;
         }

         // Always create the number of input buffers the client expects even
         // if we don't have
         // the same number of channels.
         // (These resizes may do nothing after the first track)

         if (len > 0)
            assert(numAudioIn > 0); // checked above
         inBuffers.Reinit(numAudioIn, blockSize,
            std::max<size_t>(1, bufferSize / blockSize));
         if (len > 0)
            // post of Reinit later satisfies pre of Source::Acquire()
            assert(inBuffers.Channels() > 0);

         if (prevBufferSize != bufferSize) {
            // Buffer size has changed
            // We won't be using more than the first 2 buffers,
            // so clear the rest (if any)
            for (size_t i = 2; i < numAudioIn; i++)
               inBuffers.ClearBuffer(i, bufferSize);
         }
         prevBufferSize = bufferSize;

         // Always create the number of output buffers the client expects
         // even if we don't have the same number of channels.
         // (These resizes may do nothing after the first track)
         // Output buffers get an extra blockSize worth to give extra room if
         // the plugin adds latency -- PRL:  actually not important to do
         assert(numAudioOut > 0); // checked above
         outBuffers.Reinit(numAudioOut, blockSize,
            (bufferSize / blockSize) + 1);
         // post of Reinit satisfies pre of ProcessTrack
         assert(outBuffers.Channels() > 0);

         // (Re)Set the input buffer positions
         inBuffers.Rewind();

         // Clear unused input buffers
         if (!pRight && !clear && numAudioIn > 1) {
            inBuffers.ClearBuffer(1, bufferSize);
            clear = true;
         }

         const auto genLength = [this, &settings, &left, isGenerator](
         ) -> std::optional<sampleCount> {
            double genDur = 0;
            if (isGenerator) {
               const auto duration = settings.extra.GetDuration();
               if (IsPreviewing()) {
                  gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &genDur, 6.0);
                  genDur = std::min(duration, CalcPreviewInputLength(settings, genDur));
               }
               else
                  genDur = duration;
               // round to nearest sample
               return sampleCount{ (left.GetRate() * genDur) + 0.5 };
            }
            else
               return {};
         }();

         const auto pollUser = [this, numChannels, count, start,
            length = (genLength ? *genLength : len).as_double()
         ](sampleCount inPos){
            if (numChannels > 1) {
               if (TrackGroupProgress(
                  count, (inPos - start).as_double() / length)
               )
                  return false;
            }
            else {
               if (TrackProgress(count, (inPos - start).as_double() / length))
                  return false;
            }
            return true;
         };

         // Assured above
         assert(len == 0 || inBuffers.Channels() > 0);
         SampleTrackSource source{ left, pRight, start, len, pollUser };
         // Assert source is safe to Acquire inBuffers
         assert(source.AcceptsBuffers(inBuffers));
         assert(source.AcceptsBlockSize(inBuffers.BlockSize()));

         WaveTrackSink sink{ left, pRight, start, isGenerator, isProcessor };
         assert(sink.AcceptsBuffers(outBuffers));

         // Go process the track(s)
         try {
            bGoodResult = ProcessTrack(instance, settings, source, sink,
               genLength, sampleRate, map,
               inBuffers, outBuffers);
         } catch(const std::exception&) {
            bGoodResult = false;
         }
         if (bGoodResult)
            sink.Flush(outBuffers,
               mT0, ViewInfo::Get(*FindProject()).selectedRegion.t1());
         if (!bGoodResult)
            return;
         ++count;
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
   AudioGraph::Source &source, AudioGraph::Sink &sink,
   std::optional<sampleCount> genLength,
   const double sampleRate, const ChannelNames map,
   Buffers &inBuffers, Buffers &outBuffers) const
{
   assert(source.AcceptsBuffers(inBuffers));
   assert(sink.AcceptsBuffers(outBuffers));

   bool rc = true;
   const auto blockSize = inBuffers.BlockSize();
   assert(source.AcceptsBlockSize(blockSize));

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

   bool isProcessor = GetType() == EffectTypeProcess;

   EffectStage stage{ inBuffers,
      instance, settings, sampleRate, genLength, map };

   outBuffers.Rewind();
   // Invariant O: blockSize positions from outBuffers.Positions() available
   // Initially true because of preconditions that outBuffers has the same
   // block size as inBuffers and is rewound
   assert(blockSize <= outBuffers.Remaining());

   // Call the effect until we run out of input or delayed samples
   while (source.Remaining() > 0 || stage.Remaining() > 0) {
      const auto oCurBlockSize = (source.Remaining() > 0)
         ? source.Acquire(inBuffers)
         : stage.Acquire(inBuffers);
      if (!oCurBlockSize)
         return false;
      const auto curBlockSize = *oCurBlockSize;
      assert(curBlockSize <= inBuffers.Remaining()); // Process needs this
      // post of Acquire() gives termination guarantee below
      assert(curBlockSize > 0);
      assert(curBlockSize <= blockSize);

      // Invariant O satisfies the precondition
      if (!stage.Process(outBuffers, curBlockSize))
         return false;

      // Preconditions for Discard() and Consume() hold
      // because curBlockSize <= blockSize <= outBuffers.Remaining()
      auto discard = stage.ComputeDiscard(curBlockSize);

      if (source.Remaining() > 0) {
         if (!source.Release())
            return false;
         // post of Advance() implies progress to loop termination
         if (source.Remaining() > 0) {
            // See where curBlockSize was decided:
            assert(curBlockSize <= inBuffers.Remaining());
            inBuffers.Advance(curBlockSize);
         }
      }
      else
         // make progress to termination
         stage.Release(curBlockSize);
   
      // Do latency correction on effect output
      if (discard)
         outBuffers.Discard(discard, curBlockSize - discard);

      if (!sink.Release(outBuffers, curBlockSize - discard))
         return false;
      outBuffers.Advance(curBlockSize - discard);
      if (!sink.Acquire(outBuffers))
         return false;
      // Invariant O preserved
   }
   return rc;
}

void WaveTrackSink::Flush(Buffers &data, const double t0, const double t1)
{
   DoConsume(data);
   if (mGenLeft) {
      // Transfer the data from the temporary tracks to the actual ones
      mGenLeft->Flush();
      // mT1 gives us the NEW selection. We want to replace up to GetSel1().
      PasteTimeWarper warper{ t1, t0 + mGenLeft->GetEndTime() };
      mLeft.ClearAndPaste(t0, t1, mGenLeft.get(), true, true, &warper);
      if (mGenRight) {
         mGenRight->Flush();
         mpRight->ClearAndPaste(t0, t1, mGenRight.get(), true, true, &warper);
      }
   }
}
