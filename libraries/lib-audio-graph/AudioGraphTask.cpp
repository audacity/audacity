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

#include "AudioGraphBuffers.h"
#include "EffectStage.h"
#include "SampleTrackSource.h"
#include "../SyncLock.h"
#include "ViewInfo.h"
#include "../WaveTrack.h"
#include "../WaveTrackSink.h"

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

AudioGraph::Task::Task(Source &source, Buffers &buffers, Sink &sink)
   : mSource{ source }, mBuffers{ buffers }, mSink{ sink }
{
   assert(source.AcceptsBlockSize(buffers.BlockSize()));
   assert(source.AcceptsBuffers(buffers));
   assert(sink.AcceptsBuffers(buffers));
}

bool PerTrackEffect::ProcessTrack(Instance &instance, EffectSettings &settings,
   AudioGraph::Source &upstream, AudioGraph::Sink &sink,
   std::optional<sampleCount> genLength,
   const double sampleRate, const ChannelNames map,
   Buffers &inBuffers, Buffers &outBuffers)
{
   assert(upstream.AcceptsBuffers(inBuffers));
   assert(sink.AcceptsBuffers(outBuffers));

   const auto blockSize = inBuffers.BlockSize();
   assert(upstream.AcceptsBlockSize(blockSize));
   assert(blockSize == outBuffers.BlockSize());

   AudioGraph::EffectStage source{ upstream, inBuffers,
      instance, settings, sampleRate, genLength, map };
   assert(source.AcceptsBlockSize(blockSize)); // post of ctor
   assert(source.AcceptsBuffers(outBuffers));

   AudioGraph::Task task{ source, outBuffers, sink };
   return task.RunLoop();
}

bool AudioGraph::Task::RunLoop()
{
   // Satisfy invariant initially
   mBuffers.Rewind();
   Status status{};
   do {
      assert(mBuffers.Remaining() >= mBuffers.BlockSize());
      status = RunOnce();
   } while (status == Status::More);
   return status == Status::Done;
}

auto AudioGraph::Task::RunOnce() -> Status
{
   const auto blockSize = mBuffers.BlockSize();
   assert(mBuffers.Remaining() >= blockSize); // pre
   if (auto oCurBlockSize = mSource.Acquire(mBuffers, blockSize)) {
      const auto curBlockSize = *oCurBlockSize;
      if (curBlockSize == 0)
         // post (same as pre) obviously preserved
         return Status::Done;

      // post of source.Acquire() satisfies pre of sink.Release()
      assert(curBlockSize <= blockSize);
      if (!mSink.Release(mBuffers, curBlockSize))
         return Status::Fail;
   
      // This may break the post
      mBuffers.Advance(curBlockSize);

      // posts of source.Acquire() and source.Relase()
      // give termination guarantee
      assert(mSource.Remaining() == 0 || curBlockSize > 0);
      if (!mSource.Release())
         return Status::Fail;

      // Reestablish the post
      if (!mSink.Acquire(mBuffers))
         return Status::Fail;
      assert(mBuffers.Remaining() >= blockSize);

      return Status::More;
   }
   else
      return Status::Fail;
}
