/**********************************************************************

  Audacity: A Digital Audio Editor

  PerTrackEffect.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/

#ifndef __AUDACITY_PER_TRACK_EFFECT__
#define __AUDACITY_PER_TRACK_EFFECT__

#include "AudioGraphSink.h" // to inherit
#include "AudioGraphSource.h" // to inherit
#include "Effect.h" // to inherit
#include "MemoryX.h"
#include "SampleCount.h"
#include <functional>

//! Base class for Effects that treat each (mono or stereo) track independently
//! of other tracks.
/*!
   Its override of Effect::Process() uses ProcessInitialize(),
   ProcessBlock(), and ProcessFinalize() methods of its instance made by
   MakeInstance(), which must be a subclass of PerTrackEffect::Instance.
   Also uses GetLatency() to determine how many leading output samples to
   discard and how many extra samples to produce.
 */
class PerTrackEffect
   : public Effect
{
public:
   ~PerTrackEffect() override;

   class AUDACITY_DLL_API Instance : public virtual EffectInstanceEx {
   public:
      explicit Instance(const PerTrackEffect &processor)
         : mProcessor{ processor }
      {}
      ~Instance() override;
   
      //! Uses the other virtual functions of this class
      bool Process(
         EffectContext &context, EffectSettings &settings) final;

      bool ProcessInitialize(EffectSettings &settings,
         double sampleRate, ChannelNames chanMap) override;

      bool ProcessFinalize() noexcept override;
   protected:
      const PerTrackEffect &mProcessor;
   };

protected:
   // These were overridables but the generality wasn't used yet
   /* virtual */ bool DoPass1() const;
   /* virtual */ bool DoPass2() const;

   // non-virtual
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) const;

   sampleCount    mSampleCnt{};

private:
   using Buffers = AudioGraph::Buffers;

   bool ProcessPass(
      EffectContext &context, Instance &instance, EffectSettings &settings);
   using Factory = std::function<std::shared_ptr<EffectInstance>()>;
   /*!
    Previous contents of inBuffers and outBuffers are ignored

    @pre `source.AcceptsBuffers(inBuffers)`
    @pre `source.AcceptsBlockSize(inBuffers.BlockSize())`
    @pre `sink.AcceptsBuffers(outBuffers)`
    @pre `inBuffers.BlockSize() == outBuffers.BlockSize()`
    */
   static bool ProcessTrack(bool multi,
      const Factory &factory, EffectSettings &settings,
      AudioGraph::Source &source, AudioGraph::Sink &sink,
      std::optional<sampleCount> genLength,
      double sampleRate, const Track &track,
      Buffers &inBuffers, Buffers &outBuffers);
};
#endif
