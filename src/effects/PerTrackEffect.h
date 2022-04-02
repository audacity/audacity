/**********************************************************************

  Audacity: A Digital Audio Editor

  PerTrackEffect.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/

#ifndef __AUDACITY_PER_TRACK_EFFECT__
#define __AUDACITY_PER_TRACK_EFFECT__

#include "Effect.h" // to inherit
#include "MemoryX.h"

using FloatBuffers = ArraysOf<float>;

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

   class AUDACITY_DLL_API Instance : public virtual EffectInstance {
   public:
      explicit Instance(PerTrackEffect &processor)
         : mProcessor{ processor }
      {}
      ~Instance() override;
   
      //! Uses the other virtual functions of this class
      bool Process(EffectSettings &settings) final;

      //! Called at start of destructive processing, for each (mono/stereo) track
      //! Default implementation does nothing, returns true
      virtual bool ProcessInitialize(EffectSettings &settings,
         sampleCount totalLen, ChannelNames chanMap);

      //! Called at end of destructive processing, for each (mono/stereo) track
      //! Default implementation does nothing, returns true
      //! This may be called during stack unwinding:
      virtual bool ProcessFinalize() /* noexcept */ ;

      //! Called for destructive effect computation
      virtual size_t ProcessBlock(EffectSettings &settings,
         const float *const *inBlock, float *const *outBlock, size_t blockLen)
      = 0;

      //! Called for destructive, non-realtime effect computation
      //! Default implementation returns zero
      virtual sampleCount GetLatency();

   protected:
      PerTrackEffect &mProcessor;
   };

protected:
   // These were overridables but the generality wasn't used yet
   /* virtual */ bool DoPass1() const;
   /* virtual */ bool DoPass2() const;

   // non-virtual
   bool Process(EffectInstance &instance, EffectSettings &settings);

   sampleCount    mSampleCnt{};

private:
   bool ProcessPass(Instance &instance, EffectSettings &settings);
   bool ProcessTrack(Instance &instance, EffectSettings &settings,
      int count,
      ChannelNames map,
      WaveTrack *left,
      WaveTrack *right,
      sampleCount start,
      sampleCount len,
      FloatBuffers &inBuffer,
      FloatBuffers &outBuffer,
      ArrayOf< float * > &inBufPos,
      ArrayOf< float *> &outBufPos, size_t bufferSize, size_t blockSize,
      unsigned mNumChannels);
};

template<typename Settings> using PerTrackEffectWithSettings =
   EffectWithSettings<Settings, PerTrackEffect>;

//! Subclass of PerTrackEffect, to be eliminated after all of its subclasses
//! are rewritten to be stateless
class StatefulPerTrackEffect
   : public StatefulEffectBase
   , public PerTrackEffect
{
public:

   //! Implemented with call-throughs to the
   //! StatefulPerTrackEffect virtual functions
   class AUDACITY_DLL_API Instance
      : public StatefulEffectBase::Instance
      , public PerTrackEffect::Instance
   {
   public:
      explicit Instance(StatefulPerTrackEffect &effect)
         : StatefulEffectBase::Instance{ effect }
         , PerTrackEffect::Instance{ effect }
      {}
      ~Instance() override;
      bool ProcessInitialize(EffectSettings &settings,
         sampleCount totalLen, ChannelNames chanMap) override;
      bool ProcessFinalize() /* noexcept */ override;
      size_t ProcessBlock(EffectSettings &settings,
         const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
      sampleCount GetLatency() override;

   protected:
      StatefulPerTrackEffect &GetEffect() const
      { return static_cast<StatefulPerTrackEffect &>(mProcessor); }
   };

   std::shared_ptr<EffectInstance> MakeInstance(EffectSettings &settings)
      const override;

   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   /*!
    @copydoc PerTrackEffect::Instance::GetLatency()
    */
   virtual sampleCount GetLatency();

   /*!
    @copydoc PerTrackEffect::Instance::ProcessInitialize()
    */
   virtual bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap = nullptr);

   /*!
    @copydoc PerTrackEffect::Instance::ProcessFinalize()
    */
   virtual bool ProcessFinalize() /* noexcept */;

   /*!
    @copydoc PerTrackEffect::Instance::ProcessBlock()
    */
   virtual size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen);

private:
   bool Process(EffectInstance &instance, EffectSettings &settings) final;

   size_t mBlockSize{};
};

#endif
