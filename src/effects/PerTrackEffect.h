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
   ProcessBlock(), and ProcessFinalize() methods of EffectProcessor,
   and also GetLatency() to determine how many leading output samples to
   discard and how many extra samples to produce.
 */
class PerTrackEffect : public Effect
{
public:
   ~PerTrackEffect() override;

   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   //! Adds virtual functions whose default implementations call-through to the
   //! PerTrackEffect members
   /*!
    PerTrackEffects that are completely stateless will define subclasses that
    override the new virtual functions
    */
   class AUDACITY_DLL_API Instance : public Effect::Instance {
   public:
      using Effect::Instance::Instance;
      ~Instance() override;
   
      //! Uses the other virtual functions of this class
      bool Process(EffectSettings &settings) final;

      /*!
       @copydoc PerTrackEffect::ProcessInitialize()
       */
      virtual bool ProcessInitialize(EffectSettings &settings,
         sampleCount totalLen, ChannelNames chanMap);

      /*!
       @copydoc PerTrackEffect::ProcessFinalize()
       */
      virtual bool ProcessFinalize() /* noexcept */ ;

      /*!
       @copydoc PerTrackEffect::ProcessBlock()
       */
      virtual size_t ProcessBlock(EffectSettings &settings,
         const float *const *inBlock, float *const *outBlock, size_t blockLen);

      /*!
       @copydoc PerTrackEffect::GetLatency()
       */
      virtual sampleCount GetLatency();

   protected:
      PerTrackEffect &GetEffect() const
      { return static_cast<PerTrackEffect &>(mEffect); }
   };

   //! Called for destructive, non-realtime effect computation
   //! Default implementation returns zero
   virtual sampleCount GetLatency();

   //! Called at start of destructive processing, for each (mono/stereo) track
   //! Default implementation does nothing, returns true
   virtual bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap = nullptr);

   //! Called at end of destructive processing, for each (mono/stereo) track
   //! Default implementation does nothing, returns true
   //! This may be called during stack unwinding:
   virtual bool ProcessFinalize() /* noexcept */;

   //! Called for destructive effect computation
   //! Default implementation does nothing, returns zero
   virtual size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen);

   std::shared_ptr<EffectInstance> MakeInstance(EffectSettings &settings)
      const override;

protected:
   // These were overridables but the generality wasn't used yet
   /* virtual */ bool DoPass1() const;
   /* virtual */ bool DoPass2() const;

   sampleCount    mSampleCnt{};

private:
   bool Process(EffectInstance &instance, EffectSettings &settings) final;
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

   size_t mBlockSize{};
};

template<typename Settings> using PerTrackEffectWithSettings =
   EffectWithSettings<Settings, PerTrackEffect>;

#endif
