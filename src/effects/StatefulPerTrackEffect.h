/**********************************************************************

  Audacity: A Digital Audio Editor

  StatefulPerTrackEffect.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from PerTrackEffect.h

**********************************************************************/

#ifndef __AUDACITY_STATEFUL_PER_TRACK_EFFECT__
#define __AUDACITY_STATEFUL_PER_TRACK_EFFECT__

#include "PerTrackEffect.h" // to inherit

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
      bool ProcessInitialize(EffectSettings &settings, double sampleRate,
         sampleCount totalLen, ChannelNames chanMap) override;
      bool ProcessFinalize() /* noexcept */ override;
      size_t ProcessBlock(EffectSettings &settings,
         const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
      sampleCount GetLatency(
         const EffectSettings &settings, double sampleRate) override;

   protected:
      StatefulPerTrackEffect &GetEffect() const
      {
         // Tolerate const_cast in this class while it sun-sets
         return static_cast<StatefulPerTrackEffect &>(
            const_cast<PerTrackEffect &>(mProcessor));
      }
   };

   std::shared_ptr<EffectInstance> MakeInstance() const override;

   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   /*!
    @copydoc PerTrackEffect::Instance::GetLatency()
    */
   virtual sampleCount GetLatency();

   /*!
    @copydoc PerTrackEffect::Instance::ProcessInitialize()
    */
   virtual bool ProcessInitialize(EffectSettings &settings, double sampleRate,
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
