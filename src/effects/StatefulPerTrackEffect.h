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
#include "StatefulEffectBase.h" // to inherit
#include "StatefulEffectUIServices.h" // to inherit

//! Subclass of PerTrackEffect, to be eliminated after all of its subclasses
//! are rewritten to be stateless
class StatefulPerTrackEffect
   : public StatefulEffectBase
   , public PerTrackEffect
   , public StatefulEffectUIServices
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
         ChannelNames chanMap) override;
      bool ProcessFinalize() noexcept override;
      size_t ProcessBlock(EffectSettings &settings,
         const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   protected:
      StatefulPerTrackEffect &GetEffect() const
      {
         // Tolerate const_cast in this class while it sun-sets
         return static_cast<StatefulPerTrackEffect &>(
            const_cast<PerTrackEffect &>(mProcessor));
      }
   };

   ~StatefulPerTrackEffect() override;

   std::shared_ptr<EffectInstance> MakeInstance() const override;

   size_t SetBlockSize(size_t maxBlockSize) override;
   size_t GetBlockSize() const override;

   /*!
    @copydoc PerTrackEffect::Instance::ProcessInitialize()
    */
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap = nullptr) override;

   /*!
    @copydoc PerTrackEffect::Instance::ProcessFinalize()
    */
   bool ProcessFinalize() noexcept override;

   /*!
    @copydoc PerTrackEffect::Instance::ProcessBlock()
    */
   virtual size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen) = 0;

private:
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) final;

   size_t mBlockSize{};
};

#endif
