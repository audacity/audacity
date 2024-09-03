#pragma once

#include "SettingsVisitor.h"
#include "StatefulPerTrackEffect.h"

class BUILTIN_EFFECTS_API AmplifyBase : public StatefulPerTrackEffect
{
public:
   static inline AmplifyBase* FetchParameters(AmplifyBase& e, EffectSettings&)
   {
      return &e;
   }
   static const ComponentInterfaceSymbol Symbol;

   AmplifyBase();
   virtual ~AmplifyBase() override;

   // EffectInstanceFactory
   std::shared_ptr<EffectInstance> MakeInstance() const override;


   // EffectDefinitionInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   EffectType GetType() const override;
   OptionalMessage LoadFactoryDefaults(EffectSettings& settings) const override;
   OptionalMessage DoLoadFactoryDefaults(EffectSettings& settings);

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   size_t ProcessBlock(
      EffectSettings& settings, const float* const* inBlock,
      float* const* outBlock, size_t blockLen) override;

   // Effect implementation

   bool Init() override;
   std::any BeginPreview(const EffectSettings& settings) override;

protected:
   // TODO review this
   struct BUILTIN_EFFECTS_API Instance : StatefulPerTrackEffect::Instance
   {
      using StatefulPerTrackEffect::Instance::Instance;
      ~Instance() override;
   };

protected:
   void ClampRatio();

   // AmplifyBase implementation
protected:
   double mPeak = 1.0;

   double mRatio = 1.0;
   double mRatioClip =
      1.0; // maximum value of mRatio which does not cause clipping
   double mAmp = 0.0;
   double mNewPeak = 1.0;
   bool mCanClip = true;

private:
   const EffectParameterMethods& Parameters() const override;

protected:
   static constexpr EffectParameter Ratio {
      &AmplifyBase::mRatio, L"Ratio", 0.9f, 0.003162f, 316.227766f, 1.0f
   };
   // Amp is not saved in settings!
   static constexpr EffectParameter Amp {
      &AmplifyBase::mAmp, L"", -0.91515f, -50.0f, 50.0f, 10.0f
   };
   static constexpr EffectParameter Clipping {
      &AmplifyBase::mCanClip, L"AllowClipping", false, false, true, 1
   };
};
