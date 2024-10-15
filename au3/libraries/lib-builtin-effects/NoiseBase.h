/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseBase.h

  Dominic Mazzoni

**********************************************************************/
#pragma once

#include "ShuttleAutomation.h"
#include "StatefulPerTrackEffect.h"

class BUILTIN_EFFECTS_API NoiseBase : public StatefulPerTrackEffect
{
public:
   static inline NoiseBase* FetchParameters(NoiseBase& e, EffectSettings&)
   {
      return &e;
   }
   static const ComponentInterfaceSymbol Symbol;

   NoiseBase();
   virtual ~NoiseBase();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(
      EffectSettings& settings, double sampleRate,
      ChannelNames chanMap) override;
   size_t ProcessBlock(
      EffectSettings& settings, const float* const* inBlock,
      float* const* outBlock, size_t blockLen) override;

protected:
   // NoiseBase implementation

   double mSampleRate {};
   int mType;
   double mAmp;

   float y, z, buf0, buf1, buf2, buf3, buf4, buf5, buf6;

   const EffectParameterMethods& Parameters() const override;

   enum kTypes
   {
      kWhite,
      kPink,
      kBrownian,
      nTypes
   };
   static const EnumValueSymbol kTypeStrings[nTypes];

   static constexpr EnumParameter Type {
      &NoiseBase::mType, L"Type", kWhite, 0, nTypes - 1, 1, kTypeStrings, nTypes
   };
   static constexpr EffectParameter Amp {
      &NoiseBase::mAmp, L"Amplitude", 0.8, 0.0, 1.0, 1
   };
};
