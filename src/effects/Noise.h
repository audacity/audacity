/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.h

  Dominic Mazzoni

  An effect to add white noise.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE__
#define __AUDACITY_EFFECT_NOISE__

#include "StatefulEffectUIServices.h"
#include "StatefulPerTrackEffect.h"
#include "ShuttleAutomation.h"
#include <wx/weakref.h>

class NumericTextCtrl;
class ShuttleGui;

class NoiseBase : public StatefulPerTrackEffect
{
public:
   static inline NoiseBase *
   FetchParameters(NoiseBase &e, EffectSettings &) { return &e; }
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
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

protected:
   // NoiseBase implementation

   double mSampleRate{};
   int mType;
   double mAmp;

   float y, z, buf0, buf1, buf2, buf3, buf4, buf5, buf6;

   NumericTextCtrl *mNoiseDurationT;

   const EffectParameterMethods& Parameters() const override;

   enum kTypes
   {
      kWhite,
      kPink,
      kBrownian,
      nTypes
   };
   static const EnumValueSymbol kTypeStrings[nTypes];

static constexpr EnumParameter Type{ &NoiseBase::mType,
   L"Type",       kWhite,  0,    nTypes - 1, 1, kTypeStrings, nTypes  };
static constexpr EffectParameter Amp{ &NoiseBase::mAmp,
   L"Amplitude",  0.8,     0.0,  1.0,           1  };
};

class EffectNoise : public NoiseBase, public StatefulEffectUIServices
{
public:
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access,
      const EffectOutputs* pOutputs) override;
   bool TransferDataToWindow(const EffectSettings& settings) override;
   bool TransferDataFromWindow(EffectSettings& settings) override;

private:
   wxWeakRef<wxWindow> mUIParent {};
};

#endif
