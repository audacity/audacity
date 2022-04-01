/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

#include "PerTrackEffect.h"
#include "../ShuttleAutomation.h"
#include <float.h> // for FLT_MAX

class ShuttleGui;

using Floats = ArrayOf<float>;

class EffectEcho final : public StatefulPerTrackEffect
{
public:
   static inline EffectEcho *
   FetchParameters(EffectEcho &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectEcho();
   virtual ~EffectEcho();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   // Effect implementation
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;

private:
   // EffectEcho implementation

   double delay;
   double decay;
   Floats history;
   size_t histPos;
   size_t histLen;

   const EffectParameterMethods& Parameters() const override;

#if 0
   // TODO simplify like this in C++20
   using ParametersType = CapturedParameters<EffectEcho,
        EffectParameter{
         &EffectEcho::delay, L"Delay",   1.0f, 0.001f,  FLT_MAX, 1.0f }
      , EffectParameter{
         &EffectEcho::decay, L"Decay",   0.5f, 0.0f,    FLT_MAX, 1.0f }
   >;
#else

static constexpr EffectParameter Delay{ &EffectEcho::delay,
   L"Delay",   1.0f, 0.001f,  FLT_MAX, 1.0f };
static constexpr EffectParameter Decay{ &EffectEcho::decay,
   L"Decay",   0.5f, 0.0f,    FLT_MAX, 1.0f };

#endif
};

#endif // __AUDACITY_EFFECT_ECHO__
