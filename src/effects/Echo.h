/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

#include "StatelessPerTrackEffect.h"
#include "ShuttleAutomation.h"
#include <float.h> // for FLT_MAX

class ShuttleGui;

using Floats = ArrayOf<float>;


struct EchoSettings
{
   static constexpr double delayDefault = 1.0;
   static constexpr double decayDefault = 0.5;

   double delay{ delayDefault };
   double decay{ decayDefault };
};

class EchoBase : public EffectWithSettings<EchoSettings, PerTrackEffect>
{
public:

   static const ComponentInterfaceSymbol Symbol;

   EchoBase();
   virtual ~EchoBase();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   struct Instance;

protected:
   // EchoBase implementation
   const EffectParameterMethods& Parameters() const override;

#if 0
   // TODO simplify like this in C++20
   using ParametersType = CapturedParameters<EchoBase,
        EffectParameter{
         &EchoBase::delay, L"Delay",   1.0f, 0.001f,  FLT_MAX, 1.0f }
      , EffectParameter{
         &EchoBase::decay, L"Decay",   0.5f, 0.0f,    FLT_MAX, 1.0f }
   >;
#else

static constexpr EffectParameter Delay{ &EchoSettings::delay,
   L"Delay",   EchoSettings::delayDefault, 0.001f,  FLT_MAX, 1.0f };
static constexpr EffectParameter Decay{ &EchoSettings::decay,
   L"Decay",   EchoSettings::decayDefault, 0.0f,    FLT_MAX, 1.0f };

#endif
};

class EffectEcho final : public EchoBase, public StatelessEffectUIServices
{
   public:

   struct Editor;

   // Effect implementation
   std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs)
   const override;

   std::shared_ptr<EffectInstance> MakeInstance() const override;
};

#endif // __AUDACITY_EFFECT_ECHO__
