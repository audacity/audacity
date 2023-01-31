/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

#include "StatelessPerTrackEffect.h"
#include "../ShuttleAutomation.h"
#include <float.h> // for FLT_MAX

class ShuttleGui;

using Floats = ArrayOf<float>;


struct EffectEchoSettings
{
   static constexpr double delayDefault = 1.0;
   static constexpr double decayDefault = 0.5;

   double delay{ delayDefault };
   double decay{ decayDefault };
};

class EffectEcho final : public EffectWithSettings<
   EffectEchoSettings, StatelessPerTrackEffect
>
{
public:
   
   static const ComponentInterfaceSymbol Symbol;

   EffectEcho();
   virtual ~EffectEcho();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation
   std::unique_ptr<EffectEditor> MakeEditor(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs)
   const override;

   struct Editor;

   struct Instance;

   std::shared_ptr<EffectInstance> MakeInstance() const override;

private:
   // EffectEcho implementation
   

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

static constexpr EffectParameter Delay{ &EffectEchoSettings::delay,
   L"Delay",   EffectEchoSettings::delayDefault, 0.001f,  FLT_MAX, 1.0f };
static constexpr EffectParameter Decay{ &EffectEchoSettings::decay,
   L"Decay",   EffectEchoSettings::decayDefault, 0.0f,    FLT_MAX, 1.0f };

#endif
};

#endif // __AUDACITY_EFFECT_ECHO__
