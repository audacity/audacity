/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

#include "StatefulPerTrackEffect.h"
#include "../ShuttleAutomation.h"
#include <float.h> // for FLT_MAX

class ShuttleGui;

using Floats = ArrayOf<float>;

struct EffectEchoSettings
{
   static constexpr double DefaultDelay = 1.0F;
   static constexpr double DefaultDecay = 0.5F;

   double delay{ DefaultDelay };
   double decay{ DefaultDecay };
};


class EffectEcho final : public EffectWithSettings<EffectEchoSettings, PerTrackEffect>
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

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   

   // Effect implementation
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;

   std::shared_ptr<EffectInstance> MakeInstance(EffectSettings& settings) const override;

   struct Instance;
   struct Validator;

private:
   
   const EffectParameterMethods& Parameters() const override;


static constexpr EffectParameter Delay{ &EffectEchoSettings::delay, L"Delay", EffectEchoSettings::DefaultDelay, 0.001f,  FLT_MAX, 1.0f };
static constexpr EffectParameter Decay{ &EffectEchoSettings::decay, L"Decay", EffectEchoSettings::DefaultDecay, 0.0f,    FLT_MAX, 1.0f };

};

#endif // __AUDACITY_EFFECT_ECHO__
