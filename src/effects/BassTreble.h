/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   BassTreble.h (two shelf filters)
   Steve Daulton

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_TREBLE__
#define __AUDACITY_EFFECT_BASS_TREBLE__

#include "StatefulPerTrackEffect.h"
#include "ShuttleAutomation.h"

class ShuttleGui;

class EffectBassTrebleState
{
public:
   float samplerate;
   double treble;
   double bass;
   double gain;
   double slope, hzBass, hzTreble;
   double a0Bass, a1Bass, a2Bass, b0Bass, b1Bass, b2Bass;
   double a0Treble, a1Treble, a2Treble, b0Treble, b1Treble, b2Treble;
   double xn1Bass, xn2Bass, yn1Bass, yn2Bass;
   double xn1Treble, xn2Treble, yn1Treble, yn2Treble;
};


struct EffectBassTrebleSettings
{
   static constexpr double bassDefault   = 0.0;
   static constexpr double trebleDefault = 0.0;
   static constexpr double gainDefault   = 0.0;
   static constexpr bool   linkDefault   = false;   

   double mBass  { bassDefault   };
   double mTreble{ trebleDefault };
   double mGain  { gainDefault   };
   bool   mLink  { linkDefault   };
};


class EffectBassTreble final : public EffectWithSettings<EffectBassTrebleSettings, PerTrackEffect>
{
public:
   
   static const ComponentInterfaceSymbol Symbol;

   EffectBassTreble();
   virtual ~EffectBassTreble();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   RealtimeSince RealtimeSupport() const override;


   // Effect Implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;

   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;

   struct Validator;

   struct Instance;

   std::shared_ptr<EffectInstance> MakeInstance() const override;


private:

   const EffectParameterMethods& Parameters() const override;

   static constexpr EffectParameter Bass{ &EffectBassTrebleSettings::mBass,
                         L"Bass",          EffectBassTrebleSettings::bassDefault,     -30.0,   30.0,    1  };

   static constexpr EffectParameter Treble{ &EffectBassTrebleSettings::mTreble,
                         L"Treble",          EffectBassTrebleSettings::trebleDefault, -30.0,   30.0,    1  };

   static constexpr EffectParameter Gain{ &EffectBassTrebleSettings::mGain,
                         L"Gain",          EffectBassTrebleSettings::gainDefault,     -30.0,   30.0,    1  };

   static constexpr EffectParameter Link{ &EffectBassTrebleSettings::mLink,
                         L"Link Sliders",  EffectBassTrebleSettings::linkDefault,      false,  true,    1  };
};

#endif
