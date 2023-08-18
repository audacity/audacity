/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.h

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REVERSE__
#define __AUDACITY_EFFECT_REVERSE__

#include "StatefulEffect.h"
#include <functional>

class EffectReverse final : public StatefulEffect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectReverse();
   virtual ~EffectReverse();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool IsInteractive() const override;

   // Effect implementation

   bool Process(EffectInstance &instance, EffectSettings &settings) override;
 };

#endif

