/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.h

  Mark Phillips

  This class reverses the selected audio.

**********************************************************************/
#pragma once

#include "StatefulEffect.h"

class BUILTIN_EFFECTS_API Reverse : public StatefulEffect
{
public:
   Reverse();
   virtual ~Reverse();

   static const ComponentInterfaceSymbol Symbol;

   // ComponentInterface implementation
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation
   EffectType GetType() const override;
   bool IsInteractive() const override;

   // Effect implementation
   bool Process(EffectInstance& instance, EffectSettings& settings) override;
};
