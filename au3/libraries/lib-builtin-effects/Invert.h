/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.h

  Mark Phillips

  This class inverts the selected audio.

**********************************************************************/
#pragma once

#include "StatefulPerTrackEffect.h"

class BUILTIN_EFFECTS_API Invert : public StatefulPerTrackEffect
{
public:
   Invert();
   virtual ~Invert();

   static const ComponentInterfaceSymbol Symbol;

   // ComponentInterface implementation
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation
   EffectType GetType() const override;
   bool IsInteractive() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   size_t ProcessBlock(
      EffectSettings& settings, const float* const* inBlock,
      float* const* outBlock, size_t blockLen) override;

   bool NeedsDither() const override;
};
