/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.h

  Mark Phillips

  This class inverts the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_INVERT__
#define __AUDACITY_EFFECT_INVERT__

#include "Effect.h"

class EffectInvert final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectInvert();
   virtual ~EffectInvert();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool IsInteractive() override;

   // EffectProcessor implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
};

#endif
