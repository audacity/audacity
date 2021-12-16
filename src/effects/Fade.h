/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FADE__
#define __AUDACITY_EFFECT_FADE__

#include "Effect.h"

class EffectFade : public Effect
{
public:
   EffectFade(bool fadeIn = false);
   virtual ~EffectFade();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool IsInteractive() override;

   // EffectProcessor implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;

private:
   // EffectFade implementation

   bool mFadeIn;
   sampleCount mSample;
};

class EffectFadeIn final : public EffectFade
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectFadeIn() : EffectFade{ true } {}
};


class EffectFadeOut final : public EffectFade
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectFadeOut() : EffectFade{ false } {}
};

#endif
