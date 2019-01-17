/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FADE__
#define __AUDACITY_EFFECT_FADE__

#include "Effect.h"

#define FADEIN_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Fade In") }
#define FADEOUT_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Fade Out") }

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

   // EffectClientInterface implementation

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
   EffectFadeIn() : EffectFade{ true } {}
};


class EffectFadeOut final : public EffectFade
{
public:
   EffectFadeOut() : EffectFade{ false } {}
};

#endif
