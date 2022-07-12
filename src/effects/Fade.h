/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FADE__
#define __AUDACITY_EFFECT_FADE__

#include "StatefulPerTrackEffect.h"

class EffectFade : public StatefulPerTrackEffect
{
public:
   EffectFade(bool fadeIn = false);
   virtual ~EffectFade();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool IsInteractive() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      sampleCount totalLen, ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

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
