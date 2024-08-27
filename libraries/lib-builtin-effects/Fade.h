/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/
#pragma once

#include "StatefulPerTrackEffect.h"

class BUILTIN_EFFECTS_API Fade : public StatefulPerTrackEffect
{
public:
   Fade(bool fadeIn = false);
   virtual ~Fade() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool IsInteractive() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(
      EffectSettings& settings, double sampleRate,
      ChannelNames chanMap) override;
   size_t ProcessBlock(
      EffectSettings& settings, const float* const* inBlock,
      float* const* outBlock, size_t blockLen) override;

protected:
   // EffectFade implementation

   bool mFadeIn;
   sampleCount mSample;
};

class BUILTIN_EFFECTS_API FadeIn final : public Fade
{
public:
   static const ComponentInterfaceSymbol Symbol;

   FadeIn();

   // ComponentInterface implementation
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
};

class BUILTIN_EFFECTS_API FadeOut final : public Fade
{
public:
   static const ComponentInterfaceSymbol Symbol;

   FadeOut();

   // ComponentInterface implementation
   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
};
