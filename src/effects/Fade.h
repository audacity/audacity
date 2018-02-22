/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FADE__
#define __AUDACITY_EFFECT_FADE__

#include <wx/string.h>

#include "Effect.h"

#define FADEIN_PLUGIN_SYMBOL XO("Fade In")
#define FADEOUT_PLUGIN_SYMBOL XO("Fade Out")

class EffectFade final : public Effect
{
public:
   EffectFade(bool fadeIn = false);
   virtual ~EffectFade();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;
   bool IsInteractive() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;

private:
   // EffectFadeIn implementation

   bool mFadeIn;
   sampleCount mSample;
};

#endif
