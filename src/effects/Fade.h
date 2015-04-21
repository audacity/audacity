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

class EffectFade : public Effect
{
public:
   EffectFade(bool fadeIn = false);
   virtual ~EffectFade();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual bool IsInteractive();

   // EffectClientInterface implementation

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL);
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);

private:
   // EffectFadeIn implementation

   bool mFadeIn;
   sampleCount mSample;
};

#endif
