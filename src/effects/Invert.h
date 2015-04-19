/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.h

  Mark Phillips

  This class inverts the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_INVERT__
#define __AUDACITY_EFFECT_INVERT__

#include <wx/string.h>

#include "Effect.h"

#define INVERT_PLUGIN_SYMBOL XO("Invert")

class EffectInvert : public Effect
{
public:
   EffectInvert();
   virtual ~EffectInvert();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual bool IsInteractive();

   // EffectClientInterface implementation

   virtual int GetAudioInCount();
   virtual int GetAudioOutCount();
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);
};

#endif
