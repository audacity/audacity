/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_STEREO_TO_MONO__
#define __AUDACITY_EFFECT_STEREO_TO_MONO__

#include <wx/string.h>

#include "Effect.h"

#define STEREOTOMONO_PLUGIN_SYMBOL XO("Stereo To Mono")

class EffectStereoToMono : public Effect
{
public:
   EffectStereoToMono();
   virtual ~EffectStereoToMono();

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

   // Effect implementation

   bool IsHidden();
};

#endif

