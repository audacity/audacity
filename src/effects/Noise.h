/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.h

  Dominic Mazzoni

  An effect to add white noise.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE__
#define __AUDACITY_EFFECT_NOISE__

#include <wx/string.h>

#include "../ShuttleGui.h"
#include "../widgets/NumericTextCtrl.h"

#include "Effect.h"

#define NOISE_PLUGIN_SYMBOL XO("Noise")

class EffectNoise : public Effect
{
public:
   EffectNoise();
   virtual ~EffectNoise();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual int GetAudioOutCount();
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);
   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool Startup();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectNoise implementation

private:
   int mType;
   double mAmp;

   float y, z, buf0, buf1, buf2, buf3, buf4, buf5, buf6;

   NumericTextCtrl *mNoiseDurationT;
};

#endif
