/**********************************************************************

  Audacity: A Digital Audio Editor

  Leveller.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_LEVELER__
#define __AUDACITY_EFFECT_LEVELER__

#include <wx/arrstr.h>
#include <wx/string.h>

#include "../ShuttleGui.h"

#include "Effect.h"

#define LEVELLER_PLUGIN_SYMBOL XO("Leveller")

class EffectLeveller : public Effect
{
public:
   EffectLeveller();
   virtual ~EffectLeveller();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual int GetAudioInCount();
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
   // EffectLeveller implementation

   void CalcLevellerFactors();
   float LevelOneFrame(float frame);

private:
   int    mNumPasses;
   double mDbSilenceThreshold;

   int    mDbIndex;
   int    mPassIndex;
};

#endif

