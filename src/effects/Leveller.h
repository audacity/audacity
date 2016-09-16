/**********************************************************************

  Audacity: A Digital Audio Editor

  Leveller.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_LEVELER__
#define __AUDACITY_EFFECT_LEVELER__

#include <wx/arrstr.h>
#include <wx/string.h>

#include "Effect.h"

class ShuttleGui;

#define LEVELLER_PLUGIN_SYMBOL XO("Leveller")

class EffectLeveller final : public Effect
{
public:
   EffectLeveller();
   virtual ~EffectLeveller();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation

   bool Startup() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

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

