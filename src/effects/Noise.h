/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.h

  Dominic Mazzoni

  An effect to add white noise.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE__
#define __AUDACITY_EFFECT_NOISE__

#include "../widgets/NumericTextCtrl.h"

#include "Effect.h"

class ShuttleGui;

#define NOISE_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Noise") }

class EffectNoise final : public Effect
{
public:
   EffectNoise();
   virtual ~EffectNoise();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   wxString GetDescription() override;
   wxString ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   unsigned GetAudioOutCount() override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // Effect implementation

   bool Startup() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectNoise implementation

private:
   int mType;
   double mAmp;

   float y, z, buf0, buf1, buf2, buf3, buf4, buf5, buf6;

   NumericTextCtrl *mNoiseDurationT;
};

#endif
