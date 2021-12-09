/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.h

  Dominic Mazzoni

  An effect to add white noise.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE__
#define __AUDACITY_EFFECT_NOISE__

#include "Effect.h"

class NumericTextCtrl;
class ShuttleGui;

class EffectNoise final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectNoise();
   virtual ~EffectNoise();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectProcessor implementation

   unsigned GetAudioOutCount() override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
   bool DefineParams( ShuttleParams & S ) override;

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
