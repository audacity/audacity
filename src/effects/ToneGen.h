/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly

  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include "Effect.h"

class NumericTextCtrl;
class ShuttleGui;

class EffectToneGen : public Effect
{
public:
   EffectToneGen(bool isChirp);
   virtual ~EffectToneGen();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectProcessor implementation

   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectToneGen implementation

   void OnControlUpdate(wxCommandEvent & evt);

private:
   bool mChirp;

   // mSample is an external placeholder to remember the last "buffer"
   // position so we use it to reinitialize from where we left
   sampleCount mSample;
   double mPositionInCycles;

   // If we made these static variables,
   // Tone and Chirp would share the same parameters.
   int mWaveform;
   int mInterpolation;
   double mFrequency[2];
   double mAmplitude[2];
   double mLogFrequency[2];

   NumericTextCtrl *mToneDurationT;

   DECLARE_EVENT_TABLE()
};

class EffectChirp final : public EffectToneGen
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectChirp() : EffectToneGen{ true } {}
};


class EffectTone final : public EffectToneGen
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectTone() : EffectToneGen{ false } {}
};

#endif
