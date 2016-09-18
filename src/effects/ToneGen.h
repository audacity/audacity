/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly

  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include <wx/arrstr.h>
#include <wx/string.h>

#include "../widgets/NumericTextCtrl.h"

#include "Effect.h"

class ShuttleGui;

#define CHIRP_PLUGIN_SYMBOL XO("Chirp")
#define TONE_PLUGIN_SYMBOL XO("Tone")

class EffectToneGen final : public Effect
{
public:
   EffectToneGen(bool isChirp);
   virtual ~EffectToneGen();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   unsigned GetAudioOutCount() override;
   bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataFromWindow();
   bool TransferDataToWindow();

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

   wxArrayString mWaveforms;
   wxArrayString mInterpolations;
   NumericTextCtrl *mToneDurationT;

   DECLARE_EVENT_TABLE()
};

#endif
