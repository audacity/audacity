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

#include "../ShuttleGui.h"
#include "../widgets/NumericTextCtrl.h"

#include "Effect.h"

#define CHIRP_PLUGIN_SYMBOL XO("Chirp")
#define TONE_PLUGIN_SYMBOL XO("Tone")

class EffectToneGen : public Effect
{
public:
   EffectToneGen(bool isChirp);
   virtual ~EffectToneGen();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual int GetAudioOutCount();
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL);
   virtual sampleCount ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen);
   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

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

   DECLARE_EVENT_TABLE();
};

#endif
