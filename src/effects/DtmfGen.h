/**********************************************************************

  Audacity: A Digital Audio Editor

  DtmfGen.h

  Salvo Ventura
  Dec 2006

  An effect that generates DTMF tones

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DTMF__
#define __AUDACITY_EFFECT_DTMF__

#include <wx/event.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/string.h>

#include "../widgets/NumericTextCtrl.h"

#include "Effect.h"

class ShuttleGui;

#define DTMFTONES_PLUGIN_SYMBOL XO("DTMF Tones")

class EffectDtmf final : public Effect
{
public:
   EffectDtmf();
   virtual ~EffectDtmf();

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

   bool Startup() override;
   bool Init() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataFromWindow() override;
   bool TransferDataToWindow() override;

private:
   // EffectDtmf implementation

   bool MakeDtmfTone(float *buffer, size_t len, float fs,
                     wxChar tone, sampleCount last,
                     sampleCount total, float amplitude);
   void Recalculate();

   void UpdateUI();

   void OnSequence(wxCommandEvent & evt);
   void OnAmplitude(wxCommandEvent & evt);
   void OnDuration(wxCommandEvent & evt);
   void OnDutyCycle(wxCommandEvent & evt);

private:
   sampleCount numSamplesSequence;  // total number of samples to generate
   sampleCount numSamplesTone;      // number of samples in a tone block
   sampleCount numSamplesSilence;   // number of samples in a silence block
   sampleCount diff;                // number of extra samples to redistribute
   sampleCount numRemaining;        // number of samples left to produce in the current block
   sampleCount curTonePos;          // position in tone to start the wave
   bool isTone;                     // true if block is tone, otherwise silence
   int curSeqPos;                   // index into dtmf tone string

   wxString dtmfSequence;             // dtmf tone string
   int    dtmfNTones;               // total number of tones to generate
   double dtmfTone;                 // duration of a single tone in ms
   double dtmfSilence;              // duration of silence between tones in ms
   double dtmfDutyCycle;            // ratio of dtmfTone/(dtmfTone+dtmfSilence)
   double dtmfAmplitude;            // amplitude of dtmf tone sequence, restricted to (0-1)

   wxTextCtrl *mDtmfSequenceT;
   wxTextCtrl *mDtmfAmplitudeT;
   wxSlider   *mDtmfDutyCycleS;
   NumericTextCtrl *mDtmfDurationT;
   wxStaticText *mDtmfToneT;
   wxStaticText *mDtmfSilenceT;
   wxStaticText *mDtmfDutyT;

   DECLARE_EVENT_TABLE()
};

#endif
