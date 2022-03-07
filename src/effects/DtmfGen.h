/**********************************************************************

  Audacity: A Digital Audio Editor

  DtmfGen.h

  Salvo Ventura
  Dec 2006

  An effect that generates DTMF tones

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DTMF__
#define __AUDACITY_EFFECT_DTMF__

#include "Effect.h"

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class NumericTextCtrl;
class ShuttleGui;

class EffectDtmf final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectDtmf();
   virtual ~EffectDtmf();

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
   // EffectDtmf implementation

   bool MakeDtmfTone(float *buffer, size_t len, float fs,
                     wxChar tone, sampleCount last,
                     sampleCount total, float amplitude);
   void Recalculate();

   void UpdateUI();

   void OnSequence(wxCommandEvent & evt);
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
   wxSlider   *mDtmfDutyCycleS;
   NumericTextCtrl *mDtmfDurationT;
   wxStaticText *mDtmfToneT;
   wxStaticText *mDtmfSilenceT;
   wxStaticText *mDtmfDutyT;

   DECLARE_EVENT_TABLE()
};

#endif
