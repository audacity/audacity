/**********************************************************************

  Audacity: A Digital Audio Editor

  DtmfGen.h

  Salvo Ventura
  Dec 2006

  An effect that generates DTMF tones

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DTMF__
#define __AUDACITY_EFFECT_DTMF__

#include "PerTrackEffect.h"
#include "../ShuttleAutomation.h"

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class NumericTextCtrl;
class ShuttleGui;

class EffectDtmf final : public PerTrackEffect
{
public:
   struct Settings;
   static inline Settings *
   FetchParameters(EffectDtmf &e, EffectSettings &) { return &e.mSettings; }
   static const ComponentInterfaceSymbol Symbol;

   EffectDtmf();
   virtual ~EffectDtmf();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;

private:
   // EffectDtmf implementation

   bool MakeDtmfTone(float *buffer, size_t len, float fs,
                     wxChar tone, sampleCount last,
                     sampleCount total, float amplitude);

private:
   sampleCount numSamplesSequence;  // total number of samples to generate
   sampleCount numSamplesTone;      // number of samples in a tone block
   sampleCount numSamplesSilence;   // number of samples in a silence block
   sampleCount diff;                // number of extra samples to redistribute
   sampleCount numRemaining;        // number of samples left to produce in the current block
   sampleCount curTonePos;          // position in tone to start the wave
   bool isTone;                     // true if block is tone, otherwise silence
   int curSeqPos;                   // index into dtmf tone string

public:
   struct Settings {
      static constexpr wchar_t DefaultSequence[] = L"audacity";
      static constexpr double DefaultDutyCycle = 55.0;
      static constexpr double DefaultAmplitude = 0.8;

      wxString dtmfSequence{DefaultSequence}; // dtmf tone string
      int    dtmfNTones = dtmfSequence.length(); // total number of tones to generate
      double dtmfTone{};               // duration of a single tone in ms
      double dtmfSilence{};            // duration of silence between tones in ms
      double dtmfDutyCycle{DefaultDutyCycle}; // ratio of dtmfTone/(dtmfTone+dtmfSilence)
      double dtmfAmplitude{DefaultAmplitude}; // amplitude of dtmf tone sequence, restricted to (0-1)

      void Recalculate(EffectSettings &settings);
   };

   struct Validator;

private:
   Settings mSettings;
   const EffectParameterMethods& Parameters() const override;

static constexpr EffectParameter Sequence{ &Settings::dtmfSequence,
   L"Sequence",   Settings::DefaultSequence, L"", L"", L""};
static constexpr EffectParameter DutyCycle{ &Settings::dtmfDutyCycle,
   L"Duty Cycle", Settings::DefaultDutyCycle, 0.0,     100.0,   10.0   };
static constexpr EffectParameter Amplitude{ &Settings::dtmfAmplitude,
   L"Amplitude",  Settings::DefaultAmplitude, 0.001,   1.0,     1      };
};

#endif
