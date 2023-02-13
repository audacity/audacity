/**********************************************************************

  Audacity: A Digital Audio Editor

  DtmfGen.h

  Salvo Ventura
  Dec 2006

  An effect that generates DTMF tones

**********************************************************************/

#ifndef __AUDACITY_EFFECT_DTMF__
#define __AUDACITY_EFFECT_DTMF__

#include "StatefulPerTrackEffect.h"
#include "ShuttleAutomation.h"

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class NumericTextCtrl;
class ShuttleGui;

struct DtmfSettings {
   static constexpr wchar_t DefaultSequence[] = L"audacity";
   static constexpr double DefaultDutyCycle = 55.0;
   static constexpr double DefaultAmplitude = 0.8;

   wxString dtmfSequence{DefaultSequence}; // dtmf tone string
   size_t dtmfNTones = dtmfSequence.length(); // total number of tones to generate
   double dtmfTone{};               // duration of a single tone in ms
   double dtmfSilence{};            // duration of silence between tones in ms
   double dtmfDutyCycle{DefaultDutyCycle}; // ratio of dtmfTone/(dtmfTone+dtmfSilence)
   double dtmfAmplitude{DefaultAmplitude}; // amplitude of dtmf tone sequence, restricted to (0-1)

   void Recalculate(EffectSettings &settings);
};

class EffectDtmf final
   : public EffectWithSettings<DtmfSettings, PerTrackEffect>
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

   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;

   struct Instance;
   std::shared_ptr<EffectInstance> MakeInstance() const override;

private:
   // EffectDtmf implementation

   static bool MakeDtmfTone(float *buffer, size_t len, float fs,
                     wxChar tone, sampleCount last,
                     sampleCount total, float amplitude);

public:
   struct Validator;

private:
   const EffectParameterMethods& Parameters() const override;

static constexpr EffectParameter Sequence{ &DtmfSettings::dtmfSequence,
   L"Sequence",   DtmfSettings::DefaultSequence, L"", L"", L""};
static constexpr EffectParameter DutyCycle{ &DtmfSettings::dtmfDutyCycle,
   L"Duty Cycle", DtmfSettings::DefaultDutyCycle, 0.0,     100.0,   10.0   };
static constexpr EffectParameter Amplitude{ &DtmfSettings::dtmfAmplitude,
   L"Amplitude",  DtmfSettings::DefaultAmplitude, 0.001,   1.0,     1      };
};

#endif
