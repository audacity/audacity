/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.cpp

  Steve Jolly
  James Crook (Adapted for 'Chirps')

  This class implements a tone generator effect.

*******************************************************************//**

\class EffectToneGen
\brief An Effect that can generate a sine, square or sawtooth wave.
An extended mode of EffectToneGen supports 'chirps' where the
frequency changes smoothly during the tone.

*//*******************************************************************/


#include "ToneGen.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/choice.h>
#include <wx/valgen.h>

#include "Project.h"
#include "ProjectRate.h"
#include "ShuttleGui.h"
#include "valnum.h"
#include "../widgets/NumericTextCtrl.h"

const EnumValueSymbol EffectToneGen::kInterStrings[nInterpolations] =
{
   // These are acceptable dual purpose internal/visible names
   { XO("Linear") },
   { XO("Logarithmic") }
};

const EnumValueSymbol EffectToneGen::kWaveStrings[nWaveforms] =
{
   { XO("Sine") },
   { XO("Square") },
   { XO("Sawtooth") },
   { XO("Square, no alias") },
   { XC("Triangle", "waveform") }
};

const EffectParameterMethods& EffectToneGen::Parameters() const
{
   static const auto postSet =
   [](EffectToneGen &, EffectSettings &, EffectToneGen &e, bool updating) {
      if (updating)
         e.PostSet();
      return true;
   };
   static CapturedParameters<EffectToneGen,
      StartFreq, EndFreq, StartAmp, EndAmp, Waveform, Interp
   > chirpParameters{ postSet };
   static CapturedParameters<EffectToneGen,
      Frequency, Amplitude, Waveform, Interp
   > toneParameters{ postSet };
   if (mChirp)
      return chirpParameters;
   else
      return toneParameters;
}

//
// EffectToneGen
//

const ComponentInterfaceSymbol EffectChirp::Symbol
{ XO("Chirp") };

namespace{ BuiltinEffectsModule::Registration< EffectChirp > reg; }

const ComponentInterfaceSymbol EffectTone::Symbol
{ XO("Tone") };

namespace{ BuiltinEffectsModule::Registration< EffectTone > reg2; }

BEGIN_EVENT_TABLE(EffectToneGen, wxEvtHandler)
    EVT_TEXT(wxID_ANY, EffectToneGen::OnControlUpdate)
END_EVENT_TABLE();

EffectToneGen::EffectToneGen(bool isChirp)
   : mChirp{ isChirp }
{
   Parameters().Reset(*this);

   wxASSERT(nWaveforms == WXSIZEOF(kWaveStrings));
   wxASSERT(nInterpolations == WXSIZEOF(kInterStrings));

   // Chirp varies over time so must use selected duration.
   // TODO: When previewing, calculate only the first 'preview length'.
   if (isChirp)
      SetLinearEffectFlag(false);
   else
      SetLinearEffectFlag(true);
}

EffectToneGen::~EffectToneGen()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectToneGen::GetSymbol() const
{
   return mChirp
      ? EffectChirp::Symbol
      : EffectTone::Symbol;
}

TranslatableString EffectToneGen::GetDescription() const
{
   return mChirp
      ? XO("Generates an ascending or descending tone of one of four types")
      : XO("Generates a constant frequency tone of one of four types");
}

ManualPageID EffectToneGen::ManualPage() const
{
   return mChirp
      ? L"Chirp"
      : L"Tone";
}

// EffectDefinitionInterface implementation

EffectType EffectToneGen::GetType() const
{
   return EffectTypeGenerate;
}

unsigned EffectToneGen::GetAudioOutCount() const
{
   return 1;
}

bool EffectToneGen::ProcessInitialize(
   EffectSettings &, double sampleRate, ChannelNames chanMap)
{
   mSampleRate = sampleRate;
   mPositionInCycles = 0.0;
   mSample = 0;
   return true;
}

size_t EffectToneGen::ProcessBlock(EffectSettings &,
   const float *const *, float *const *outBlock, size_t blockLen)
{
   float *buffer = outBlock[0];
   double throwaway = 0;        //passed to modf but never used
   double f = 0.0;
   double a, b;
   int k;

   double frequencyQuantum;
   double BlendedFrequency;
   double BlendedAmplitude;
   double BlendedLogFrequency = 0.0;

   // calculate delta, and reposition from where we left
   auto doubleSampleCount = mSampleCnt.as_double();
   auto doubleSample = mSample.as_double();
   double amplitudeQuantum =
      (mAmplitude1 - mAmplitude0) / doubleSampleCount;
   BlendedAmplitude = mAmplitude0 +
      amplitudeQuantum * doubleSample;

   // precalculations:
   double pre2PI = 2.0 * M_PI;
   double pre4divPI = 4.0 / M_PI;

   // initial setup should calculate deltas
   if (mInterpolation == kLogarithmic)
   {
      // this for log interpolation
      mLogFrequency[0] = log10(mFrequency0);
      mLogFrequency[1] = log10(mFrequency1);
      // calculate delta, and reposition from where we left
      frequencyQuantum = (mLogFrequency[1] - mLogFrequency[0]) / doubleSampleCount;
      BlendedLogFrequency = mLogFrequency[0] + frequencyQuantum * doubleSample;
      BlendedFrequency = pow(10.0, BlendedLogFrequency);
   }
   else
   {
      // this for regular case, linear interpolation
      frequencyQuantum = (mFrequency1 - mFrequency0) / doubleSampleCount;
      BlendedFrequency = mFrequency0 + frequencyQuantum * doubleSample;
   }

   // synth loop
   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      switch (mWaveform)
      {
      case kSine:
         f = sin(pre2PI * mPositionInCycles / mSampleRate);
         break;
      case kSquare:
         f = (modf(mPositionInCycles / mSampleRate, &throwaway) < 0.5) ? 1.0 : -1.0;
         break;
      case kSawtooth:
         f = (2.0 * modf(mPositionInCycles / mSampleRate + 0.5, &throwaway)) - 1.0;
         break;
      case kTriangle:
         f = modf(mPositionInCycles / mSampleRate, &throwaway);
         if(f < 0.25) {
             f *= 4.0;
         } else if(f > 0.75) {
             f = (f - 1.0) * 4.0;
         } else { /* f >= 0.25 || f <= 0.75 */
             f = (0.5 - f) * 4.0;
         }
         break;
      case kSquareNoAlias:    // Good down to 110Hz @ 44100Hz sampling.
         //do fundamental (k=1) outside loop
         b = (1.0 + cos((pre2PI * BlendedFrequency) / mSampleRate)) / pre4divPI;  //scaling
         f = pre4divPI * sin(pre2PI * mPositionInCycles / mSampleRate);
         for (k = 3; (k < 200) && (k * BlendedFrequency < mSampleRate / 2.0); k += 2)
         {
            //Hann Window in freq domain
            a = 1.0 + cos((pre2PI * k * BlendedFrequency) / mSampleRate);
            //calc harmonic, apply window, scale to amplitude of fundamental
            f += a * sin(pre2PI * mPositionInCycles / mSampleRate * k) / (b * k);
         }
      }
      // insert value in buffer
      buffer[i] = (float) (BlendedAmplitude * f);
      // update freq,amplitude
      mPositionInCycles += BlendedFrequency;
      BlendedAmplitude += amplitudeQuantum;
      if (mInterpolation == kLogarithmic)
      {
         BlendedLogFrequency += frequencyQuantum;
         BlendedFrequency = pow(10.0, BlendedLogFrequency);
      }
      else
      {
         BlendedFrequency += frequencyQuantum;
      }
   }

   // update external placeholder
   mSample += blockLen;

   return blockLen;
}

void EffectToneGen::PostSet()
{
   if (!mChirp) {
      mFrequency1 = mFrequency0;
      mAmplitude1 = mAmplitude0;
   }
//   double freqMax =
//      (FindProject()
//         ? ProjectRate::Get( *FindProject() ).GetRate()
//         : 44100.0)
//      / 2.0;
//   mFrequency1 = std::clamp<double>(mFrequency1, EndFreq.min, freqMax);
}

// Effect implementation

std::unique_ptr<EffectUIValidator> EffectToneGen::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
   wxTextCtrl *t;

   S.StartMultiColumn(2, wxCENTER);
   {
      S.Validator<wxGenericValidator>(&mWaveform)
         .AddChoice(XXO("&Waveform:"),
            Msgids( kWaveStrings, nWaveforms ) );

      if (mChirp)
      {
         S.AddFixedText( {} );
         S.StartHorizontalLay(wxEXPAND);
         {
            S.StartHorizontalLay(wxLEFT, 50);
            {
               S.AddTitle(XO("Start"));
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxLEFT, 50);
            {
               S.AddTitle(XO("End"));
            }
            S.EndHorizontalLay();
         }
         S.EndHorizontalLay();

         S.AddPrompt(XXO("&Frequency (Hz):"));
         S.StartHorizontalLay(wxEXPAND);
         {
            S.StartHorizontalLay(wxLEFT, 50);
            {
               t = S.Name(XO("Frequency Hertz Start"))
                  .Validator<FloatingPointValidator<double>>(
                     6, &mFrequency0,
                     NumValidatorStyle::NO_TRAILING_ZEROES,
                     StartFreq.min,
                     mProjectRate / 2.0 )
                  .AddTextBox( {}, L"", 12);
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxLEFT, 50);
            {
               t = S.Name(XO("Frequency Hertz End"))
                  .Validator<FloatingPointValidator<double>>(
                     6, &mFrequency1,
                     NumValidatorStyle::NO_TRAILING_ZEROES,
                     EndFreq.min,
                     mProjectRate / 2.0 )
                  .AddTextBox( {}, L"", 12);
            }
            S.EndHorizontalLay();
         }
         S.EndHorizontalLay();

         S.AddPrompt(XXO("&Amplitude (0-1):"));
         S.StartHorizontalLay(wxEXPAND);
         {
            S.StartHorizontalLay(wxLEFT, 50);
            {
               t = S.Name(XO("Amplitude Start"))
                  .Validator<FloatingPointValidator<double>>(
                     6, &mAmplitude0, NumValidatorStyle::NO_TRAILING_ZEROES,
                     StartAmp.min, StartAmp.max )
                  .AddTextBox( {}, L"", 12);
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxLEFT, 50);
            {
               t = S.Name(XO("Amplitude End"))
                  .Validator<FloatingPointValidator<double>>(
                     6, &mAmplitude1, NumValidatorStyle::NO_TRAILING_ZEROES,
                     EndAmp.min, EndAmp.max )
                  .AddTextBox( {}, L"", 12);
            }
            S.EndHorizontalLay();
         }
         S.EndHorizontalLay();

         S.Validator<wxGenericValidator>(&mInterpolation)
            .AddChoice(XXO("I&nterpolation:"),
               Msgids( kInterStrings, nInterpolations ) );
      }
      else
      {
         t = S.Validator<FloatingPointValidator<double>>(
               6, &mFrequency0, NumValidatorStyle::NO_TRAILING_ZEROES,
               Frequency.min,
               mProjectRate / 2.0 )
            .AddTextBox(XXO("&Frequency (Hz):"), L"", 12);

         t = S.Validator<FloatingPointValidator<double>>(
               6, &mAmplitude0, NumValidatorStyle::NO_TRAILING_ZEROES,
               Amplitude.min, Amplitude.max )
            .AddTextBox(XXO("&Amplitude (0-1):"), L"", 12);
      }

      S.AddPrompt(XXO("&Duration:"));
      auto &extra = access.Get().extra;
      mToneDurationT = safenew
         NumericTextCtrl(S.GetParent(), wxID_ANY,
                         NumericConverter::TIME,
                         extra.GetDurationFormat(),
                         extra.GetDuration(),
                         mProjectRate,
                         NumericTextCtrl::Options{}
                            .AutoPos(true));
      S.Name(XO("Duration"))
         .Position(wxALIGN_LEFT | wxALL)
         .AddWindow(mToneDurationT);
   }
   S.EndMultiColumn();

   return nullptr;
}

bool EffectToneGen::TransferDataToWindow(const EffectSettings &settings)
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mToneDurationT->SetValue(settings.extra.GetDuration());
   return true;
}

bool EffectToneGen::TransferDataFromWindow(EffectSettings &settings)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   if (!mChirp)
   {
      mFrequency1 = mFrequency0;
      mAmplitude1 = mAmplitude0;
   }

   settings.extra.SetDuration(mToneDurationT->GetValue());

   return true;
}

// EffectToneGen implementation

void EffectToneGen::OnControlUpdate(wxCommandEvent & WXUNUSED(evt))
{
   if (!EffectUIValidator::EnableApply(
      mUIParent, mUIParent->TransferDataFromWindow()))
   {
      return;
   }
}
