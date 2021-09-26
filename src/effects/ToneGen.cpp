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
#include <float.h>

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/valgen.h>

#include "Project.h"
#include "ProjectRate.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"
#include "../widgets/NumericTextCtrl.h"

enum kInterpolations
{
   kLinear,
   kLogarithmic,
   nInterpolations
};

static const EnumValueSymbol kInterStrings[nInterpolations] =
{
   // These are acceptable dual purpose internal/visible names
   { XO("Linear") },
   { XO("Logarithmic") }
};

enum kWaveforms
{
   kSine,
   kSquare,
   kSawtooth,
   kSquareNoAlias,
   kTriangle,
   nWaveforms
};

static const EnumValueSymbol kWaveStrings[nWaveforms] =
{
   { XO("Sine") },
   { XO("Square") },
   { XO("Sawtooth") },
   { XO("Square, no alias") },
   { XO("Triangle") }
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key                  Def      Min      Max                     Scale
Param( StartFreq, double,  wxT("StartFreq"),     440.0,   1.0,     DBL_MAX,                1  );
Param( EndFreq,   double,  wxT("EndFreq"),       1320.0,  1.0,     DBL_MAX,                1  );
Param( StartAmp,  double,  wxT("StartAmp"),      0.8,     0.0,     1.0,                    1  );
Param( EndAmp,    double,  wxT("EndAmp"),        0.1,     0.0,     1.0,                    1  );
Param( Frequency, double,  wxT("Frequency"),     440.0,   1.0,     DBL_MAX,                1  );
Param( Amplitude, double,  wxT("Amplitude"),     0.8,     0.0,     1.0,                    1  );
Param( Waveform,  int,     wxT("Waveform"),      0,       0,       nWaveforms - 1,      1  );
Param( Interp,    int,     wxT("Interpolation"), 0,       0,       nInterpolations - 1, 1  );

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
{
   wxASSERT(nWaveforms == WXSIZEOF(kWaveStrings));
   wxASSERT(nInterpolations == WXSIZEOF(kInterStrings));

   mChirp = isChirp;

   mWaveform = DEF_Waveform;
   mFrequency[0] = DEF_StartFreq;
   mFrequency[1] = DEF_EndFreq;
   mAmplitude[0] = DEF_StartAmp;
   mAmplitude[1] = DEF_EndAmp;
   mInterpolation = DEF_Interp;

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

ComponentInterfaceSymbol EffectToneGen::GetSymbol()
{
   return mChirp
      ? EffectChirp::Symbol
      : EffectTone::Symbol;
}

TranslatableString EffectToneGen::GetDescription()
{
   return mChirp
      ? XO("Generates an ascending or descending tone of one of four types")
      : XO("Generates a constant frequency tone of one of four types");
}

ManualPageID EffectToneGen::ManualPage()
{
   return mChirp
      ? L"Chirp"
      : L"Tone";
}

// EffectDefinitionInterface implementation

EffectType EffectToneGen::GetType()
{
   return EffectTypeGenerate;
}

// EffectClientInterface implementation

unsigned EffectToneGen::GetAudioOutCount()
{
   return 1;
}

bool EffectToneGen::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   mPositionInCycles = 0.0;
   mSample = 0;

   return true;
}

size_t EffectToneGen::ProcessBlock(float **WXUNUSED(inBlock), float **outBlock, size_t blockLen)
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
      (mAmplitude[1] - mAmplitude[0]) / doubleSampleCount;
   BlendedAmplitude = mAmplitude[0] +
      amplitudeQuantum * doubleSample;

   // precalculations:
   double pre2PI = 2.0 * M_PI;
   double pre4divPI = 4.0 / M_PI;

   // initial setup should calculate deltas
   if (mInterpolation == kLogarithmic)
   {
      // this for log interpolation
      mLogFrequency[0] = log10(mFrequency[0]);
      mLogFrequency[1] = log10(mFrequency[1]);
      // calculate delta, and reposition from where we left
      frequencyQuantum = (mLogFrequency[1] - mLogFrequency[0]) / doubleSampleCount;
      BlendedLogFrequency = mLogFrequency[0] + frequencyQuantum * doubleSample;
      BlendedFrequency = pow(10.0, BlendedLogFrequency);
   }
   else
   {
      // this for regular case, linear interpolation
      frequencyQuantum = (mFrequency[1] - mFrequency[0]) / doubleSampleCount;
      BlendedFrequency = mFrequency[0] + frequencyQuantum * doubleSample;
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

bool EffectToneGen::DefineParams( ShuttleParams & S ){
   if( mChirp ){
      S.SHUTTLE_PARAM( mFrequency[0], StartFreq  );
      S.SHUTTLE_PARAM( mFrequency[1], EndFreq  );
      S.SHUTTLE_PARAM( mAmplitude[0], StartAmp  );
      S.SHUTTLE_PARAM( mAmplitude[1], EndAmp  );
   } else {
      S.SHUTTLE_PARAM( mFrequency[0], Frequency  );
      S.SHUTTLE_PARAM( mAmplitude[0], Amplitude );
      // Slightly hacky way to set freq and ampl
      // since we do this whatever query to params was made.
      mFrequency[1] = mFrequency[0];
      mAmplitude[1] = mAmplitude[0];
   }
   S.SHUTTLE_ENUM_PARAM( mWaveform, Waveform, kWaveStrings, nWaveforms  );
   S.SHUTTLE_ENUM_PARAM( mInterpolation, Interp, kInterStrings, nInterpolations  );


//   double freqMax = (FindProject() ? FindProject()->GetRate() : 44100.0) / 2.0;
//   mFrequency[1] = TrapDouble(mFrequency[1], MIN_EndFreq, freqMax);


   return true;
}

bool EffectToneGen::GetAutomationParameters(CommandParameters & parms)
{
   if (mChirp)
   {
      parms.Write(KEY_StartFreq, mFrequency[0]);
      parms.Write(KEY_EndFreq, mFrequency[1]);
      parms.Write(KEY_StartAmp, mAmplitude[0]);
      parms.Write(KEY_EndAmp, mAmplitude[1]);
   }
   else
   {
      parms.Write(KEY_Frequency, mFrequency[0]);
      parms.Write(KEY_Amplitude, mAmplitude[0]);
   }

   parms.Write(KEY_Waveform, kWaveStrings[mWaveform].Internal());
   parms.Write(KEY_Interp, kInterStrings[mInterpolation].Internal());

   return true;
}

bool EffectToneGen::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyEnum(Waveform,  kWaveStrings, nWaveforms);
   ReadAndVerifyEnum(Interp, kInterStrings, nInterpolations);
   if (mChirp)
   {
      ReadAndVerifyDouble(StartFreq);
      ReadAndVerifyDouble(EndFreq);
      ReadAndVerifyDouble(StartAmp);
      ReadAndVerifyDouble(EndAmp);
      mFrequency[0] = StartFreq;
      mFrequency[1] = EndFreq;
      mAmplitude[0] = StartAmp;
      mAmplitude[1] = EndAmp;
   }
   else
   {
      ReadAndVerifyDouble(Frequency);
      ReadAndVerifyDouble(Amplitude);
      mFrequency[0] = Frequency;
      mFrequency[1] = Frequency;
      mAmplitude[0] = Amplitude;
      mAmplitude[1] = Amplitude;
   }

   mWaveform = Waveform;
   mInterpolation = Interp;

   double freqMax =
      (FindProject()
         ? ProjectRate::Get( *FindProject() ).GetRate()
         : 44100.0)
      / 2.0;
   mFrequency[1] = TrapDouble(mFrequency[1], MIN_EndFreq, freqMax);

   return true;
}

// Effect implementation

void EffectToneGen::PopulateOrExchange(ShuttleGui & S)
{
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
                     6, &mFrequency[0],
                     NumValidatorStyle::NO_TRAILING_ZEROES,
                     MIN_StartFreq,
                     mProjectRate / 2.0
                  )
                  .AddTextBox( {}, wxT(""), 12);
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxLEFT, 50);
            {
               t = S.Name(XO("Frequency Hertz End"))
                  .Validator<FloatingPointValidator<double>>(
                     6, &mFrequency[1],
                     NumValidatorStyle::NO_TRAILING_ZEROES,
                     MIN_EndFreq,
                     mProjectRate / 2.0
                  )
                  .AddTextBox( {}, wxT(""), 12);
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
                     6, &mAmplitude[0], NumValidatorStyle::NO_TRAILING_ZEROES,
                     MIN_StartAmp, MAX_StartAmp
                  )
                  .AddTextBox( {}, wxT(""), 12);
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxLEFT, 50);
            {
               t = S.Name(XO("Amplitude End"))
                  .Validator<FloatingPointValidator<double>>(
                     6, &mAmplitude[1], NumValidatorStyle::NO_TRAILING_ZEROES,
                     MIN_EndAmp, MAX_EndAmp
                  )
                  .AddTextBox( {}, wxT(""), 12);
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
               6, &mFrequency[0], NumValidatorStyle::NO_TRAILING_ZEROES,
               MIN_Frequency,
               mProjectRate / 2.0
            )
            .AddTextBox(XXO("&Frequency (Hz):"), wxT(""), 12);

         t = S.Validator<FloatingPointValidator<double>>(
               6, &mAmplitude[0], NumValidatorStyle::NO_TRAILING_ZEROES,
               MIN_Amplitude, MAX_Amplitude
            )
            .AddTextBox(XXO("&Amplitude (0-1):"), wxT(""), 12);
      }

      S.AddPrompt(XXO("&Duration:"));
      mToneDurationT = safenew
         NumericTextCtrl(S.GetParent(), wxID_ANY,
                         NumericConverter::TIME,
                         GetDurationFormat(),
                         GetDuration(),
                         mProjectRate,
                         NumericTextCtrl::Options{}
                            .AutoPos(true));
      S.Name(XO("Duration"))
         .Position(wxALIGN_LEFT | wxALL)
         .AddWindow(mToneDurationT);
   }
   S.EndMultiColumn();

   return;
}

bool EffectToneGen::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mToneDurationT->SetValue(GetDuration());

   return true;
}

bool EffectToneGen::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   if (!mChirp)
   {
      mFrequency[1] = mFrequency[0];
      mAmplitude[1] = mAmplitude[0];
   }

   SetDuration(mToneDurationT->GetValue());

   return true;
}

// EffectToneGen implementation

void EffectToneGen::OnControlUpdate(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }
}
