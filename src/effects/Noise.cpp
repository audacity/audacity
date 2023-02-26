/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectNoise
\brief An effect to add white noise.

*//*******************************************************************/
#include "Noise.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/valgen.h>

#include "Prefs.h"
#include "ShuttleGui.h"
#include "valnum.h"
#include "NumericTextCtrl.h"

const EnumValueSymbol EffectNoise::kTypeStrings[nTypes] =
{
   // These are acceptable dual purpose internal/visible names
   /* i18n-hint: not a color, but "white noise" having a uniform spectrum  */
   { XC("White", "noise") },
   /* i18n-hint: not a color, but "pink noise" having a spectrum with more power
    in low frequencies */
   { XC("Pink", "noise") },
   /* i18n-hint: a kind of noise spectrum also known as "red" or "brown" */
   { XC("Brownian", "noise") }
};

const EffectParameterMethods& EffectNoise::Parameters() const
{
   static CapturedParameters<EffectNoise,
      Type, Amp
   > parameters;
   return parameters;
}

//
// EffectNoise
//

const ComponentInterfaceSymbol EffectNoise::Symbol
{ XO("Noise") };

namespace{ BuiltinEffectsModule::Registration< EffectNoise > reg; }


EffectNoise::EffectNoise()
{
   Parameters().Reset(*this);

   SetLinearEffectFlag(true);

   y = z = buf0 = buf1 = buf2 = buf3 = buf4 = buf5 = buf6 = 0;
}

EffectNoise::~EffectNoise()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectNoise::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectNoise::GetDescription() const
{
   return XO("Generates one of three different types of noise");
}

ManualPageID EffectNoise::ManualPage() const
{
   return L"Noise";
}

// EffectDefinitionInterface implementation

EffectType EffectNoise::GetType() const
{
   return EffectTypeGenerate;
}

unsigned EffectNoise::GetAudioOutCount() const
{
   return 1;
}

bool EffectNoise::ProcessInitialize(EffectSettings &,
   double sampleRate, ChannelNames)
{
   mSampleRate = sampleRate;
   return true;
}

size_t EffectNoise::ProcessBlock(EffectSettings &,
   const float *const *, float *const *outbuf, size_t size)
{
   float *buffer = outbuf[0];

   float white;
   float amplitude;
   float div = ((float) RAND_MAX) / 2.0f;

   switch (mType)
   {
   default:
   case kWhite: // white
       for (decltype(size) i = 0; i < size; i++)
       {
          buffer[i] = mAmp * ((rand() / div) - 1.0f);
       }
       break;

   case kPink: // pink
      // based on Paul Kellet's "instrumentation grade" algorithm.

      // 0.129f is an experimental normalization factor.
      amplitude = mAmp * 0.129f;
      for (decltype(size) i = 0; i < size; i++)
      {
         white = (rand() / div) - 1.0f;
         buf0 = 0.99886f * buf0 + 0.0555179f * white;
         buf1 = 0.99332f * buf1 + 0.0750759f * white;
         buf2 = 0.96900f * buf2 + 0.1538520f * white;
         buf3 = 0.86650f * buf3 + 0.3104856f * white;
         buf4 = 0.55000f * buf4 + 0.5329522f * white;
         buf5 = -0.7616f * buf5 - 0.0168980f * white;
         buffer[i] = amplitude *
            (buf0 + buf1 + buf2 + buf3 + buf4 + buf5 + buf6 + white * 0.5362);
         buf6 = white * 0.115926;
      }
      break;

   case kBrownian: // Brownian
      //float leakage=0.997; // experimental value at 44.1kHz
      //double scaling = 0.05; // experimental value at 44.1kHz
      // min and max protect against instability at extreme sample rates.
      float leakage = ((mSampleRate - 144.0) / mSampleRate < 0.9999)
         ? (mSampleRate - 144.0) / mSampleRate
         : 0.9999f;

      float scaling = (9.0 / sqrt(mSampleRate) > 0.01)
         ? 9.0 / sqrt(mSampleRate)
         : 0.01f;
 
      for (decltype(size) i = 0; i < size; i++)
      {
         white = (rand() / div) - 1.0f;
         z = leakage * y + white * scaling;
         y = fabs(z) > 1.0
            ? leakage * y - white * scaling
            : z;
         buffer[i] = mAmp * y;
      }
      break;
   }

   return size;
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectNoise::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();

   wxASSERT(nTypes == WXSIZEOF(kTypeStrings));

   S.StartMultiColumn(2, wxCENTER);
   {
      S.Validator<wxGenericValidator>(&mType)
         .AddChoice(XXO("&Noise type:"), Msgids(kTypeStrings, nTypes));

      S
         .Validator<FloatingPointValidator<double>>(
            6, &mAmp, NumValidatorStyle::NO_TRAILING_ZEROES, Amp.min, Amp.max )
         .AddTextBox(XXO("&Amplitude (0-1):"), L"", 12);

      S.AddPrompt(XXO("&Duration:"));
      auto &extra = access.Get().extra;
      mNoiseDurationT = safenew
         NumericTextCtrl(S.GetParent(), wxID_ANY,
                         NumericConverter::TIME,
                         extra.GetDurationFormat(),
                         extra.GetDuration(),
                         mProjectRate,
                         NumericTextCtrl::Options{}
                            .AutoPos(true));
      S.Name(XO("Duration"))
         .Position(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL)
         .AddWindow(mNoiseDurationT);
   }
   S.EndMultiColumn();
   return nullptr;
}

bool EffectNoise::TransferDataToWindow(const EffectSettings &settings)
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mNoiseDurationT->SetValue(settings.extra.GetDuration());
   return true;
}

bool EffectNoise::TransferDataFromWindow(EffectSettings &settings)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   settings.extra.SetDuration(mNoiseDurationT->GetValue());
   return true;
}
