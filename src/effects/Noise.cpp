/**********************************************************************

  Audacity: A Digital Audio Editor

  Noise.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectNoise
\brief An effect to add white noise.

*//*******************************************************************/

#include "../Audacity.h"
#include "Noise.h"

#include <math.h>

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/textctrl.h>
#include <wx/valgen.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../widgets/valnum.h"

enum kTypes
{
   kWhite,
   kPink,
   kBrownian,
   kNumTypes
};

static const wxChar *kTypeStrings[kNumTypes] =
{
   XO("White"),
   XO("Pink"),
   XO("Brownian")
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name    Type     Key               Def      Min   Max            Scale
Param( Type,   int,     XO("Type"),       kWhite,  0,    kNumTypes - 1, 1  );
Param( Amp,    double,  XO("Amplitude"),  0.8,     0.0,  1.0,           1  );

//
// EffectNoise
//

EffectNoise::EffectNoise()
{
   mType = DEF_Type;
   mAmp = DEF_Amp;

   SetLinearEffectFlag(true);

   y = z = buf0 = buf1 = buf2 = buf3 = buf4 = buf5 = buf6 = 0;
}

EffectNoise::~EffectNoise()
{
}

// IdentInterface implementation

wxString EffectNoise::GetSymbol()
{
   return NOISE_PLUGIN_SYMBOL;
}

wxString EffectNoise::GetDescription()
{
   return XO("Generates one of three different types of noise");
}

// EffectIdentInterface implementation

EffectType EffectNoise::GetType()
{
   return EffectTypeGenerate;
}

// EffectClientInterface implementation

unsigned EffectNoise::GetAudioOutCount()
{
   return 1;
}

size_t EffectNoise::ProcessBlock(float **WXUNUSED(inbuf), float **outbuf, size_t size)
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

bool EffectNoise::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Type, kTypeStrings[mType]);
   parms.Write(KEY_Amp, mAmp);

   return true;
}

bool EffectNoise::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyEnum(Type, wxArrayString(kNumTypes, kTypeStrings));
   ReadAndVerifyDouble(Amp);

   mType = Type;
   mAmp = Amp;

   return true;
}

// Effect implementation

bool EffectNoise::Startup()
{
   wxString base = wxT("/Effects/Noise/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      gPrefs->Read(base + wxT("Type"), &mType, 0L);
      gPrefs->Read(base + wxT("Amplitude"), &mAmp, 0.8f);

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

void EffectNoise::PopulateOrExchange(ShuttleGui & S)
{
   wxASSERT(kNumTypes == WXSIZEOF(kTypeStrings));

   wxArrayString typeChoices;
   for (int i = 0; i < kNumTypes; i++)
   {
      typeChoices.Add(wxGetTranslation(kTypeStrings[i]));
   }

   S.StartMultiColumn(2, wxCENTER);
   {
      S.AddChoice(_("Noise type:"), wxT(""), &typeChoices)->SetValidator(wxGenericValidator(&mType));

      FloatingPointValidator<double> vldAmp(6, &mAmp, NUM_VAL_NO_TRAILING_ZEROES);
      vldAmp.SetRange(MIN_Amp, MAX_Amp);
      S.AddTextBox(_("Amplitude (0-1):"), wxT(""), 12)->SetValidator(vldAmp);

      S.AddPrompt(_("Duration:"));
      mNoiseDurationT = safenew
         NumericTextCtrl(NumericConverter::TIME,
                         S.GetParent(),
                         wxID_ANY,
                         GetDurationFormat(),
                         GetDuration(),
                         mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
      mNoiseDurationT->SetName(_("Duration"));
      mNoiseDurationT->EnableMenu();
      S.AddWindow(mNoiseDurationT, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL);
   }
   S.EndMultiColumn();
}

bool EffectNoise::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mNoiseDurationT->SetValue(GetDuration());

   return true;
}

bool EffectNoise::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   SetDuration(mNoiseDurationT->GetValue());

   return true;
}
