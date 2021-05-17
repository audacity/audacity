/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectAmplify
\brief An Effect that makes a sound louder or softer.

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

*//*******************************************************************/


#include "Amplify.h"
#include "LoadEffects.h"

#include <math.h>
#include <float.h>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/log.h>

#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"


enum
{
   ID_Amp = 10000,
   ID_Peak,
   ID_Clip
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key                     Def         Min         Max            Scale
Param( Ratio,     float,   wxT("Ratio"),            0.9f,       0.003162f,  316.227766f,   1.0f  );
Param( Amp,       float,   wxT(""),                -0.91515f,  -50.0f,     50.0f,         10.0f );
Param( Clipping,  bool,    wxT("AllowClipping"),    false,    false,  true,    1  );

//
// EffectAmplify
//

const ComponentInterfaceSymbol EffectAmplify::Symbol
{ XO("Amplify") };

namespace{ BuiltinEffectsModule::Registration< EffectAmplify > reg; }

BEGIN_EVENT_TABLE(EffectAmplify, wxEvtHandler)
   EVT_SLIDER(ID_Amp, EffectAmplify::OnAmpSlider)
   EVT_TEXT(ID_Amp, EffectAmplify::OnAmpText)
   EVT_TEXT(ID_Peak, EffectAmplify::OnPeakText)
   EVT_CHECKBOX(ID_Clip, EffectAmplify::OnClipCheckBox)
END_EVENT_TABLE()

EffectAmplify::EffectAmplify()
{
   mAmp = DEF_Amp;
   mRatio = DB_TO_LINEAR(mAmp);
   mRatioClip = 0.0;
   mCanClip = false;
   mPeak = 0.0;

   SetLinearEffectFlag(true);
}

EffectAmplify::~EffectAmplify()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectAmplify::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectAmplify::GetDescription()
{
   // Note: This is useful only after ratio has been set.
   return XO("Increases or decreases the volume of the audio you have selected");
}

wxString EffectAmplify::ManualPage()
{
   return wxT("Amplify");
}

// EffectDefinitionInterface implementation

EffectType EffectAmplify::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

unsigned EffectAmplify::GetAudioInCount()
{
   return 1;
}

unsigned EffectAmplify::GetAudioOutCount()
{
   return 1;
}

size_t EffectAmplify::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   for (decltype(blockLen) i = 0; i < blockLen; i++)
   {
      outBlock[0][i] = inBlock[0][i] * mRatio;
   }

   return blockLen;
}
bool EffectAmplify::DefineParams( ShuttleParams & S ){
   S.SHUTTLE_PARAM( mRatio, Ratio );
   if (!IsBatchProcessing())
      S.SHUTTLE_PARAM( mCanClip, Clipping );
   return true;
}

bool EffectAmplify::GetAutomationParameters(CommandParameters & parms)
{
   parms.WriteFloat(KEY_Ratio, mRatio);
   if (!IsBatchProcessing())
      parms.WriteFloat(KEY_Clipping, mCanClip);

   return true;
}

bool EffectAmplify::SetAutomationParameters(CommandParameters & parms)
{
   ReadAndVerifyFloat(Ratio);
   mRatio = Ratio;

   if (!IsBatchProcessing()){
      ReadAndVerifyBool(Clipping);
      mCanClip = Clipping;
   } else {
      mCanClip = true;
   }

   return true;
}

bool EffectAmplify::LoadFactoryDefaults()
{
   Init();

   mRatioClip = 0.0;
   if (mPeak > 0.0)
   {
      mRatio = 1.0 / mPeak;
      mRatioClip = mRatio;
   }
   else
   {
      mRatio = 1.0;
   }
   mCanClip = false;

   return TransferDataToWindow();
}

// Effect implementation

bool EffectAmplify::Init()
{
   mPeak = 0.0;

   for (auto t : inputTracks()->Selected< const WaveTrack >())
   {
      auto pair = t->GetMinMax(mT0, mT1); // may throw
      const float min = pair.first, max = pair.second;
      float newpeak = (fabs(min) > fabs(max) ? fabs(min) : fabs(max));

      if (newpeak > mPeak)
      {
         mPeak = newpeak;
      }
   }

   return true;
}

void EffectAmplify::Preview(bool dryOnly)
{
   auto cleanup1 = valueRestorer( mRatio );
   auto cleanup2 = valueRestorer( mPeak );

   Effect::Preview(dryOnly);
}

void EffectAmplify::PopulateOrExchange(ShuttleGui & S)
{
   enum{ precision = 3 }; // allow (a generous) 3 decimal  places for Amplification (dB)

   bool batch = IsBatchProcessing();
   if ( batch )
   {
      mCanClip = true;
      mPeak = 1.0;
   }
   else 
   {
      if (mPeak > 0.0)
      {
         mRatio = 1.0 / mPeak;
         mRatioClip = mRatio;
      }
      else
      {
         mRatio = 1.0;
      }
   }

   S.AddSpace(0, 5);

   S.StartVerticalLay(0);
   {
      // Amplitude
      S.StartMultiColumn(2, wxCENTER);
      {
         mAmpT = S.Id(ID_Amp)
            .Validator<FloatingPointValidator<double>>(
               precision, &mAmp, NumValidatorStyle::ONE_TRAILING_ZERO, MIN_Amp, MAX_Amp
            )
            .AddTextBox(XXO("&Amplification (dB):"), wxT(""), 12);
      }
      S.EndMultiColumn();

      // Amplitude
      S.StartHorizontalLay(wxEXPAND);
      {
         mAmpS = S.Id(ID_Amp)
            .Style(wxSL_HORIZONTAL)
            .Name(XO("Amplification dB"))
            .AddSlider( {}, 0, MAX_Amp * SCL_Amp, MIN_Amp * SCL_Amp);
      }
      S.EndHorizontalLay();

      // Peak
      S.StartMultiColumn(2, wxCENTER);
      {
         mNewPeakT = S.Id(ID_Peak)
            .Validator<FloatingPointValidator<double>>(
               // One extra decimal place so that rounding is visible to user
               // (see: bug 958)
               precision + 1,
               &mNewPeak, NumValidatorStyle::ONE_TRAILING_ZERO,
               // min and max need same precision as what we're validating (bug 963)
               RoundValue( precision + 1, MIN_Amp + LINEAR_TO_DB(mPeak) ),
               RoundValue( precision + 1, MAX_Amp + LINEAR_TO_DB(mPeak) )
            )
            .AddTextBox(XXO("&New Peak Amplitude (dB):"), wxT(""), 12);
      }
      S.EndMultiColumn();

      // Clipping
      S.StartHorizontalLay(wxCENTER);
      {

         mClip = S.Id(ID_Clip).Disable( batch )
            .AddCheckBox(XXO("Allo&w clipping"), false);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   return;
}

bool EffectAmplify::TransferDataToWindow()
{
   // limit range of gain
   double dBInit = LINEAR_TO_DB(mRatio);
   double dB = TrapDouble(dBInit, MIN_Amp, MAX_Amp);
   if (dB != dBInit)
      mRatio = DB_TO_LINEAR(dB);

   mAmp = LINEAR_TO_DB(mRatio);
   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mAmp * SCL_Amp + 0.5f));

   mNewPeak = LINEAR_TO_DB(mRatio * mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   mClip->SetValue(mCanClip);

   CheckClip();

   return true;
}

bool EffectAmplify::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   mRatio = DB_TO_LINEAR(TrapDouble(mAmp * SCL_Amp, MIN_Amp * SCL_Amp, MAX_Amp * SCL_Amp) / SCL_Amp);

   mCanClip = mClip->GetValue();

   if (!mCanClip && mRatio * mPeak > 1.0)
   {
      mRatio = 1.0 / mPeak;
   }

   return true;
}

// EffectAmplify implementation

void EffectAmplify::CheckClip()
{
   EnableApply(mClip->GetValue() || (mPeak > 0.0 && mRatio <= mRatioClip));
}

void EffectAmplify::OnAmpText(wxCommandEvent & WXUNUSED(evt))
{
   if (!mAmpT->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   mRatio = DB_TO_LINEAR(TrapDouble(mAmp * SCL_Amp, MIN_Amp * SCL_Amp, MAX_Amp * SCL_Amp) / SCL_Amp);

   mAmpS->SetValue((int) (LINEAR_TO_DB(mRatio) * SCL_Amp + 0.5));

   mNewPeak = LINEAR_TO_DB(mRatio * mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}

void EffectAmplify::OnPeakText(wxCommandEvent & WXUNUSED(evt))
{
   if (!mNewPeakT->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   if (mNewPeak == 0.0)
      mRatio = mRatioClip;
   else
      mRatio = DB_TO_LINEAR(mNewPeak) / mPeak;

   double ampInit = LINEAR_TO_DB(mRatio);
   mAmp = TrapDouble(ampInit, MIN_Amp, MAX_Amp);
   if (mAmp != ampInit)
      mRatio = DB_TO_LINEAR(mAmp);

   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mAmp * SCL_Amp + 0.5f));

   CheckClip();
}

void EffectAmplify::OnAmpSlider(wxCommandEvent & evt)
{
   double dB = evt.GetInt() / SCL_Amp;
   mRatio = DB_TO_LINEAR(TrapDouble(dB, MIN_Amp, MAX_Amp));

   double dB2 = (evt.GetInt() - 1) / SCL_Amp;
   double ratio2 = DB_TO_LINEAR(TrapDouble(dB2, MIN_Amp, MAX_Amp));

   if (!mClip->GetValue() && mRatio * mPeak > 1.0 && ratio2 * mPeak < 1.0)
   {
      mRatio = 1.0 / mPeak;
   }

   mAmp = LINEAR_TO_DB(mRatio);
   mAmpT->GetValidator()->TransferToWindow();

   mNewPeak = LINEAR_TO_DB(mRatio * mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}

void EffectAmplify::OnClipCheckBox(wxCommandEvent & WXUNUSED(evt))
{
   CheckClip();
}
