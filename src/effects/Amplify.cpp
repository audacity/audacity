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

#include "../Audacity.h"

#include <math.h>
#include <float.h>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/log.h>

#include "../WaveTrack.h"
#include "../widgets/valnum.h"

#include "Amplify.h"


enum
{
   ID_Amp = 10000,
   ID_Peak,
   ID_Clip
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key                     Def         Min         Max            Scale
Param( Ratio,     float,   XO("Ratio"),            0.9f,       0.003162f,  316.227766f,   1.0f  );
Param( Amp,       float,   wxT(""),                -0.91515f,  -50.0f,     50.0f,         10.0f );

//
// EffectAmplify
//

BEGIN_EVENT_TABLE(EffectAmplify, wxEvtHandler)
   EVT_SLIDER(ID_Amp, EffectAmplify::OnAmpSlider)
   EVT_TEXT(ID_Amp, EffectAmplify::OnAmpText)
   EVT_TEXT(ID_Peak, EffectAmplify::OnPeakText)
   EVT_CHECKBOX(ID_Clip, EffectAmplify::OnClipCheckBox)
END_EVENT_TABLE()

EffectAmplify::EffectAmplify()
{
   mAmp = DEF_Amp;
   mRatio = pow(10.0, mAmp / 20.0);
   mRatioClip = 0.0;
   mCanClip = false;
   mPeak = 0.0;

   SetLinearEffectFlag(true);
}

EffectAmplify::~EffectAmplify()
{
}

// IdentInterface implementation

wxString EffectAmplify::GetSymbol()
{
   return AMPLIFY_PLUGIN_SYMBOL;
}

wxString EffectAmplify::GetDescription()
{
   // Note: This is useful only after ratio has been set.
   return XO("Increases or decreases the volume of the audio you have selected");
}

// EffectIdentInterface implementation

EffectType EffectAmplify::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

int EffectAmplify::GetAudioInCount()
{
   return 1;
}

int EffectAmplify::GetAudioOutCount()
{
   return 1;
}

sampleCount EffectAmplify::ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen)
{
   for (sampleCount i = 0; i < blockLen; i++)
   {
      outBlock[0][i] = inBlock[0][i] * mRatio;
   }

   return blockLen;
}

bool EffectAmplify::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.WriteFloat(KEY_Ratio, mRatio);

   return true;
}

bool EffectAmplify::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyFloat(Ratio);

   mRatio = Ratio;

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

   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);

   for (Track *t = iter.First(); t; t = iter.Next())
   {
      float min, max;
      ((WaveTrack *)t)->GetMinMax(&min, &max, mT0, mT1);
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
   double ratio = mRatio;
   double peak = mPeak;

   Effect::Preview(dryOnly);

   mRatio = ratio;
   mPeak = peak;
}

void EffectAmplify::PopulateOrExchange(ShuttleGui & S)
{
   if (IsBatchProcessing())
   {
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
         FloatingPointValidator<double> vldAmp(2, &mAmp);
         vldAmp.SetRange(MIN_Amp, MAX_Amp);
         mAmpT = S.Id(ID_Amp).AddTextBox(_("Amplification (dB):"), wxT(""), 12);
         mAmpT->SetValidator(vldAmp);
      }
      S.EndMultiColumn();

      // Amplitude
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetStyle(wxSL_HORIZONTAL);
         mAmpS = S.Id(ID_Amp).AddSlider(wxT(""), 0, MAX_Amp * SCL_Amp, MIN_Amp * SCL_Amp);
         mAmpS->SetName(_("Amplification dB"));
      }
      S.EndHorizontalLay();

      // Peak
      S.StartMultiColumn(2, wxCENTER);
      {
         int precission = 2;
         FloatingPointValidator<double> vldNewPeak(precission, &mNewPeak);
         double minAmp = MIN_Amp + (20.0 * log10(mPeak));
         double maxAmp = MAX_Amp + (20.0 * log10(mPeak));

         // min and max need same precision as what we're validating (bug 963)
         minAmp = Internat::CompatibleToDouble(Internat::ToString(minAmp, precission));
         maxAmp = Internat::CompatibleToDouble(Internat::ToString(maxAmp, precission));

         vldNewPeak.SetRange(minAmp, maxAmp);
         mNewPeakT = S.Id(ID_Peak).AddTextBox(_("New Peak Amplitude (dB):"), wxT(""), 12);
         mNewPeakT->SetValidator(vldNewPeak);
      }
      S.EndMultiColumn();

      // Clipping
      S.StartHorizontalLay(wxCENTER);
      {
         mClip = S.Id(ID_Clip).AddCheckBox(_("Allow clipping"), wxT("false"));
         if (IsBatchProcessing())
         {
            mClip->Enable(false);
            mCanClip = true;
         }
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   return;
}

bool EffectAmplify::TransferDataToWindow()
{
   // limit range of gain
   double dBInit = 20.0*log10(mRatio);
   double dB = TrapDouble(dBInit, MIN_Amp, MAX_Amp);
   if (dB != dBInit)
      mRatio = pow(10.0, dB / 20.0);

   mAmp = 20.0 * log10(mRatio);
   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mAmp * SCL_Amp + 0.5f));

   mNewPeak = 20.0 * log10(mRatio * mPeak);
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

   mRatio = pow(10.0, TrapDouble(mAmp * SCL_Amp, MIN_Amp * SCL_Amp, MAX_Amp * SCL_Amp) / (20.0 * SCL_Amp));

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

   mRatio = pow(10.0, TrapDouble(mAmp * SCL_Amp, MIN_Amp * SCL_Amp, MAX_Amp * SCL_Amp) / (20.0 * SCL_Amp));

   mAmpS->SetValue((int) (20.0 * log10(mRatio) * SCL_Amp + 0.5));

   mNewPeak = 20.0 * log10(mRatio * mPeak);
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
      mRatio = pow(10.0, mNewPeak / 20.0) / mPeak;

   double ampInit = 20.0 * log10(mRatio);
   mAmp = TrapDouble(ampInit, MIN_Amp, MAX_Amp);
   if (mAmp != ampInit)
      mRatio = pow(10.0, mAmp / 20.0);

   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mAmp * SCL_Amp + 0.5f));

   CheckClip();
}

void EffectAmplify::OnAmpSlider(wxCommandEvent & evt)
{
   double dB = evt.GetInt() / SCL_Amp;
   mRatio = pow(10.0, TrapDouble(dB, MIN_Amp, MAX_Amp) / 20.0);

   double dB2 = (evt.GetInt() - 1) / SCL_Amp;
   double ratio2 = pow(10.0, TrapDouble(dB2, MIN_Amp, MAX_Amp) / 20.0);

   if (!mClip->GetValue() && mRatio * mPeak > 1.0 && ratio2 * mPeak < 1.0)
   {
      mRatio = 1.0 / mPeak;
   }

   mAmp = 20.0 * log10(mRatio);
   mAmpT->GetValidator()->TransferToWindow();

   mNewPeak = 20.0 * log10(mRatio * mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();
}

void EffectAmplify::OnClipCheckBox(wxCommandEvent & WXUNUSED(evt))
{
   CheckClip();
}
