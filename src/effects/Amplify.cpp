/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectAmplify
\brief An Effect

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
//     Name    Type     Key                     Def         Min         Max      Scale
Param( Ratio,  float,   XO("Ratio"),            0.9f,       -FLT_MAX,   FLT_MAX, 1.0f  );
Param( NoClip, bool,    XO("Allow Clipping"),   false,      false,      true,    1.0f  );
Param( Amp,    float,   wxT(""),                -0.91515f,  -50.0,      50.0,    10.0f );

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
   mNoClip = DEF_NoClip;
   mRatio = pow(10.0, mAmp * 20.0);
   mPeak = 0.0;
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
   parms.Write(KEY_NoClip, !mNoClip);

   return true;
}

bool EffectAmplify::SetAutomationParameters(EffectAutomationParameters & parms)
{
   float Ratio;
   float def = pow(10.0, DEF_Amp / 20.0);
   float min = pow(10.0, MIN_Amp / 20.0);
   float max = pow(10.0, MAX_Amp / 20.0);

   if (!parms.ReadAndVerify(KEY_Ratio, &Ratio, def, min, max))
   {
      return false;
   }
   ReadAndVerifyBool(NoClip);

   mRatio = Ratio;
   mNoClip = !NoClip;

   return true;
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

void EffectAmplify::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartVerticalLay(0);
   {
      // Amplitude
      S.StartMultiColumn(2, wxCENTER);
      {
         FloatingPointValidator<float> vldAmp(1, &mAmp);
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

      // Peek
      S.StartMultiColumn(2, wxCENTER);
      {
         FloatingPointValidator<float> vldNewPeak(1, &mNewPeak);
         vldNewPeak.SetRange(20.0 * log10(pow(10.0, MIN_Amp / 20.0) * mPeak),
                             20.0 * log10(pow(10.0, MAX_Amp / 20.0) * mPeak));
         mNewPeakT = S.Id(ID_Peak).AddTextBox(_("New Peak Amplitude (dB):"), wxT(""), 12);
         mNewPeakT->SetValidator(vldNewPeak);
      }
      S.EndMultiColumn();

      // Clipping
      S.StartHorizontalLay(wxCENTER);
      {
         mClip = S.Id(ID_Clip).AddCheckBox(_("Allow clipping"), wxT("false"));
      }
      S.EndHorizontalLay();

   }
   S.EndVerticalLay();

   return;
}

bool EffectAmplify::TransferDataToWindow()
{
   if (mPeak > 0.0)
      mRatio = 1.0 / mPeak;
   else
      mRatio = 1.0;

   // limit range of gain
   double dB = TrapDouble(20.0 * log10(mRatio), MIN_Amp, MAX_Amp);

   mRatio = pow(10.0, dB / 20.0);

   mAmp = 20.0 * log10(mRatio);
   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mAmp * SCL_Amp + 0.5));

   mNewPeak = 20.0 * log10(mRatio * mPeak);
   mNewPeakT->GetValidator()->TransferToWindow();

   CheckClip();

   return true;
}

bool EffectAmplify::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   mRatio = pow(10.0, TrapDouble(mAmp, MIN_Amp, MAX_Amp) / 20.0);

   mNoClip = !mClip->GetValue();

   if (mNoClip && mRatio * mPeak > 1.0)
   {
      mRatio = 1.0 / mPeak;
   }

   return true;
}

// EffectAmplify implementation

void EffectAmplify::CheckClip()
{
   double peak = mRatio * mPeak;
   EnableApply(mClip->GetValue() || (peak > 0.0 && peak <= 1.0));
}

void EffectAmplify::OnAmpText(wxCommandEvent & WXUNUSED(evt))
{
   if (!mAmpT->GetValidator()->TransferFromWindow())
   {
      EnableApply(false);
      return;
   }

   mRatio = pow(10.0, TrapDouble(mAmp, MIN_Amp, MAX_Amp) / 20.0);

   mAmpS->SetValue((int) (mAmp * SCL_Amp + 0.5));

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

   double r = pow(10.0, mNewPeak / 20.0) / mPeak;

   mAmp = TrapDouble(20.0 * log10(r), MIN_Amp, MAX_Amp);
   mRatio = pow(10.0, mAmp / 20.0);

   mAmpT->GetValidator()->TransferToWindow();

   mAmpS->SetValue((int) (mAmp * SCL_Amp + 0.5));

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
