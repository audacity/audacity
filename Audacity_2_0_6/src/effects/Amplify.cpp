/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectAmplify
\brief An EffectSimpleMono

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

*//****************************************************************//**

\class AmplifyDialog
\brief Dilaog used with EffectAmplify.

*//*******************************************************************/

#include "../Audacity.h"

#include "Amplify.h"

#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include <math.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>

//
// EffectAmplify
//

EffectAmplify::EffectAmplify()
{
   ratio = float(1.0);
   peak = float(0.0);
}

wxString EffectAmplify::GetEffectDescription() {
   // Note: This is useful only after ratio has been set.
   return wxString::Format(_("Applied effect: %s %.1f dB"),
                           this->GetEffectName().c_str(), 20*log10(ratio));
}

bool EffectAmplify::Init()
{
   peak = float(0.0);

   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);

   for (Track *t = iter.First(); t; t = iter.Next()) {
      float min, max;
      ((WaveTrack *)t)->GetMinMax(&min, &max, mT0, mT1);
      float newpeak = (fabs(min) > fabs(max) ? fabs(min) : fabs(max));

      if (newpeak > peak) {
         peak = newpeak;
      }
   }

   return true;
}

bool EffectAmplify::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferFloat( wxT("Ratio"), ratio, 0.9f );
   return true;
}

bool EffectAmplify::PromptUser()
{
   AmplifyDialog dlog(this, mParent);
   dlog.peak = peak;
   if (peak > 0.0)
      dlog.ratio = 1.0 / peak;
   else
      dlog.ratio = 1.0;
   dlog.TransferDataToWindow();
   dlog.CenterOnParent();
   dlog.ShowModal();

  if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   ratio = dlog.ratio;
   if (dlog.noclip && ratio*peak > 1.0)
      ratio = 1.0 / peak;

   return true;
}

bool EffectAmplify::ProcessSimpleMono(float *buffer, sampleCount len)
{
   sampleCount i;
   for (i = 0; i < len; i++)
   {
      buffer[i] = (buffer[i] * ratio);
   }
   return true;
}

//----------------------------------------------------------------------------
// AmplifyDialog
//----------------------------------------------------------------------------

#define AMP_MIN -500
#define AMP_MAX 500

#define ID_AMP_TEXT 10001
#define ID_PEAK_TEXT 10002
#define ID_AMP_SLIDER 10003
#define ID_CLIP_CHECKBOX 10004

BEGIN_EVENT_TABLE(AmplifyDialog, EffectDialog)
   EVT_SLIDER(ID_AMP_SLIDER, AmplifyDialog::OnAmpSlider)
   EVT_TEXT(ID_AMP_TEXT, AmplifyDialog::OnAmpText)
   EVT_TEXT(ID_PEAK_TEXT, AmplifyDialog::OnPeakText)
   EVT_CHECKBOX(ID_CLIP_CHECKBOX, AmplifyDialog::OnClipCheckBox)
   EVT_BUTTON(ID_EFFECT_PREVIEW, AmplifyDialog::OnPreview)
END_EVENT_TABLE()

AmplifyDialog::AmplifyDialog(EffectAmplify *effect,
                             wxWindow * parent)
:  EffectDialog(parent, _("Amplify"), PROCESS_EFFECT),
   mEffect(effect)
{

   ratio = float(1.0);
   peak = float(0.0);

   Init();
}

void AmplifyDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator vld(wxFILTER_NUMERIC);

   S.AddSpace(0, 5);

   // Amplitude
   S.StartMultiColumn(2, wxCENTER);
   {
      mAmpT = S.Id(ID_AMP_TEXT).AddTextBox(_("Amplification (dB):"),
                                           wxT(""),
                                           12);
      mAmpT->SetValidator(vld);
   }
   S.EndMultiColumn();

   // Amplitude
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      mAmpS = S.Id(ID_AMP_SLIDER).AddSlider(wxT(""),
                                            0,
                                            AMP_MAX,
                                            AMP_MIN);
      mAmpS->SetName(_("Amplification dB"));
   }
   S.EndHorizontalLay();

   // Peek
   S.StartMultiColumn(2, wxCENTER);
   {
      mPeakT = S.Id(ID_PEAK_TEXT).AddTextBox(_("New Peak Amplitude (dB):"),
                                             wxT(""),
                                             12);
      mPeakT->SetValidator(vld);
   }
   S.EndMultiColumn();

   // Clipping
   S.StartHorizontalLay(wxCENTER);
   {
      mClip = S.Id(ID_CLIP_CHECKBOX).AddCheckBox(_("Allow clipping"),
                                                 wxT("false"));
   }
   S.EndHorizontalLay();

   return;
}

bool AmplifyDialog::TransferDataToWindow()
{
   // limit range of gain
   double dB = TrapDouble(200*log10(ratio), AMP_MIN, AMP_MAX)/10.0;
   ratio = pow(10.0, dB/20.0);

   mAmpS->SetValue((int)(200*log10(ratio)+0.5));

   mAmpT->ChangeValue(wxString::Format(wxT("%.1f"), 20*log10(ratio)));

   wxString str;
   if( ratio*peak > 0.0 )
      str.Printf(wxT("%.1f"), 20*log10(ratio*peak));
   else
      str = _("-Infinity");   // the case when the waveform is all zero
   mPeakT->ChangeValue(str);

   return true;
}

bool AmplifyDialog::TransferDataFromWindow()
{
   wxString val = mAmpT->GetValue();
   double r;

   val.ToDouble(&r);
   ratio = pow(10.0,TrapDouble(r*10, AMP_MIN, AMP_MAX)/200.0);

   noclip = !mClip->GetValue();

   return true;
}

bool AmplifyDialog::Validate()
{
   TransferDataFromWindow();

   if (mClip->GetValue() == false) {
     if (ratio * peak > 1.0)
        ratio = 1.0 / peak;
   }

   return true;
}
// handler implementations for AmplifyDialog

void AmplifyDialog::CheckClip()
{
   wxButton *ok = (wxButton *) FindWindow(wxID_OK);

   if (!mClip->GetValue() == true) {
      ok->Enable(ratio * peak <= 1.0);
   }
   else {
      ok->Enable(true);
   }
}

void AmplifyDialog::OnAmpText(wxCommandEvent & WXUNUSED(event))
{
   wxString val = mAmpT->GetValue();
   double r;

   val.ToDouble(&r);
   ratio = pow(10.0,TrapDouble(r*10, AMP_MIN, AMP_MAX)/200.0);

   mAmpS->SetValue((int)(200*log10(ratio)+0.5));

   if( ratio*peak > 0.0 )
      val.Printf(wxT("%.1f"), 20*log10(ratio*peak));
   else
      val = _("-Infinity");   // the case when the waveform is all zero
   mPeakT->ChangeValue(val);

   CheckClip();
}

void AmplifyDialog::OnPeakText(wxCommandEvent & WXUNUSED(event))
{
   wxString val = mPeakT->GetValue();
   double r;

   val.ToDouble(&r);
   ratio = pow(10.0, r/20.0) / peak;

   double dB = TrapDouble(200*log10(ratio), AMP_MIN, AMP_MAX)/10.0;
   ratio = pow(10.0, dB/20.0);

   mAmpS->SetValue((int)(10*dB+0.5));

   val.Printf(wxT("%.1f"), dB);
   mAmpT->ChangeValue(val);

   CheckClip();
}

void AmplifyDialog::OnAmpSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;

   double dB = mAmpS->GetValue() / 10.0;
   ratio = pow(10.0,TrapDouble(dB, AMP_MIN, AMP_MAX)/20.0);

   double dB2 = (mAmpS->GetValue()-1) / 10.0;
   double ratio2 = pow(10.0,TrapDouble(dB2, AMP_MIN, AMP_MAX)/20.0);

   if (!mClip->GetValue() && ratio * peak > 1.0 && ratio2 * peak < 1.0)
      ratio = 1.0 / peak;

   str.Printf(wxT("%.1f"), 20*log10(ratio));
   mAmpT->ChangeValue(str);

   if (ratio*peak > 0.0)
      str.Printf(wxT("%.1f"), 20*log10(ratio*peak));
   else
      str = _("-Infinity");   // the case when the waveform is all zero
   mPeakT->ChangeValue(str);

   CheckClip();
}

void AmplifyDialog::OnClipCheckBox(wxCommandEvent & WXUNUSED(event))
{
   CheckClip();
}

void AmplifyDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   float oldRatio = mEffect->ratio;
   float oldPeak = mEffect->peak;

   mEffect->ratio = ratio;
   if (noclip && ratio*peak > 1.0)
      mEffect->ratio = 1.0 / peak;
   mEffect->peak = peak;

   mEffect->Preview();

   mEffect->ratio = oldRatio;
   mEffect->peak = oldPeak;
}
