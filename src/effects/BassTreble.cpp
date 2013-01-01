/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   BassTreble.cpp
   Steve Daulton

******************************************************************//**

\class EffectBassTreble
\brief An EffectSimpleMono, high shelf and low shelf filters,
derived from BassBoost by Nasca Octavian Paul

*//****************************************************************//**

\class BassTrebleDialog
\brief Dialog for EffectBassTreble

*//*******************************************************************/

#include "../Audacity.h"

#include "BassTreble.h"
#include "../WaveTrack.h"

#include <wx/button.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>

#include <math.h>

//
// EffectBassTreble
//

EffectBassTreble::EffectBassTreble()
{
   dB_bass = 0;
   dB_treble = 0;
   dB_gain = 0;
}

wxString EffectBassTreble::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   return wxString::Format(_("Applied effect: %s bass = %.0f dB, treble = %.0f dB, gain = %.0f dB"), 
                           this->GetEffectName().c_str(), dB_bass, dB_treble, dB_gain); 
} 

bool EffectBassTreble::NewTrackSimpleMono()
{
   const float slope = 0.4;   // same slope for both filters
   //(re)initialise filter parameters for low shelf
   xn1Bass=0;
   xn2Bass=0;
   yn1Bass=0;
   yn2Bass=0;
   xn1Treble=0;
   xn2Treble=0;
   yn1Treble=0;
   yn2Treble=0;

   // Compute coefficents of the low shelf biquand IIR filter
   wBass = 2 * M_PI * 250 / mCurRate;   // half gain frequency 250 Hz
   swBass = sin(wBass);
   cwBass = cos(wBass);
   aBass = exp(log(10.0) * dB_bass / 40);
   bBass = sqrt((aBass * aBass + 1) / slope - (pow((aBass - 1), 2)));
   //  Coefficients  for low shelf
   b0Bass = aBass * ((aBass + 1) - (aBass - 1) * cwBass + bBass * swBass);
   b1Bass = 2 * aBass * ((aBass - 1) - (aBass + 1) * cwBass);
   b2Bass = aBass * ((aBass + 1) - (aBass - 1) * cwBass - bBass * swBass);
   a0Bass = ((aBass + 1) + (aBass - 1) * cwBass + bBass * swBass);
   a1Bass = -2 * ((aBass - 1) + (aBass + 1) * cwBass);
   a2Bass = (aBass + 1) + (aBass - 1) * cwBass - bBass * swBass;

   // Compute coefficents of the high shelf biquand IIR filter
   wTreble = 2 * M_PI * 4000 / mCurRate;   // half gain frequency 4000 Hz
   swTreble = sin(wTreble);
   cwTreble = cos(wTreble);
   aTreble = exp(log(10.0) * dB_treble / 40);
   bTreble = sqrt((aTreble * aTreble + 1) / slope - (pow((aTreble - 1), 2)));
   //  Coefficients for high shelf
   b0Treble = aTreble * ((aTreble + 1) + (aTreble - 1) * cwTreble + bTreble * swTreble);
   b1Treble = -2 * aTreble * ((aTreble - 1) + (aTreble + 1) * cwTreble);
   b2Treble = aTreble * ((aTreble + 1) + (aTreble - 1) * cwTreble - bTreble * swTreble);
   a0Treble = ((aTreble + 1) - (aTreble - 1) * cwTreble + bTreble * swTreble);   
   a1Treble = 2 * ((aTreble - 1) - (aTreble + 1) * cwTreble);
   a2Treble = (aTreble + 1) - (aTreble - 1) * cwTreble - bTreble * swTreble;

   return true;
}

bool EffectBassTreble::PromptUser()
{
   BassTrebleDialog dlog(this, mParent);
   dlog.bass = dB_bass;
   dlog.treble = dB_treble;
   dlog.gain = dB_gain;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   dB_bass = dlog.bass;
   dB_treble = dlog.treble;
   dB_gain = dlog.gain;

   return true;
}

bool EffectBassTreble::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferFloat(wxT("Bass"),dB_bass,0.0);
   shuttle.TransferFloat(wxT("Treble"),dB_treble,0.0);
   shuttle.TransferFloat(wxT("Gain"),dB_gain,0.0);
   return true;
}

bool EffectBassTreble::ProcessSimpleMono(float *buffer, sampleCount len)
{
   /* initialise the filter */

   float out, in = 0;

   for (sampleCount i = 0; i < len; i++) {
      in = buffer[i];
      // Bass filter
      out = (b0Bass * in + b1Bass * xn1Bass + b2Bass * xn2Bass -
            a1Bass * yn1Bass - a2Bass * yn2Bass) / a0Bass;
      xn2Bass = xn1Bass;
      xn1Bass = in;
      yn2Bass = yn1Bass;
      yn1Bass = out;
      // Treble filter
      in = out;
      out = (b0Treble * in + b1Treble * xn1Treble + b2Treble * xn2Treble -
            a1Treble * yn1Treble - a2Treble * yn2Treble) / a0Treble;
      xn2Treble = xn1Treble;
      xn1Treble = in;
      yn2Treble = yn1Treble;
      yn1Treble = out;
      // Gain control
      buffer[i] = pow(10.0, dB_gain / 20.0) * out;
   }

   return true;
}

//----------------------------------------------------------------------------
// BassTrebleDialog
//----------------------------------------------------------------------------

// Declare window functions

#define ID_BASS_TEXT 10001
#define ID_BASS_SLIDER 10002
#define ID_TREBLE_TEXT 10003
#define ID_TREBLE_SLIDER 10004
#define ID_GAIN_TEXT 10005
#define ID_GAIN_SLIDER 10006

// Declare ranges

#define BASS_MIN -15
#define BASS_MAX 15
#define TREBLE_MIN -15
#define TREBLE_MAX 15
#define GAIN_MIN -15
#define GAIN_MAX 15

BEGIN_EVENT_TABLE(BassTrebleDialog, EffectDialog)
   EVT_SLIDER(ID_BASS_SLIDER, BassTrebleDialog::OnBassSlider)
   EVT_SLIDER(ID_TREBLE_SLIDER, BassTrebleDialog::OnTrebleSlider)
   EVT_SLIDER(ID_GAIN_SLIDER, BassTrebleDialog::OnGainSlider)
   EVT_TEXT(ID_BASS_TEXT, BassTrebleDialog::OnBassText)
   EVT_TEXT(ID_TREBLE_TEXT, BassTrebleDialog::OnTrebleText)
   EVT_TEXT(ID_GAIN_TEXT, BassTrebleDialog::OnGainText)
   EVT_BUTTON(ID_EFFECT_PREVIEW, BassTrebleDialog::OnPreview)
END_EVENT_TABLE()

BassTrebleDialog::BassTrebleDialog(EffectBassTreble *effect,
                                 wxWindow * parent):
   EffectDialog(parent, _("Bass and Treble"), PROCESS_EFFECT),
   mEffect(effect)
{
   Init();
}

void BassTrebleDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator vld(wxFILTER_NUMERIC);
   
   S.StartHorizontalLay(wxCENTER, false);
   {
      /* i18n-hint: Steve Daulton is a person's name.*/
      S.AddTitle(_("by Steve Daulton"));
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxCENTER, false);
   {
      // Add a little space
   }
   S.EndHorizontalLay();

   S.StartMultiColumn(3, wxEXPAND);
   S.SetStretchyCol(2);
   {
      // Bass control
      mBassT = S.Id(ID_BASS_TEXT).AddTextBox(_("Bass (dB):"), wxT(""), 10);
      mBassT->SetValidator(vld);

      S.SetStyle(wxSL_HORIZONTAL);
      mBassS = S.Id(ID_BASS_SLIDER).AddSlider(wxT(""), 0, BASS_MAX);
      mBassS->SetName(_("Bass (dB)"));
      mBassS->SetRange(BASS_MIN, BASS_MAX);
      
      // Treble control
      mTrebleT = S.Id(ID_TREBLE_TEXT).AddTextBox(_("Treble (dB):"), wxT(""), 10);
      mTrebleT->SetValidator(vld);

      S.SetStyle(wxSL_HORIZONTAL);
      mTrebleS = S.Id(ID_TREBLE_SLIDER).AddSlider(wxT(""), 0, TREBLE_MAX);
      mTrebleS->SetName(_("Treble (dB)"));
      mTrebleS->SetRange(TREBLE_MIN, TREBLE_MAX);
     
      // Gain control
      mGainT = S.Id(ID_GAIN_TEXT).AddTextBox(_("Gain (dB):"), wxT(""), 10);
      mGainT->SetValidator(vld);

      S.SetStyle(wxSL_HORIZONTAL);
      mGainS = S.Id(ID_GAIN_SLIDER).AddSlider(wxT(""), 0, GAIN_MAX);
      mGainS->SetName(_("Gain (dB)"));
      mGainS->SetRange(GAIN_MIN, GAIN_MAX);
   }
   S.EndMultiColumn();
   return;
}

bool BassTrebleDialog::TransferDataToWindow()
{
   mBassS->SetValue((int)bass);
   mTrebleS->SetValue((int)treble);
   mGainS->SetValue((int)gain);

   mBassT->SetValue(wxString::Format(wxT("%d"), (int) bass));
   mTrebleT->SetValue(wxString::Format(wxT("%d"), (int) treble));
   mGainT->SetValue(wxString::Format(wxT("%d"), (int) gain));

   return true;
}

bool BassTrebleDialog::TransferDataFromWindow()
{
   bass = TrapLong(mBassS->GetValue(), BASS_MIN, BASS_MAX);
   treble = TrapLong(mTrebleS->GetValue(), TREBLE_MIN, TREBLE_MAX);
   gain = TrapLong(mGainS->GetValue(), GAIN_MIN, GAIN_MAX);

   return true;
}

// handler implementations for BassTrebleDialog

void BassTrebleDialog::OnBassText(wxCommandEvent & WXUNUSED(event))
{
   long val;

   mBassT->GetValue().ToLong(&val);
   mBassS->SetValue(TrapLong(val, BASS_MIN, BASS_MAX));
}

void BassTrebleDialog::OnTrebleText(wxCommandEvent & WXUNUSED(event))
{
   long val;

   mTrebleT->GetValue().ToLong(&val);
   mTrebleS->SetValue(TrapLong(val, TREBLE_MIN, TREBLE_MAX));
}

void BassTrebleDialog::OnGainText(wxCommandEvent & WXUNUSED(event))
{
   long val;

   mGainT->GetValue().ToLong(&val);
   mGainS->SetValue(TrapLong(val, GAIN_MIN, GAIN_MAX));
}

void BassTrebleDialog::OnBassSlider(wxCommandEvent & WXUNUSED(event))
{
   mBassT->SetValue(wxString::Format(wxT("%d"), mBassS->GetValue()));
}

void BassTrebleDialog::OnTrebleSlider(wxCommandEvent & WXUNUSED(event))
{
   mTrebleT->SetValue(wxString::Format(wxT("%d"), mTrebleS->GetValue()));
}

void BassTrebleDialog::OnGainSlider(wxCommandEvent & WXUNUSED(event))
{
   mGainT->SetValue(wxString::Format(wxT("%d"), mGainS->GetValue()));
}

void BassTrebleDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();
   mEffect->dB_bass = bass;
   mEffect->dB_treble = treble;
   mEffect->dB_gain = gain;
   mEffect->Preview();
}
