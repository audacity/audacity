/**********************************************************************

  Audacity: A Digital Audio Editor

  BassBoost.cpp

  Effect programming:
  Nasca Octavian Paul

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectBassBoost
\brief An EffectSimpleMono

*//****************************************************************//**

\class BassBoostDialog
\brief Dialog for EffectBassBoost

*//*******************************************************************/

#include "../Audacity.h"

#include "BassBoost.h"
#include "../WaveTrack.h"

#include <wx/button.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>

#include <math.h>

//
// EffectBassBoost
//

EffectBassBoost::EffectBassBoost()
{
   frequency = 200;
   dB_boost = 12;
}

wxString EffectBassBoost::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   return wxString::Format(_("Applied effect: %s frequency = %.0f Hz, boost = %.0f dB"), 
                           this->GetEffectName().c_str(), frequency, dB_boost); 
} 

bool EffectBassBoost::NewTrackSimpleMono()
{
//(re)initialise filter parameters
   xn1=0;
   xn2=0;
   yn1=0;
   yn2=0;

   /* Compute coefficents of the biquand IIR filter */
   omega = 2 * 3.141592653589 * frequency / mCurRate;
   sn = sin(omega);
   cs = cos(omega);
   a = exp(log(10.0) * dB_boost / 40);
   shape = float(1.0);           /*Low Shelf filter's shape, if this is too large
                            or too small it will result an unstable filter */
   beta = sqrt((a * a + 1) / shape - (pow((a - 1), 2)));
   /*  Coefficients  */
   b0 = a * ((a + 1) - (a - 1) * cs + beta * sn);
   b1 = 2 * a * ((a - 1) - (a + 1) * cs);
   b2 = a * ((a + 1) - (a - 1) * cs - beta * sn);
   a0 = ((a + 1) + (a - 1) * cs + beta * sn);
   a1 = -2 * ((a - 1) + (a + 1) * cs);
   a2 = (a + 1) + (a - 1) * cs - beta * sn;

   return true;
}

bool EffectBassBoost::PromptUser()
{
   BassBoostDialog dlog(this, mParent);
   dlog.freq = frequency;
   dlog.boost = dB_boost;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   frequency = dlog.freq;
   dB_boost = dlog.boost;

   return true;
}

bool EffectBassBoost::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferFloat(wxT("Boost"),dB_boost,0.0);
   shuttle.TransferFloat(wxT("Freq"),frequency,0.0);
   return true;
}

bool EffectBassBoost::ProcessSimpleMono(float *buffer, sampleCount len)
{
   /* initialise the filter */

   float out, in = 0;

   for (int i = 0; i < len; i++) {
      in = buffer[i];
      out = (b0 * in + b1 * xn1 + b2 * xn2 - a1 * yn1 - a2 * yn2) / a0;
      xn2 = xn1;
      xn1 = in;
      yn2 = yn1;
      yn1 = out;

      if (out < -1.0)
         out = float(-1.0);
      else if (out > 1.0)
         out = float(1.0);        //Prevents clipping

      buffer[i] = out;
   }

   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// BassBoostDialog
//----------------------------------------------------------------------------

const static wxChar *numbers[] =
{
   wxT("0"), wxT("1"), wxT("2"), wxT("3"), wxT("4"),
   wxT("5"), wxT("6"), wxT("7"), wxT("8"), wxT("9")
};

// Declare window functions                                                                                                         
                                                                                                                                    
#define ID_FREQ_TEXT    10001
#define ID_FREQ_SLIDER  10002
#define ID_BOOST_TEXT   10003
#define ID_BOOST_SLIDER 10004

// Declare ranges

#define FREQ_MIN 1
#define FREQ_MAX 1000
#define BOOST_MIN 0
#define BOOST_MAX 36

BEGIN_EVENT_TABLE(BassBoostDialog, EffectDialog)
   EVT_SLIDER(ID_FREQ_SLIDER, BassBoostDialog::OnFreqSlider)
   EVT_SLIDER(ID_BOOST_SLIDER, BassBoostDialog::OnBoostSlider)
   EVT_TEXT(ID_FREQ_TEXT, BassBoostDialog::OnFreqText)
   EVT_TEXT(ID_BOOST_TEXT, BassBoostDialog::OnBoostText)
   EVT_BUTTON(ID_EFFECT_PREVIEW, BassBoostDialog::OnPreview)
END_EVENT_TABLE()

BassBoostDialog::BassBoostDialog(EffectBassBoost *effect,
                                 wxWindow * parent):
   EffectDialog(parent, _("Bass Boost"), PROCESS_EFFECT),
   mEffect(effect)
{
   Init();
}

void BassBoostDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator vld(wxFILTER_INCLUDE_CHAR_LIST);
   vld.SetIncludes(wxArrayString(10, numbers));

   S.StartHorizontalLay(wxCENTER, false);
   {
      /* i18n-hint: Nasca Octavian Paul is a person's name.*/
      S.AddTitle(_("by Nasca Octavian Paul"));
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
      // Frequency
      mFreqT = S.Id(ID_FREQ_TEXT).AddTextBox(_("Frequency (Hz):"),
                                             wxT(""),
                                             10);
      mFreqT->SetValidator(vld);

      S.SetStyle(wxSL_HORIZONTAL);
      mFreqS = S.Id(ID_FREQ_SLIDER).AddSlider(wxT(""),
                                              0,
                                              FREQ_MAX);
      mFreqS->SetName(_("Frequency Hertz"));
      mFreqS->SetRange(FREQ_MIN, FREQ_MAX);

      // Boost
      mBoostT = S.Id(ID_BOOST_TEXT).AddTextBox(_("Boost (dB):"),
                                               wxT(""),
                                               10);
      mBoostT->SetValidator(vld);

      S.SetStyle(wxSL_HORIZONTAL);
      mBoostS = S.Id(ID_BOOST_SLIDER).AddSlider(wxT(""),
                                                0,
                                                BOOST_MAX);
      mBoostS->SetName(_("Boost dB"));
      mBoostS->SetRange(BOOST_MIN, BOOST_MAX);
   }
   S.EndMultiColumn();
   return;
}

bool BassBoostDialog::TransferDataToWindow()
{
   mBoostS->SetValue((int)boost);
   mFreqS->SetValue((int)freq);

   mBoostT->SetValue(wxString::Format(wxT("%d"), (int) boost));
   mFreqT->SetValue(wxString::Format(wxT("%d"), (int) freq));

   return true;
}

bool BassBoostDialog::TransferDataFromWindow()
{
   boost = TrapLong(mBoostS->GetValue(), BOOST_MIN, BOOST_MAX);
   freq = TrapLong(mFreqS->GetValue(), FREQ_MIN, FREQ_MAX);

   return true;
}

// WDR: handler implementations for BassBoostDialog

void BassBoostDialog::OnBoostText(wxCommandEvent & event)
{
   long val;

   mBoostT->GetValue().ToLong(&val);
   mBoostS->SetValue(TrapLong(val, BOOST_MIN, BOOST_MAX));
}

void BassBoostDialog::OnFreqText(wxCommandEvent & event)
{
   long val;

   mFreqT->GetValue().ToLong(&val);
   mFreqS->SetValue(TrapLong(val, FREQ_MIN, FREQ_MAX));
}

void BassBoostDialog::OnBoostSlider(wxCommandEvent & event)
{
   mBoostT->SetValue(wxString::Format(wxT("%d"), mBoostS->GetValue()));
}

void BassBoostDialog::OnFreqSlider(wxCommandEvent & event)
{
   mFreqT->SetValue(wxString::Format(wxT("%d"), mFreqS->GetValue()));
}

void BassBoostDialog::OnPreview(wxCommandEvent & event)
{
   TransferDataFromWindow();
   mEffect->frequency = freq;
   mEffect->dB_boost = boost;
   mEffect->Preview();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 25dfd7cb-9e1b-4c8d-a188-ed406c2b51b7

