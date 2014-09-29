/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   BassTreble.cpp
   Steve Daulton

******************************************************************//**

\class EffectBassTreble
\brief A TwoPassSimpleMono, high shelf and low shelf filters.

   The first pass applies the equalization and calculates the
   peak value. The second pass, if enabled, normalizes to the
   level set by the level control.

*//****************************************************************//**

\class BassTrebleDialog
\brief Dialog for EffectBassTreble

*//*******************************************************************/

#include "../Audacity.h"
#include <math.h>

#include "BassTreble.h"
#include "../WaveTrack.h"
#include "../Prefs.h"

#include <wx/button.h>
#include <wx/valtext.h>
#include <wx/checkbox.h>
#include <wx/slider.h>
#include <wx/sizer.h>

// Used to communicate the type of the filter.
static const int bassType = 0; //Low Shelf
static const int trebleType = 1;  // High Shelf


EffectBassTreble::EffectBassTreble()
{
}

bool EffectBassTreble::Init()
{
   // restore saved preferences
   int readBool;
   gPrefs->Read(wxT("/Effects/BassTreble/Bass"), &dB_bass, 0.0);
   gPrefs->Read(wxT("/Effects/BassTreble/Treble"), &dB_treble, 0.0);
   gPrefs->Read(wxT("/Effects/BassTreble/Level"), &dB_level, -1.0);
   gPrefs->Read(wxT("/Effects/BassTreble/Normalize"), &readBool, 1 );

   // Validate data
   dB_level = (dB_level > 0)? 0 : dB_level;
   mbNormalize = (readBool != 0);

   return true;
}

wxString EffectBassTreble::GetEffectDescription() {
   // Note: This is useful only after values have been set.
   wxString strResult =
      wxString::Format(_("Applied effect: %s bass = %.1f dB, treble = %.1f dB"),
                           this->GetEffectName().c_str(),
                           dB_bass, dB_treble);
   (mbNormalize) ?
      strResult += wxString::Format(_(", level enabled at = %.1f dB"), dB_level) :
      strResult += wxString::Format(_(", level disabled"));

   return strResult;
}

bool EffectBassTreble::PromptUser()
{
   BassTrebleDialog dlog(this, mParent);
   dlog.bass = dB_bass;
   dlog.treble = dB_treble;
   dlog.level = dB_level;
   dlog.mbNormalize = mbNormalize;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   dB_bass = dlog.bass;
   dB_treble = dlog.treble;
   dB_level = dlog.level;
   mbNormalize = dlog.mbNormalize;

   gPrefs->Write(wxT("/Effects/BassTreble/Bass"), dB_bass);
   gPrefs->Write(wxT("/Effects/BassTreble/Treble"), dB_treble);
   gPrefs->Write(wxT("/Effects/BassTreble/Level"), dB_level);
   gPrefs->Write(wxT("/Effects/BassTreble/Normalize"), mbNormalize);
   gPrefs->Flush();

   return true;
}

bool EffectBassTreble::TransferParameters(Shuttle & shuttle)
{
   shuttle.TransferDouble(wxT("Bass"),dB_bass,0.0);
   shuttle.TransferDouble(wxT("Treble"),dB_treble,0.0);
   shuttle.TransferDouble(wxT("Level"),dB_level,0.0);
   shuttle.TransferBool( wxT("Normalize"), mbNormalize, true );

   return true;
}

bool EffectBassTreble::InitPass1()
{
   mMax=0.0;

   // Integer format tracks require headroom to avoid clipping
   // when saved between passes (bug 619)

   if (mbNormalize)  // don't need to calculate this if only doing one pass.
   {
      // Up to (gain + 6dB) headroom required for treble boost (experimental).
      mPreGain = (dB_treble > 0)? (dB_treble + 6.0) : 0.0;
      if (dB_bass >= 0)
      {
         mPreGain = (mPreGain > dB_bass)? mPreGain : dB_bass;
      } else {
         // Up to 6 dB headroom reaquired for bass cut (experimental)
         mPreGain = (mPreGain > 6.0)? mPreGain : 6.0;
      }
      mPreGain = (exp (log(10.0) * mPreGain / 20));   // to linear
   } else {
      mPreGain = 1.0;   // Unity gain
   }

   if (!mbNormalize)
       DisableSecondPass();

   return true;
}

bool EffectBassTreble::NewTrackPass1()
{
   const float slope = 0.4f;   // same slope for both filters
   const double hzBass = 250.0f;
   const double hzTreble = 4000.0f;

   //(re)initialise filter parameters
   xn1Bass=xn2Bass=yn1Bass=yn2Bass=0;
   xn1Treble=xn2Treble=yn1Treble=yn2Treble=0;

   // Compute coefficents of the low shelf biquand IIR filter
   Coefficents(hzBass, slope, dB_bass, bassType,
               a0Bass, a1Bass, a2Bass,
               b0Bass, b1Bass, b2Bass);

   // Compute coefficents of the high shelf biquand IIR filter
   Coefficents(hzTreble, slope, dB_treble, trebleType,
               a0Treble, a1Treble, a2Treble,
               b0Treble, b1Treble, b2Treble);

   return true;
}

void EffectBassTreble::Coefficents(double hz, float slope, double gain, int type,
                                   float& a0, float& a1, float& a2,
                                   float& b0, float& b1, float& b2)
{
   double w = 2 * M_PI * hz / mCurRate;
   double a = exp(log(10.0) * gain / 40);
   double b = sqrt((a * a + 1) / slope - (pow((a - 1), 2)));

   if (type == bassType)
   {
      b0 = a * ((a + 1) - (a - 1) * cos(w) + b * sin(w));
      b1 = 2 * a * ((a - 1) - (a + 1) * cos(w));
      b2 = a * ((a + 1) - (a - 1) * cos(w) - b * sin(w));
      a0 = ((a + 1) + (a - 1) * cos(w) + b * sin(w));
      a1 = -2 * ((a - 1) + (a + 1) * cos(w));
      a2 = (a + 1) + (a - 1) * cos(w) - b * sin(w);
   }
   else //assumed trebleType
   {
      b0 = a * ((a + 1) + (a - 1) * cos(w) + b * sin(w));
      b1 = -2 * a * ((a - 1) + (a + 1) * cos(w));
      b2 = a * ((a + 1) + (a - 1) * cos(w) - b * sin(w));
      a0 = ((a + 1) - (a - 1) * cos(w) + b * sin(w));
      a1 = 2 * ((a - 1) - (a + 1) * cos(w));
      a2 = (a + 1) - (a - 1) * cos(w) - b * sin(w);
   }
}

bool EffectBassTreble::InitPass2()
{
    return mbNormalize;
}

// Process the input
bool EffectBassTreble::ProcessPass1(float *buffer, sampleCount len)
{
   for (sampleCount i = 0; i < len; i++)
      buffer[i] = (DoFilter(buffer[i]) / mPreGain);

   return true;
}

bool EffectBassTreble::ProcessPass2(float *buffer, sampleCount len)
{
   if (mMax != 0)
   {
      float gain = (pow(10.0, dB_level/20.0f))/mMax;
      for (int i = 0; i < len; i++)
         // Normalize to specified level
         buffer[i] *= (mPreGain * gain);
   }
   return true;
}

float EffectBassTreble::DoFilter(float in)
{
   // Bass filter
   float out = (b0Bass * in + b1Bass * xn1Bass + b2Bass * xn2Bass -
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

   // Retain the maximum value for use in the normalization pass
   if(mMax < fabs(out))
      mMax = fabs(out);
   return out;
}

//----------------------------------------------------------------------------
// BassTrebleDialog
//----------------------------------------------------------------------------

// Declare window functions

#define ID_BASS_TEXT 10001
#define ID_BASS_SLIDER 10002
#define ID_TREBLE_TEXT 10003
#define ID_TREBLE_SLIDER 10004
#define ID_LEVEL_TEXT 10005
#define ID_LEVEL_SLIDER 10006
#define ID_NORMALIZE 10007

// Declare ranges
// Sliders are integer, so range is x 10
// to allow 1 decimal place resolution

#define BASS_MIN -150      // Corresponds to -15 db
#define BASS_MAX 150       // Corresponds to +15 dB
#define TREBLE_MIN -150    // Corresponds to -15 dB
#define TREBLE_MAX 150     // Corresponds to +15 dB
#define LEVEL_MIN -300     // Corresponds to -30 dN
#define LEVEL_MAX 0        // Corresponds to 0 dB

BEGIN_EVENT_TABLE(BassTrebleDialog, EffectDialog)
   EVT_SLIDER(ID_BASS_SLIDER, BassTrebleDialog::OnBassSlider)
   EVT_SLIDER(ID_TREBLE_SLIDER, BassTrebleDialog::OnTrebleSlider)
   EVT_SLIDER(ID_LEVEL_SLIDER, BassTrebleDialog::OnLevelSlider)
   EVT_TEXT(ID_BASS_TEXT, BassTrebleDialog::OnBassText)
   EVT_TEXT(ID_TREBLE_TEXT, BassTrebleDialog::OnTrebleText)
   EVT_TEXT(ID_LEVEL_TEXT, BassTrebleDialog::OnLevelText)
   EVT_CHECKBOX(ID_NORMALIZE, BassTrebleDialog::OnNormalize)
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
   S.StartStatic(wxT(""));
   {
      S.StartMultiColumn(3, wxEXPAND);
      S.SetStretchyCol(2);
      {
         #ifdef __WXGTK__
         // BoxSizer is to make first mnemonic work, on Linux.
         wxPanel* cPanel = new wxPanel( this, wxID_ANY );
         wxBoxSizer* cSizer = new wxBoxSizer(wxVERTICAL);
         cPanel->SetSizer(cSizer);
         #endif

         wxTextValidator vld(wxFILTER_NUMERIC);

         // Bass control
         mBassT = S.Id(ID_BASS_TEXT).AddTextBox(_("&Bass (dB):"), wxT(""), 10);
         mBassT->SetName(_("Bass (dB):"));
         mBassT->SetValidator(vld);

         S.SetStyle(wxSL_HORIZONTAL);
         mBassS = S.Id(ID_BASS_SLIDER).AddSlider(wxT(""), 0, BASS_MAX, BASS_MIN);
         mBassS->SetName(_("Bass"));
         mBassS->SetRange(BASS_MIN, BASS_MAX);
         mBassS->SetPageSize(30);

         // Treble control
         mTrebleT = S.Id(ID_TREBLE_TEXT).AddTextBox(_("&Treble (dB):"), wxT(""), 10);
         mTrebleT->SetValidator(vld);

         S.SetStyle(wxSL_HORIZONTAL);
         mTrebleS = S.Id(ID_TREBLE_SLIDER).AddSlider(wxT(""), 0, TREBLE_MAX, TREBLE_MIN);
         mTrebleS->SetName(_("Treble"));
         mTrebleS->SetRange(TREBLE_MIN, TREBLE_MAX);
         mTrebleS->SetPageSize(30);

         // Level control
         mLevelT = S.Id(ID_LEVEL_TEXT).AddTextBox(_("&Level (dB):"), wxT(""), 10);
         mLevelT->SetValidator(vld);

         S.SetStyle(wxSL_HORIZONTAL);
         mLevelS = S.Id(ID_LEVEL_SLIDER).AddSlider(wxT(""), 0, LEVEL_MAX, LEVEL_MIN);
         mLevelS->SetName(_("Level"));
         mLevelS->SetRange(LEVEL_MIN, LEVEL_MAX);
         mLevelS->SetPageSize(30);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   // Normalize checkbox
   S.StartHorizontalLay(wxLEFT, true);
   {
      mNormalizeCheckBox = S.Id(ID_NORMALIZE).AddCheckBox(_("&Enable level control"),
                                    mbNormalize ? wxT("true") : wxT("false"));
      mWarning = S.AddVariableText( wxT(""), false,
                                    wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
   }
   S.EndHorizontalLay();

}

bool BassTrebleDialog::TransferDataToWindow()
{
   mBassS->SetValue((double)bass);
   mTrebleS->SetValue((double)treble);
   mLevelS->SetValue((double)level);

   mBassT->SetValue(wxString::Format(wxT("%.1f"), (float)bass));
   mTrebleT->SetValue(wxString::Format(wxT("%.1f"), (float)treble));
   mLevelT->SetValue(wxString::Format(wxT("%.1f"), (float)level));

   mNormalizeCheckBox->SetValue(mbNormalize);
   UpdateUI();
   TransferDataFromWindow();

   return true;
}

bool BassTrebleDialog::TransferDataFromWindow()
{
   mBassT->GetValue().ToDouble(&bass);
   mTrebleT->GetValue().ToDouble(&treble);
   mLevelT->GetValue().ToDouble(&level);
   mbNormalize = mNormalizeCheckBox->GetValue();

   return true;
}

void BassTrebleDialog::OnNormalize(wxCommandEvent& WXUNUSED(evt))
{
   UpdateUI();
}

void BassTrebleDialog::UpdateUI()
{
   bool enable = mNormalizeCheckBox->GetValue();
   double v0, v1, v2;
   wxString val0 = mBassT->GetValue();
   val0.ToDouble(&v0);
   wxString val1 = mTrebleT->GetValue();
   val1.ToDouble(&v1);
   wxString val2 = mLevelT->GetValue();
   val2.ToDouble(&v2);

   // Disallow level control if disabled
   mLevelT->Enable(enable);
   mLevelS->Enable(enable);

   wxButton *ok = (wxButton *) FindWindow(wxID_OK);
   wxButton *preview = (wxButton *) FindWindow(ID_EFFECT_PREVIEW);

   if (v0==0 && v1==0 && !enable)
   {
      // Disallow OK if nothing to do (but  allow preview)
      ok->Enable(false);
      preview->Enable(true);
      mWarning->SetLabel(_("    No change to apply."));
   }
   else
   {
      if ((v2 > 0) && enable)
      {
         // Disallow OK and Preview if level enabled and > 0
         ok->Enable(false);
         preview->Enable(false);
         mWarning->SetLabel(_(":   Maximum 0 dB."));
      }
      else
      {
         // OK and Preview enabled
         ok->Enable(true);
         preview->Enable(true);
         mWarning->SetLabel(wxT(""));
      }
   }
}

// handler implementations for BassTrebleDialog
void BassTrebleDialog::OnBassText(wxCommandEvent & WXUNUSED(event))
{
   double val;
   mBassT->GetValue().ToDouble(&val);
   int newval = floor(val / 0.1 + 0.5);
   mBassS->SetValue(TrapDouble(newval, BASS_MIN, BASS_MAX));
   UpdateUI();
}

void BassTrebleDialog::OnTrebleText(wxCommandEvent & WXUNUSED(event))
{
   double val;
   mTrebleT->GetValue().ToDouble(&val);
   int newval = floor(val / 0.1 + 0.5);
   mTrebleS->SetValue(TrapDouble(newval, TREBLE_MIN, TREBLE_MAX));
   UpdateUI();
}

void BassTrebleDialog::OnLevelText(wxCommandEvent & WXUNUSED(event))
{
   double val;
   mLevelT->GetValue().ToDouble(&val);
   int newval = floor(val / 0.1 + 0.5);
   mLevelS->SetValue(TrapDouble(newval, LEVEL_MIN, LEVEL_MAX));
   UpdateUI();
}

void BassTrebleDialog::OnBassSlider(wxCommandEvent & WXUNUSED(event))
{
   mBassT->SetValue(wxString::Format(wxT("%.1f"), mBassS->GetValue() * 0.1));
   UpdateUI();
}

void BassTrebleDialog::OnTrebleSlider(wxCommandEvent & WXUNUSED(event))
{
   mTrebleT->SetValue(wxString::Format(wxT("%.1f"), mTrebleS->GetValue() * 0.1));
   UpdateUI();
}

void BassTrebleDialog::OnLevelSlider(wxCommandEvent & WXUNUSED(event))
{
   mLevelT->SetValue(wxString::Format(wxT("%.1f"), mLevelS->GetValue() * 0.1));
   UpdateUI();
}

void BassTrebleDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   double oldBass = mEffect->dB_bass;
   double oldTreble = mEffect->dB_treble;
   double oldLevel = mEffect->dB_level;
   bool oldUseGain = mEffect->mbNormalize;

   mEffect->dB_bass = bass;
   mEffect->dB_treble = treble;
   mEffect->dB_level = level;
   mEffect->mbNormalize = mbNormalize;
   mEffect->Preview();

   mEffect->dB_bass = oldBass;
   mEffect->dB_treble = oldTreble;
   mEffect->dB_level = oldLevel;
   mEffect->mbNormalize = oldUseGain;
}
