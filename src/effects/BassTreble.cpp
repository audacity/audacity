/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

   BassTreble.cpp
   Steve Daulton

******************************************************************//**

\class EffectBassTreble
\brief A high shelf and low shelf filter.

   The first pass applies the equalization and calculates the
   peak value. The second pass, if enabled, normalizes to the
   level set by the level control.

*//*******************************************************************/

#include "../Audacity.h"

#include <math.h>

#include <wx/button.h>
#include <wx/intl.h>
#include <wx/panel.h>
#include <wx/sizer.h>

#include "../Prefs.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"

#include "BassTreble.h"

enum 
{
   ID_Bass = 10000,
   ID_Treble,
   ID_Level,
   ID_Normalize,
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key               Def      Min      Max      Scale
Param( Bass,      double,  XO("Bass"),       0.0,     -15.0,   15.0,    1  );
Param( Treble,    double,  XO("Treble"),     0.0,     -15.0,   15.0,    1  );
Param( Level,     double,  XO("Level"),      -1.0,    -30.0,   0.0,     1  );
Param( Normalize, bool,    XO("Normalize"),  true,    false,   true,    1  );

// Sliders are integer, so range is x 10
// to allow 1 decimal place resolution
static const int kSliderScale = 10;

// Used to communicate the type of the filter.
enum kShelfType
{
   kBass,
   kTreble
};

BEGIN_EVENT_TABLE(EffectBassTreble, wxEvtHandler)
   EVT_SLIDER(ID_Bass, EffectBassTreble::OnBassSlider)
   EVT_SLIDER(ID_Treble, EffectBassTreble::OnTrebleSlider)
   EVT_SLIDER(ID_Level, EffectBassTreble::OnLevelSlider)
   EVT_TEXT(ID_Bass, EffectBassTreble::OnBassText)
   EVT_TEXT(ID_Treble, EffectBassTreble::OnTrebleText)
   EVT_TEXT(ID_Level, EffectBassTreble::OnLevelText)
   EVT_CHECKBOX(ID_Normalize, EffectBassTreble::OnNormalize)
END_EVENT_TABLE()

EffectBassTreble::EffectBassTreble()
{
   dB_bass = DEF_Bass;
   dB_treble = DEF_Treble;
   dB_level = DEF_Level;
   mbNormalize = DEF_Normalize;

   SetLinearEffectFlag(false);
}

EffectBassTreble::~EffectBassTreble()
{
}

// IdentInterface implementation

wxString EffectBassTreble::GetSymbol()
{
   return BASSTREBLE_PLUGIN_SYMBOL;
}

wxString EffectBassTreble::GetDescription()
{
   return XO("Increases or decreases the lower frequencies and higher frequencies of your audio independently");
}

// EffectIdentInterface implementation

EffectType EffectBassTreble::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

int EffectBassTreble::GetAudioInCount()
{
   return 1;
}

int EffectBassTreble::GetAudioOutCount()
{
   return 1;
}

bool EffectBassTreble::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   if (GetPass() == 1)
   {
      const float slope = 0.4f;   // same slope for both filters
      const double hzBass = 250.0f;
      const double hzTreble = 4000.0f;

      //(re)initialise filter parameters
      xn1Bass=xn2Bass=yn1Bass=yn2Bass=0;
      xn1Treble=xn2Treble=yn1Treble=yn2Treble=0;

      // Compute coefficents of the low shelf biquand IIR filter
      Coefficents(hzBass, slope, dB_bass, kBass,
                  a0Bass, a1Bass, a2Bass,
                  b0Bass, b1Bass, b2Bass);

      // Compute coefficents of the high shelf biquand IIR filter
      Coefficents(hzTreble, slope, dB_treble, kTreble,
                  a0Treble, a1Treble, a2Treble,
                  b0Treble, b1Treble, b2Treble);
   }

   return true;
}

sampleCount EffectBassTreble::ProcessBlock(float **inBlock, float **outBlock, sampleCount blockLen)
{
   float *ibuf = inBlock[0];
   float *obuf = outBlock[0];

   if (GetPass() == 1)
   {
      for (sampleCount i = 0; i < blockLen; i++)
      {
         obuf[i] = DoFilter(ibuf[i]) / mPreGain;
      }
   }
   else
   {
      float gain = (pow(10.0, dB_level / 20.0f)) / mMax;
      for (sampleCount i = 0; i < blockLen; i++)
      {
         // Normalize to specified level
         obuf[i] = ibuf[i] * (mPreGain * gain);
      }
   }

   return blockLen;
}

bool EffectBassTreble::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Bass, dB_bass);
   parms.Write(KEY_Treble, dB_treble);
   parms.Write(KEY_Level, dB_level);
   parms.Write(KEY_Normalize, mbNormalize);

   return true;
}

bool EffectBassTreble::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyDouble(Bass);
   ReadAndVerifyDouble(Treble);
   ReadAndVerifyDouble(Level);
   ReadAndVerifyBool(Normalize);

   dB_bass = Bass;
   dB_treble = Treble;
   dB_level = Level;
   mbNormalize = Normalize;

   return true;
}

// Effect implementation

bool EffectBassTreble::Startup()
{
   wxString base = wxT("/Effects/BassTreble/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      int readBool;
      gPrefs->Read(base + wxT("Bass"), &dB_bass, 0.0);
      gPrefs->Read(base + wxT("Treble"), &dB_treble, 0.0);
      gPrefs->Read(base + wxT("Level"), &dB_level, -1.0);
      gPrefs->Read(base + wxT("Normalize"), &readBool, 1 );
      
      // Validate data
      dB_level = (dB_level > 0) ? 0 : dB_level;
      mbNormalize = (readBool != 0);

      SaveUserPreset(GetCurrentSettingsGroup());
      
      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

bool EffectBassTreble::InitPass1()
{
   mMax = 0.0;

   // Integer format tracks require headroom to avoid clipping
   // when saved between passes (bug 619)

   if (mbNormalize)  // don't need to calculate this if only doing one pass.
   {
      // Up to (gain + 6dB) headroom required for treble boost (experimental).
      mPreGain = (dB_treble > 0) ? (dB_treble + 6.0) : 0.0;
      if (dB_bass >= 0)
      {
         mPreGain = (mPreGain > dB_bass) ? mPreGain : dB_bass;
      }
      else
      {
         // Up to 6 dB headroom reaquired for bass cut (experimental)
         mPreGain = (mPreGain > 6.0) ? mPreGain : 6.0;
      }
      mPreGain = (exp(log(10.0) * mPreGain / 20));   // to linear
   }
   else
   {
      mPreGain = 1.0;   // Unity gain
   }

   return true;
}

bool EffectBassTreble::InitPass2()
{
   return mbNormalize && mMax != 0;
}

void EffectBassTreble::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay(0);
   {
      S.StartStatic(wxT(""));
      {
         S.StartMultiColumn(3, wxEXPAND);
         S.SetStretchyCol(2);
         {
            #ifdef __WXGTK__
            // BoxSizer is to make first mnemonic work, on Linux.
            wxPanel* cPanel = new wxPanel(S.GetParent(), wxID_ANY);
            wxBoxSizer* cSizer = new wxBoxSizer(wxVERTICAL);
            cPanel->SetSizer(cSizer);
            #endif

            // Bass control
            FloatingPointValidator<double> vldBass(1, &dB_bass);
            vldBass.SetRange(MIN_Bass, MAX_Bass);
            mBassT = S.Id(ID_Bass).AddTextBox(_("&Bass (dB):"), wxT(""), 10);
            mBassT->SetName(_("Bass (dB):"));
            mBassT->SetValidator(vldBass);

            S.SetStyle(wxSL_HORIZONTAL);
            mBassS = S.Id(ID_Bass).AddSlider(wxT(""), 0, MAX_Bass * kSliderScale, MIN_Bass * kSliderScale);
            mBassS->SetName(_("Bass"));
            mBassS->SetPageSize(30);

            // Treble control
            FloatingPointValidator<double> vldTreble(1, &dB_treble);
            vldTreble.SetRange(MIN_Treble, MAX_Treble);
            mTrebleT = S.Id(ID_Treble).AddTextBox(_("&Treble (dB):"), wxT(""), 10);
            mTrebleT->SetValidator(vldTreble);

            S.SetStyle(wxSL_HORIZONTAL);
            mTrebleS = S.Id(ID_Treble).AddSlider(wxT(""), 0, MAX_Treble * kSliderScale, MIN_Treble * kSliderScale);
            mTrebleS->SetName(_("Treble"));
            mTrebleS->SetPageSize(30);

            // Level control
            FloatingPointValidator<double> vldLevel(1, &dB_level);
            vldLevel.SetRange(MIN_Level, MAX_Level);
            mLevelT = S.Id(ID_Level).AddTextBox(_("&Level (dB):"), wxT(""), 10);
            mLevelT->SetValidator(vldLevel);

            S.SetStyle(wxSL_HORIZONTAL);
            mLevelS = S.Id(ID_Level).AddSlider(wxT(""), 0, MAX_Level * kSliderScale, MIN_Level * kSliderScale);
            mLevelS->SetName(_("Level"));
            mLevelS->SetPageSize(30);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();

      // Normalize checkbox
      S.StartHorizontalLay(wxLEFT, true);
      {
         mNormalizeCheckBox = S.Id(ID_Normalize).AddCheckBox(_("&Enable level control"),
                                       DEF_Normalize ? wxT("true") : wxT("false"));
         mWarning = S.AddVariableText(wxT(""), false, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   return;
}

bool EffectBassTreble::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mBassS->SetValue((int) dB_bass * kSliderScale + 0.5);
   mTrebleS->SetValue((int) dB_treble * kSliderScale + 0.5);
   mLevelS->SetValue((int) dB_level * kSliderScale + 0.5);
   mNormalizeCheckBox->SetValue(mbNormalize);

   UpdateUI();

   return true;
}

bool EffectBassTreble::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   mbNormalize = mNormalizeCheckBox->GetValue();

   return true;
}

// EffectBassTreble implementation

void EffectBassTreble::Coefficents(double hz, float slope, double gain, int type,
                                   float& a0, float& a1, float& a2,
                                   float& b0, float& b1, float& b2)
{
   double w = 2 * M_PI * hz / mSampleRate;
   double a = exp(log(10.0) * gain / 40);
   double b = sqrt((a * a + 1) / slope - (pow((a - 1), 2)));

   if (type == kBass)
   {
      b0 = a * ((a + 1) - (a - 1) * cos(w) + b * sin(w));
      b1 = 2 * a * ((a - 1) - (a + 1) * cos(w));
      b2 = a * ((a + 1) - (a - 1) * cos(w) - b * sin(w));
      a0 = ((a + 1) + (a - 1) * cos(w) + b * sin(w));
      a1 = -2 * ((a - 1) + (a + 1) * cos(w));
      a2 = (a + 1) + (a - 1) * cos(w) - b * sin(w);
   }
   else //assumed kTreble
   {
      b0 = a * ((a + 1) + (a - 1) * cos(w) + b * sin(w));
      b1 = -2 * a * ((a - 1) + (a + 1) * cos(w));
      b2 = a * ((a + 1) + (a - 1) * cos(w) - b * sin(w));
      a0 = ((a + 1) - (a - 1) * cos(w) + b * sin(w));
      a1 = 2 * ((a - 1) - (a + 1) * cos(w));
      a2 = (a + 1) - (a - 1) * cos(w) - b * sin(w);
   }
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

void EffectBassTreble::UpdateUI()
{
   double bass, treble, level;
   mBassT->GetValue().ToDouble(&bass);
   mTrebleT->GetValue().ToDouble(&treble);
   mLevelT->GetValue().ToDouble(&level);
   bool enable = mNormalizeCheckBox->GetValue();

   // Disallow level control if disabled
   mLevelT->Enable(enable);
   mLevelS->Enable(enable);

   if (bass == 0 && treble == 0 && !enable)
   {
      // Disallow Apply if nothing to do
      EnableApply(false);
      mWarning->SetLabel(_("    No change to apply."));
   }
   else
   {
      if (level > 0 && enable)
      {
         // Disallow Apply if level enabled and > 0
         EnableApply(false);
         mWarning->SetLabel(_(":   Maximum 0 dB."));
      }
      else
      {
         // Apply enabled
         EnableApply(true);
         mWarning->SetLabel(wxT(""));
      }
   }
}

void EffectBassTreble::OnBassText(wxCommandEvent & WXUNUSED(evt))
{
   mBassT->GetValidator()->TransferFromWindow();
   mBassS->SetValue((int) floor(dB_bass * kSliderScale + 0.5));
   UpdateUI();
}

void EffectBassTreble::OnTrebleText(wxCommandEvent & WXUNUSED(evt))
{
   mTrebleT->GetValidator()->TransferFromWindow();
   mTrebleS->SetValue((int) floor(dB_treble * kSliderScale + 0.5));
   UpdateUI();
}

void EffectBassTreble::OnLevelText(wxCommandEvent & WXUNUSED(evt))
{
   mLevelT->GetValidator()->TransferFromWindow();
   mLevelS->SetValue((int) floor(dB_level * kSliderScale + 0.5));
   UpdateUI();
}

void EffectBassTreble::OnBassSlider(wxCommandEvent & evt)
{
   dB_bass = (double) evt.GetInt() / kSliderScale;
   mBassT->GetValidator()->TransferToWindow();
   UpdateUI();
}

void EffectBassTreble::OnTrebleSlider(wxCommandEvent & evt)
{
   dB_treble = (double) evt.GetInt() / kSliderScale;
   mTrebleT->GetValidator()->TransferToWindow();
   UpdateUI();
}

void EffectBassTreble::OnLevelSlider(wxCommandEvent & evt)
{
   dB_level = (double) evt.GetInt() / kSliderScale;
   mLevelT->GetValidator()->TransferToWindow();
   UpdateUI();
}

void EffectBassTreble::OnNormalize(wxCommandEvent& WXUNUSED(evt))
{
   UpdateUI();
}
