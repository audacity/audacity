/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2013 Audacity Team.
   License: GPL v2.  See License.txt.

  ChangePitch.cpp
  Vaughan Johnson, Dominic Mazzoni, Steve Daulton 

******************************************************************//**

\file ChangePitch.cpp
\brief Change Pitch effect provides raising or lowering 
the pitch without changing the tempo.

*//*******************************************************************/

#include "../Audacity.h" // for USE_SOUNDTOUCH

#if USE_SOUNDTOUCH

#include "ChangePitch.h"

#include <float.h>
#include <math.h>

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>

#include "../ShuttleGui.h"
#include "../PitchName.h"
#include "../Spectrum.h"
#include "../WaveTrack.h"
#include "TimeWarper.h"


// EffectChangePitch

EffectChangePitch::EffectChangePitch()
{
   m_dSemitonesChange = 0.0;
   m_dStartFrequency = 0.0; // 0.0 => uninitialized
   m_dPercentChange = 0.0;
}

wxString EffectChangePitch::GetEffectDescription() 
{ 
   // This is useful only after m_dSemitonesChange has been set. 
   return wxString::Format(_("Applied effect: %s %.2f semitones"), 
                           this->GetEffectName().c_str(), 
                           m_dSemitonesChange); 
} 

bool EffectChangePitch::Init()
{
   mSoundTouch = NULL;
   return true;
}

// Deduce m_FromFrequency from the samples at the beginning of 
// the selection. Then set some other params accordingly.
void EffectChangePitch::DeduceFrequencies()
{
   // As a neat trick, attempt to get the frequency of the note at the
   // beginning of the selection.
   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   if (track) {
      double rate = track->GetRate();

      // Auto-size window -- high sample rates require larger windowSize.
      // Aim for around 2048 samples at 44.1 kHz (good down to about 100 Hz).
      // To detect single notes, analysis period should be about 0.2 seconds.
      // windowSize must be a power of 2.
      int windowSize = wxRound(pow(2.0, floor((log(rate / 20.0)/log(2.0)) + 0.5)));
      // windowSize < 256 too inaccurate
      windowSize = (windowSize > 256)? windowSize : 256;

      // we want about 0.2 seconds to catch the first note.
      // number of windows rounded to nearest integer >= 1.
      int numWindows = wxRound((double)(rate / (5.0f * windowSize)));
      numWindows = (numWindows > 0)? numWindows : 1;
      
      double trackStart = track->GetStartTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      sampleCount start = track->TimeToLongSamples(t0);

      int analyzeSize = windowSize * numWindows;      
      float * buffer;
      buffer = new float[analyzeSize];

      float * freq;
      freq = new float[windowSize/2];

      float * freqa;
      freqa = new float[windowSize/2];

      int i, j, argmax;
      int lag;

      for(j=0; j<windowSize/2; j++)
         freqa[j] = 0;

      track->Get((samplePtr) buffer, floatSample, start, analyzeSize);
      for(i=0; i<numWindows; i++) {
         ComputeSpectrum(buffer+i*windowSize, windowSize,
                         windowSize, rate, freq, true);
         for(j=0; j<windowSize/2; j++)
            freqa[j] += freq[j];
      }
      argmax=0;
      for(j=1; j<windowSize/2; j++)
         if (freqa[j] > freqa[argmax])
            argmax = j;

      delete [] freq;
      delete [] freqa;
      delete [] buffer;

      lag = (windowSize/2 - 1) - argmax;
      m_dStartFrequency = rate / lag;
   }
}

bool EffectChangePitch::PromptUser()
{
   this->DeduceFrequencies(); // Set frequency-related control values based on sample.

   ChangePitchDialog dlog(this, mParent, m_dSemitonesChange, m_dStartFrequency);
   dlog.CentreOnParent();
   dlog.ShowModal();

  if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   m_dSemitonesChange = dlog.m_dSemitonesChange;
   m_dStartFrequency = dlog.m_FromFrequency;
   m_dPercentChange = dlog.m_dPercentChange;
   return true;
}

bool EffectChangePitch::TransferParameters( Shuttle & shuttle )
{  
   // Vaughan, 2013-06: Long lost to history, I don't see why m_dPercentChange was chosen to be shuttled. 
   // Only m_dSemitonesChange is used in Process(). 
   shuttle.TransferDouble(wxT("Percentage"),m_dPercentChange,0.0);
   m_dSemitonesChange = (12.0 * log((100.0 + m_dPercentChange) / 100.0)) / log(2.0);
   return true;
}

bool EffectChangePitch::Process()
{
   mSoundTouch = new SoundTouch();
   SetTimeWarper(new IdentityTimeWarper());
   mSoundTouch->setPitchSemiTones((float)(m_dSemitonesChange));
#ifdef USE_MIDI
   // Note: m_dSemitonesChange is private to ChangePitch because it only
   // needs to pass it along to mSoundTouch (above). I added mSemitones
   // to SoundTouchEffect (the super class) to convey this value
   // to process Note tracks. This approach minimizes changes to existing 
   // code, but it would be cleaner to change all m_dSemitonesChange to
   // mSemitones, make mSemitones exist with or without USE_MIDI, and 
   // eliminate the next line:
   mSemitones = m_dSemitonesChange;
#endif
   return this->EffectSoundTouch::Process();
}

//----------------------------------------------------------------------------
// ChangePitchDialog
//----------------------------------------------------------------------------

// Soundtouch is not reasonable below -99% or above 3000%.
// We warp the slider to go up to 400%, but user can enter up to 3000%
#define PERCENTCHANGE_MIN -99.0 
#define PERCENTCHANGE_MAX_SLIDER 100.0 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_MAX_TEXT 3000.0
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

enum {
   ID_TEXT_PERCENTCHANGE = 10001,
   ID_SLIDER_PERCENTCHANGE,
   ID_CHOICE_FROMPITCH,
   ID_SPIN_FROMOCTAVE,
   ID_CHOICE_TOPITCH,
   ID_SPIN_TOOCTAVE,
   ID_TEXT_SEMITONESCHANGE,
   ID_TEXT_FROMFREQUENCY,
   ID_TEXT_TOFREQUENCY
};

// event table for ChangePitchDialog

BEGIN_EVENT_TABLE(ChangePitchDialog, EffectDialog)
   EVT_CHOICE(ID_CHOICE_FROMPITCH, ChangePitchDialog::OnChoice_FromPitch)
   EVT_TEXT(ID_SPIN_FROMOCTAVE, ChangePitchDialog::OnSpin_FromOctave)
   EVT_CHOICE(ID_CHOICE_TOPITCH, ChangePitchDialog::OnChoice_ToPitch)
   EVT_TEXT(ID_SPIN_TOOCTAVE, ChangePitchDialog::OnSpin_ToOctave)

   EVT_TEXT(ID_TEXT_SEMITONESCHANGE, ChangePitchDialog::OnText_SemitonesChange)

   EVT_TEXT(ID_TEXT_FROMFREQUENCY, ChangePitchDialog::OnText_FromFrequency)
   EVT_TEXT(ID_TEXT_TOFREQUENCY, ChangePitchDialog::OnText_ToFrequency)

   EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangePitchDialog::OnText_PercentChange)
   EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangePitchDialog::OnSlider_PercentChange)

   EVT_BUTTON(ID_EFFECT_PREVIEW, ChangePitchDialog::OnPreview)
END_EVENT_TABLE()

ChangePitchDialog::ChangePitchDialog(EffectChangePitch *effect, wxWindow *parent, 
                                       double dSemitonesChange, double dStartFrequency)
:  EffectDialog(parent, _("Change Pitch"), PROCESS_EFFECT),
   mEffect(effect)
{
   m_bLoopDetect = false; 

   // NULL out these control members because there are some cases where the 
   // event table handlers get called during this method, and those handlers that 
   // can cause trouble check for NULL.
   m_pChoice_FromPitch = NULL;
   m_pSpin_FromOctave = NULL;
   m_pChoice_ToPitch = NULL;
   m_pSpin_ToOctave = NULL;
   
   m_pTextCtrl_SemitonesChange = NULL;

   m_pTextCtrl_FromFrequency = NULL;
   m_pTextCtrl_ToFrequency = NULL;
   
   m_pTextCtrl_PercentChange = NULL;
   m_pSlider_PercentChange = NULL;

   // effect parameters
   double dFromMIDInote = FreqToMIDInote(dStartFrequency);
   double dToMIDInote = dFromMIDInote + dSemitonesChange;
   m_nFromPitch = PitchIndex(dFromMIDInote);
   m_nFromOctave = PitchOctave(dFromMIDInote);
   m_nToPitch = PitchIndex(dToMIDInote);
   m_nToOctave = PitchOctave(dToMIDInote);

   m_dSemitonesChange = dSemitonesChange;

   m_FromFrequency = dStartFrequency;
   this->Calc_PercentChange();
   this->Calc_ToFrequency();

   Init();
}

void ChangePitchDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator nullvld(wxFILTER_INCLUDE_CHAR_LIST);
   wxTextValidator numvld(wxFILTER_NUMERIC);
   
   wxTextValidator nonNegNumValidator(wxFILTER_INCLUDE_CHAR_LIST); // like wxFILTER_NUMERIC, but disallow negative numbers. 
   wxArrayString aIncludes; 
   aIncludes.Add(wxT("0"));
   aIncludes.Add(wxT("1"));
   aIncludes.Add(wxT("2"));
   aIncludes.Add(wxT("3"));
   aIncludes.Add(wxT("4"));
   aIncludes.Add(wxT("5"));
   aIncludes.Add(wxT("6"));
   aIncludes.Add(wxT("7"));
   aIncludes.Add(wxT("8"));
   aIncludes.Add(wxT("9"));
   aIncludes.Add(wxT("."));
   nonNegNumValidator.SetIncludes(aIncludes); 

   wxArrayString pitch;
   pitch.Add(wxT("C"));
   pitch.Add(wxT("C#/Db"));
   pitch.Add(wxT("D"));
   pitch.Add(wxT("D#/Eb"));
   pitch.Add(wxT("E"));
   pitch.Add(wxT("F"));
   pitch.Add(wxT("F#/Gb"));
   pitch.Add(wxT("G"));
   pitch.Add(wxT("G#/Ab"));
   pitch.Add(wxT("A"));
   pitch.Add(wxT("A#/Bb"));
   pitch.Add(wxT("B"));

   S.SetBorder(5);

   S.StartVerticalLay();
   {
      S.AddTitle(_("Change Pitch without Changing Tempo"));
      S.AddTitle(
         wxString::Format(_("Estimated Start Pitch: %s%d (%.3f Hz)"), 
                           pitch[m_nFromPitch].c_str(), m_nFromOctave, m_FromFrequency));
   }
   S.EndVerticalLay();

   /* i18n-hint: (noun) Musical pitch.*/
   S.StartStatic(_("Pitch"));
   {
      S.StartMultiColumn(6, wxALIGN_CENTER); // 6 controls, because each AddChoice adds a wxStaticText and a wxChoice.
      {
         m_pChoice_FromPitch = S.Id(ID_CHOICE_FROMPITCH).AddChoice(_("from"), wxT(""), &pitch);
         m_pChoice_FromPitch->SetName(_("from"));
         m_pChoice_FromPitch->SetSizeHints(80, -1);

         m_pSpin_FromOctave = S.Id(ID_SPIN_FROMOCTAVE).AddSpinCtrl(wxT(""), m_nFromOctave, INT_MAX, INT_MIN); 
         m_pSpin_FromOctave->SetName(_("from Octave"));
         m_pSpin_FromOctave->SetSizeHints(50, -1);

         m_pChoice_ToPitch = S.Id(ID_CHOICE_TOPITCH).AddChoice(_("to"), wxT(""), &pitch);
         m_pChoice_ToPitch->SetName(_("to"));
         m_pChoice_ToPitch->SetSizeHints(80, -1);

         m_pSpin_ToOctave = 
            S.Id(ID_SPIN_TOOCTAVE).AddSpinCtrl(wxT(""), m_nToOctave, INT_MAX, INT_MIN); 
         m_pSpin_ToOctave->SetName(_("to Octave"));
         m_pSpin_ToOctave->SetSizeHints(50, -1);
      }
      S.EndMultiColumn();
  
      S.StartHorizontalLay(wxALIGN_CENTER);
      {
         m_pTextCtrl_SemitonesChange = 
            S.Id(ID_TEXT_SEMITONESCHANGE).AddTextBox(_("Semitones (half-steps):"), wxT(""), 12);
         m_pTextCtrl_SemitonesChange->SetName(_("Semitones (half-steps)"));
         m_pTextCtrl_SemitonesChange->SetValidator(numvld);
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();

   S.StartStatic(_("Frequency"));
   {
      S.StartMultiColumn(5, wxALIGN_CENTER); // 5, because AddTextBox adds a wxStaticText and a wxTextCtrl.
      {
         m_pTextCtrl_FromFrequency = S.Id(ID_TEXT_FROMFREQUENCY).AddTextBox(_("from"), wxT(""), 12);
         m_pTextCtrl_FromFrequency->SetName(_("from (Hz)"));
         m_pTextCtrl_FromFrequency->SetValidator(nonNegNumValidator);

         m_pTextCtrl_ToFrequency = S.Id(ID_TEXT_TOFREQUENCY).AddTextBox(_("to"), wxT(""), 12);
         m_pTextCtrl_ToFrequency->SetName(_("to (Hz)"));
         m_pTextCtrl_ToFrequency->SetValidator(nonNegNumValidator);

         S.AddUnits(_("Hz"));
      }
      S.EndMultiColumn();

      S.StartHorizontalLay(wxALIGN_CENTER);
      {
         m_pTextCtrl_PercentChange = S.Id(ID_TEXT_PERCENTCHANGE).AddTextBox(_("Percent Change:"), wxT(""), 12);
         m_pTextCtrl_PercentChange->SetValidator(numvld);
      }
      S.EndHorizontalLay();

      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetStyle(wxSL_HORIZONTAL);
         m_pSlider_PercentChange = S.Id(ID_SLIDER_PERCENTCHANGE)
            .AddSlider(wxT(""), 0, (int)PERCENTCHANGE_MAX_SLIDER, (int)PERCENTCHANGE_MIN);
         m_pSlider_PercentChange->SetName(_("Percent Change"));
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();
}

bool ChangePitchDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

   // from/to pitch controls
   if (m_pChoice_FromPitch) 
      m_pChoice_FromPitch->SetSelection(m_nFromPitch);
   if (m_pSpin_FromOctave)
      m_pSpin_FromOctave->SetValue(m_nFromOctave); 
   this->Update_Choice_ToPitch(); 
   this->Update_Spin_ToOctave(); 

   // semitones change control
   this->Update_Text_SemitonesChange();

   // from/to frequency controls
   if (m_pTextCtrl_FromFrequency) {
      wxString str;
      if ((m_ToFrequency > 0.0) && (m_ToFrequency <= DBL_MAX))
         str.Printf(wxT("%.3f"), m_FromFrequency);
      else
         str = wxT("");
      m_pTextCtrl_FromFrequency->SetValue(str);
   }

   this->Update_Text_ToFrequency();

   // percent change controls
   this->Update_Text_PercentChange();
   this->Update_Slider_PercentChange();

   m_bLoopDetect = false;

   return true;
}

bool ChangePitchDialog::TransferDataFromWindow()
{
   double newDouble;
   wxString str;


   // from/to pitch controls
   if (m_pChoice_FromPitch) 
      m_nFromPitch = m_pChoice_FromPitch->GetSelection(); 
   if (m_pSpin_FromOctave) 
      m_nFromOctave = m_pSpin_FromOctave->GetValue();

   if (m_pChoice_ToPitch) 
      m_nToPitch = m_pChoice_ToPitch->GetSelection();


   // semitones change control
   if (m_pTextCtrl_SemitonesChange) {
      str = m_pTextCtrl_SemitonesChange->GetValue();
      str.ToDouble(&newDouble);
      m_dSemitonesChange = newDouble;
   }


   // from/to frequency controls
   if (m_pTextCtrl_FromFrequency) {
      str = m_pTextCtrl_FromFrequency->GetValue();
      str.ToDouble(&newDouble);
      m_FromFrequency = newDouble;
   }

   if (m_pTextCtrl_ToFrequency) {
      str = m_pTextCtrl_ToFrequency->GetValue();
      str.ToDouble(&newDouble);
      m_ToFrequency = newDouble;
   }


   // percent change controls
   if (m_pTextCtrl_PercentChange) {
      str = m_pTextCtrl_PercentChange->GetValue();
      str.ToDouble(&newDouble);
      m_dPercentChange = newDouble;
   }

   // No need to update Slider_PercentChange here because TextCtrl_PercentChange 
   // always tracks it & is more precise (decimal points).


   return true;
}


// calculations

void ChangePitchDialog::Calc_ToPitch() 
{
   int nSemitonesChange = 
      (int)(m_dSemitonesChange + ((m_dSemitonesChange < 0.0) ? -0.5 : 0.5));
   m_nToPitch = (m_nFromPitch + nSemitonesChange) % 12;
   if (m_nToPitch < 0)
      m_nToPitch += 12;
}

void ChangePitchDialog::Calc_ToOctave()
{
   m_nToOctave = PitchOctave(FreqToMIDInote(m_ToFrequency));
}

void ChangePitchDialog::Calc_SemitonesChange_fromPitches()
{
   m_dSemitonesChange = 
      PitchToMIDInote(m_nToPitch, m_nToOctave) - PitchToMIDInote(m_nFromPitch, m_nFromOctave);
}

void ChangePitchDialog::Calc_SemitonesChange_fromPercentChange()
{
   // Use m_dPercentChange rather than m_FromFrequency & m_ToFrequency, because 
   // they start out uninitialized, but m_dPercentChange is always valid.
   m_dSemitonesChange = (12.0 * log((100.0 + m_dPercentChange) / 100.0)) / log(2.0);
}

void ChangePitchDialog::Calc_ToFrequency()
{
   m_ToFrequency = (m_FromFrequency * (100.0 + m_dPercentChange)) / 100.0;
}

void ChangePitchDialog::Calc_PercentChange()
{
   m_dPercentChange = 100.0 * (pow(2.0, (m_dSemitonesChange / 12.0)) - 1.0);
}


// handlers

void ChangePitchDialog::OnChoice_FromPitch(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pChoice_FromPitch) {
      m_nFromPitch = m_pChoice_FromPitch->GetSelection();
      m_FromFrequency = PitchToFreq(m_nFromPitch, m_nFromOctave);

      this->Calc_ToPitch();
      this->Calc_ToFrequency();
      this->Calc_ToOctave(); // Call after Calc_ToFrequency().

      m_bLoopDetect = true;
      {
         this->Update_Choice_ToPitch();
         this->Update_Spin_ToOctave();
         this->Update_Text_FromFrequency();
         this->Update_Text_ToFrequency();
      }
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnSpin_FromOctave(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pSpin_FromOctave) 
   {
      m_nFromOctave = m_pSpin_FromOctave->GetValue();
      //vvv If I change this code to not keep semitones and percent constant, 
      // will need validation code as in OnSpin_ToOctave.
      m_FromFrequency = PitchToFreq(m_nFromPitch, m_nFromOctave);

      this->Calc_ToFrequency();
      this->Calc_ToOctave(); // Call after Calc_ToFrequency().

      m_bLoopDetect = true;
      {
         this->Update_Spin_ToOctave();
         this->Update_Text_FromFrequency();
         this->Update_Text_ToFrequency();
      }
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnChoice_ToPitch(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pChoice_ToPitch) 
   {
      m_nToPitch = m_pChoice_ToPitch->GetSelection();

      this->Calc_SemitonesChange_fromPitches();
      this->Calc_PercentChange(); // Call *after* m_dSemitonesChange is updated.
      this->Calc_ToFrequency(); // Call *after* m_dPercentChange is updated.

      m_bLoopDetect = true;
      {
         this->Update_Text_SemitonesChange();
         this->Update_Text_ToFrequency();
         this->Update_Text_PercentChange();
         this->Update_Slider_PercentChange();
      }
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnSpin_ToOctave(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pSpin_ToOctave) 
   {
      int nNewValue = m_pSpin_ToOctave->GetValue();
      // Validation: Rather than set a range for octave numbers, enforce a range that 
      // keeps m_dPercentChange above -99%, per Soundtouch constraints. 
      if ((nNewValue + 3) < m_nFromOctave)
      {
         ::wxBell();
         m_pSpin_ToOctave->SetValue(m_nFromOctave - 3);
         return;
      }
      m_nToOctave = nNewValue;

      m_ToFrequency = PitchToFreq(m_nToPitch, m_nToOctave);

      this->Calc_SemitonesChange_fromPitches();
      this->Calc_PercentChange(); // Call *after* m_dSemitonesChange is updated.

      m_bLoopDetect = true;
      {
         this->Update_Text_SemitonesChange();
         this->Update_Text_ToFrequency();
         this->Update_Text_PercentChange();
         this->Update_Slider_PercentChange();
      }
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnText_SemitonesChange(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_SemitonesChange) {
      wxString str = m_pTextCtrl_SemitonesChange->GetValue();
      if (str.IsEmpty())
         m_dSemitonesChange = 0.0;
      else
      {
         double newValue = 0;
         str.ToDouble(&newValue);
         m_dSemitonesChange = newValue;
      }

      this->Calc_PercentChange();
      this->Calc_ToFrequency(); // Call *after* m_dPercentChange is updated.
      this->Calc_ToPitch();
      this->Calc_ToOctave(); // Call after Calc_ToFrequency().

      m_bLoopDetect = true;
      {
         this->Update_Choice_ToPitch();
         this->Update_Spin_ToOctave();
         this->Update_Text_ToFrequency();
         this->Update_Text_PercentChange();
         this->Update_Slider_PercentChange();
      }
      m_bLoopDetect = false;

      // If m_dSemitonesChange is a big enough negative, we can go to or below 0 freq. 
      // If m_dSemitonesChange is a big enough positive, we can go to 1.#INF (Windows) or inf (Linux). 
      // But practically, these are best limits for Soundtouch.
      bool bIsGoodValue = (m_dSemitonesChange > -80.0) && (m_dSemitonesChange <= 60.0);
      this->FindWindow(wxID_OK)->Enable(bIsGoodValue);
      this->FindWindow(ID_EFFECT_PREVIEW)->Enable(bIsGoodValue);
   }
}

void ChangePitchDialog::OnText_FromFrequency(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_FromFrequency) {
      wxString str = m_pTextCtrl_FromFrequency->GetValue();
      double newDouble;
      str.ToDouble(&newDouble);
      // Empty string causes unpredictable results with ToDouble() and later calculations.
      // Non-positive frequency makes no sense, but user might still be editing, 
      // so it's not an error, but we do not want to update the values/controls. 
      if (str.IsEmpty() || (newDouble <= 0.0) || (newDouble > DBL_MAX))
      {
         this->FindWindow(wxID_OK)->Disable();
         this->FindWindow(ID_EFFECT_PREVIEW)->Disable();
         return;
      }
      m_FromFrequency = newDouble;

      double newFromMIDInote = FreqToMIDInote(m_FromFrequency);
      m_nFromPitch = PitchIndex(newFromMIDInote);
      m_nFromOctave = PitchOctave(newFromMIDInote);
      this->Calc_ToPitch();
      this->Calc_ToFrequency();
      this->Calc_ToOctave(); // Call after Calc_ToFrequency().

      m_bLoopDetect = true;
      {
         this->Update_Choice_FromPitch();
         this->Update_Spin_FromOctave();
         this->Update_Choice_ToPitch();
         this->Update_Spin_ToOctave();
         this->Update_Text_ToFrequency();
      }
      m_bLoopDetect = false;

      // Success. Make sure OK and Preview are enabled, in case we disabled above during editing. 
      this->FindWindow(wxID_OK)->Enable();
      this->FindWindow(ID_EFFECT_PREVIEW)->Enable();
}
}

void ChangePitchDialog::OnText_ToFrequency(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_ToFrequency) {
      wxString str = m_pTextCtrl_ToFrequency->GetValue();
      double newDouble;
      str.ToDouble(&newDouble);
      // Empty string causes unpredictable results with ToDouble() and later calculations.
      // Non-positive frequency makes no sense, but user might still be editing, 
      // so it's not an error, but we do not want to update the values/controls. 
      if (str.IsEmpty() || (newDouble <= 0.0) )
      {
         this->FindWindow(wxID_OK)->Disable();
         this->FindWindow(ID_EFFECT_PREVIEW)->Disable();
         return;
      }
      m_ToFrequency = newDouble;

      m_dPercentChange = (((double)(m_ToFrequency) * 100.0) / 
                           (double)(m_FromFrequency)) - 100.0;

      this->Calc_ToOctave(); // Call after Calc_ToFrequency().
      this->Calc_SemitonesChange_fromPercentChange();
      this->Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.

      m_bLoopDetect = true;
      {
         this->Update_Choice_ToPitch();
         this->Update_Spin_ToOctave();
         this->Update_Text_SemitonesChange();
         this->Update_Text_PercentChange();
         this->Update_Slider_PercentChange();
      }
      m_bLoopDetect = false;
   
      // Success. Make sure OK and Preview are disabled if percent change is out of bounds. 
      // Can happen while editing. 
      // If the value is good, might also need to re-enable because of above clause. 
      bool bIsGoodValue = (m_dPercentChange > PERCENTCHANGE_MIN) && (m_dPercentChange <= PERCENTCHANGE_MAX_TEXT);
      this->FindWindow(wxID_OK)->Enable(bIsGoodValue);
      this->FindWindow(ID_EFFECT_PREVIEW)->Enable(bIsGoodValue);
   }
}

void ChangePitchDialog::OnText_PercentChange(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PercentChange) {
      wxString str = m_pTextCtrl_PercentChange->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      // User might still be editing, so out of bounds is not an error, 
      // but we do not want to update the values/controls. 
      if (str.IsEmpty() || (newValue < PERCENTCHANGE_MIN) || (newValue > PERCENTCHANGE_MAX_TEXT))
      {
         this->FindWindow(wxID_OK)->Disable();
         this->FindWindow(ID_EFFECT_PREVIEW)->Disable();
         return;
      }
      m_dPercentChange = newValue;

      this->Calc_SemitonesChange_fromPercentChange();
      this->Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.
      this->Calc_ToFrequency();
      this->Calc_ToOctave(); // Call after Calc_ToFrequency().

      m_bLoopDetect = true;
      {
         this->Update_Choice_ToPitch();
         this->Update_Spin_ToOctave();
         this->Update_Text_SemitonesChange();
         this->Update_Text_ToFrequency();
         this->Update_Slider_PercentChange();
      }
      m_bLoopDetect = false;

      // Success. Make sure OK and Preview are enabled, in case we disabled above during editing. 
      this->FindWindow(wxID_OK)->Enable();
      this->FindWindow(ID_EFFECT_PREVIEW)->Enable();
   }
}

void ChangePitchDialog::OnSlider_PercentChange(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pSlider_PercentChange) {
      m_dPercentChange = (double)(m_pSlider_PercentChange->GetValue()); 
      // Warp positive values to actually go up faster & further than negatives.
      if (m_dPercentChange > 0.0)
         m_dPercentChange = pow(m_dPercentChange, PERCENTCHANGE_SLIDER_WARP);

      this->Calc_SemitonesChange_fromPercentChange();
      this->Calc_ToPitch(); // Call *after* m_dSemitonesChange is updated.
      this->Calc_ToFrequency();
      this->Calc_ToOctave(); // Call after Calc_ToFrequency().

      m_bLoopDetect = true;
      {
         this->Update_Choice_ToPitch();
         this->Update_Spin_ToOctave();
         this->Update_Text_SemitonesChange();
         this->Update_Text_ToFrequency();
         this->Update_Text_PercentChange();
      }
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   double oldSemitonesChange = m_dSemitonesChange;
   if( m_dPercentChange < PERCENTCHANGE_MIN)
   {
      m_dPercentChange = PERCENTCHANGE_MIN;
      this->Update_Text_PercentChange();
   }
   mEffect->m_dSemitonesChange = m_dSemitonesChange;
   mEffect->Preview();
   mEffect->m_dSemitonesChange = oldSemitonesChange;
}

// helper fns for controls

void ChangePitchDialog::Update_Choice_FromPitch() 
{
   if (m_pChoice_FromPitch) 
      m_pChoice_FromPitch->SetSelection(m_nFromPitch);
}

void ChangePitchDialog::Update_Spin_FromOctave() 
{
   if (m_pSpin_FromOctave) 
      m_pSpin_FromOctave->SetValue(m_nFromOctave); 
}

void ChangePitchDialog::Update_Choice_ToPitch() 
{
   if (m_pChoice_ToPitch) 
      m_pChoice_ToPitch->SetSelection(m_nToPitch);
}

void ChangePitchDialog::Update_Spin_ToOctave() 
{
   if (m_pSpin_ToOctave) 
      m_pSpin_ToOctave->SetValue(m_nToOctave); 
}

void ChangePitchDialog::Update_Text_SemitonesChange()
{
   if (m_pTextCtrl_SemitonesChange) {
      wxString str;
      str.Printf(wxT("%.2f"), m_dSemitonesChange);
      m_pTextCtrl_SemitonesChange->SetValue(str);
   }
}

void ChangePitchDialog::Update_Text_FromFrequency() 
{
   if (m_pTextCtrl_FromFrequency) {
      wxString str;
      if ((m_FromFrequency > 0.0) && (m_FromFrequency <= DBL_MAX))
         str.Printf(wxT("%.3f"), m_FromFrequency);
      else
         str = wxT("");
      m_pTextCtrl_FromFrequency->SetValue(str);
   }
}

void ChangePitchDialog::Update_Text_ToFrequency() 
{
   if (m_pTextCtrl_ToFrequency) {
      wxString str;
      if ((m_ToFrequency > 0.0) && (m_ToFrequency <= DBL_MAX))
         str.Printf(wxT("%.3f"), m_ToFrequency);
      else
         str = wxT("");
      m_pTextCtrl_ToFrequency->SetValue(str);
   }
}


void ChangePitchDialog::Update_Text_PercentChange()
{
   if (m_pTextCtrl_PercentChange) {
      wxString str;
      if ((m_ToFrequency > 0.0) && (m_ToFrequency <= DBL_MAX))
         str.Printf(wxT("%.3f"), m_dPercentChange);
      else
         str = wxT("");
      m_pTextCtrl_PercentChange->SetValue(str);

      bool bIsGoodValue = (m_dPercentChange >= PERCENTCHANGE_MIN) && (m_dPercentChange <= PERCENTCHANGE_MAX_TEXT);
      this->FindWindow(wxID_OK)->Enable(bIsGoodValue);
      this->FindWindow(ID_EFFECT_PREVIEW)->Enable(bIsGoodValue);
   }
}

void ChangePitchDialog::Update_Slider_PercentChange()
{
   if (m_pSlider_PercentChange) {
      double unwarped = m_dPercentChange;
      if (unwarped > 0.0)
         // Un-warp values above zero to actually go up to PERCENTCHANGE_MAX_SLIDER.
         unwarped = pow(m_dPercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));

      // Add 0.5 to unwarped so trunc -> round.
      int newSetting = (int)(unwarped + 0.5);
      if (newSetting < PERCENTCHANGE_MIN)
         newSetting = (int)PERCENTCHANGE_MIN;
      if (newSetting > PERCENTCHANGE_MAX_SLIDER)
         newSetting = (int)PERCENTCHANGE_MAX_SLIDER;
      m_pSlider_PercentChange->SetValue(newSetting); 
   }
}


#endif // USE_SOUNDTOUCH

