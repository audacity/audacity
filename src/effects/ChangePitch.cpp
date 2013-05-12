/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangePitch.cpp

  Vaughan Johnson, Dominic Mazzoni
  
  Change Pitch effect provides raising or lowering 
  the pitch without changing the tempo.

**********************************************************************/

#include "../Audacity.h" // for USE_SOUNDTOUCH

#if USE_SOUNDTOUCH

#include "ChangePitch.h"

#include "../ShuttleGui.h"
#include "../PitchName.h"
#include "../Spectrum.h"
#include "../WaveTrack.h"
#include "TimeWarper.h"

#include <math.h>

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>

//
// EffectChangePitch
//

EffectChangePitch::EffectChangePitch()
{
   m_FromPitchIndex = -1;		// -1 => uninitialized
   m_bWantPitchDown = false;
   m_ToPitchIndex = -1;			// -1 => uninitialized

   m_SemitonesChange = 0.0;

   m_FromFrequency = 0.0;		// 0.0 => uninitialized
   m_ToFrequency = 0.0;			// 0.0 => uninitialized

   m_PercentChange = 0.0;
}

wxString EffectChangePitch::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set. 
   return wxString::Format(_("Applied effect: %s %.2f semitones"), 
                           this->GetEffectName().c_str(), 
                           m_SemitonesChange); 
} 

bool EffectChangePitch::Init()
{
   mSoundTouch = NULL;
   return true;
}

// DeduceFrequencies is Dominic's extremely cool trick (Vaughan sez so!) 
// to set deduce m_FromFrequency from the samples at the beginning of 
// the selection. Then we set some other params accordingly.
void EffectChangePitch::DeduceFrequencies()
{
   // As a neat trick, attempt to get the frequency of the note at the
   // beginning of the selection.
   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   if (track) {
      const int windowSize = 1024;
      const int analyzeSize = 8192;
      const int numWindows = analyzeSize / windowSize;
      double trackStart = track->GetStartTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      sampleCount start = track->TimeToLongSamples(t0);
      double rate = track->GetRate();
      float buffer[analyzeSize];
      float freq[windowSize/2];
      float freqa[windowSize/2];
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
      lag = (windowSize/2 - 1) - argmax;
      m_FromFrequency = rate / lag;
      m_ToFrequency = (m_FromFrequency * (100.0 + m_PercentChange)) / 100.0;

      // Now we can set the pitch control values. 
      m_FromPitchIndex = PitchIndex(FreqToMIDInoteNumber(m_FromFrequency));
      m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
      m_ToPitchIndex = PitchIndex(FreqToMIDInoteNumber(m_ToFrequency));
   }
}

bool EffectChangePitch::PromptUser()
{
   this->DeduceFrequencies(); // Set frequency-related control values based on sample.

   ChangePitchDialog dlog(this, mParent);
   dlog.m_FromPitchIndex = m_FromPitchIndex;
   dlog.m_bWantPitchDown = m_bWantPitchDown;
   dlog.m_ToPitchIndex = m_ToPitchIndex;
   dlog.m_SemitonesChange = m_SemitonesChange;
   dlog.m_FromFrequency = m_FromFrequency;
   dlog.m_ToFrequency = m_ToFrequency;
   dlog.m_PercentChange = m_PercentChange;
   // Don't need to call TransferDataToWindow, although other 
   //	Audacity dialogs (from which I derived this one) do it, because 
   //	ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog, 
   //	which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

  if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   m_FromPitchIndex = dlog.m_FromPitchIndex;
   m_bWantPitchDown = dlog.m_bWantPitchDown;
   m_ToPitchIndex = dlog.m_ToPitchIndex;
   m_SemitonesChange = dlog.m_SemitonesChange;
   m_FromFrequency = dlog.m_FromFrequency;
   m_ToFrequency = dlog.m_ToFrequency;
   m_PercentChange = dlog.m_PercentChange;
   return true;
}

bool EffectChangePitch::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferDouble(wxT("Percentage"),m_PercentChange,0.0);
   m_SemitonesChange = (12.0 * log((100.0 + m_PercentChange) / 100.0)) / log(2.0);
   return true;
}

bool EffectChangePitch::Process()
{
   mSoundTouch = new SoundTouch();
   SetTimeWarper(new IdentityTimeWarper());
   mSoundTouch->setPitchSemiTones((float)(m_SemitonesChange));
#ifdef USE_MIDI
   // Note: m_SemitonesChange is private to ChangePitch because it only
   // needs to pass it along to mSoundTouch (above). I added mSemitones
   // to SoundTouchEffect (the super class) to convey this value
   // to process Note tracks. This approach minimizes changes to existing 
   // code, but it would be cleaner to change all m_SemitonesChange to
   // mSemitones, make mSemitones exist with or without USE_MIDI, and 
   // eliminate the next line:
   mSemitones = m_SemitonesChange;
#endif
   return this->EffectSoundTouch::Process();
}

//----------------------------------------------------------------------------
// ChangePitchDialog
//----------------------------------------------------------------------------

#define PERCENTCHANGE_MIN -99
#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

enum {
   ID_TEXT_PERCENTCHANGE = 10001,
   ID_SLIDER_PERCENTCHANGE,
   ID_CHOICE_FROMPITCH,
   ID_RADIOBUTTON_PITCHUPDOWN,
   ID_CHOICE_TOPITCH,
   ID_TEXT_SEMITONESCHANGE,
   ID_TEXT_FROMFREQUENCY,
   ID_TEXT_TOFREQUENCY
};

// event table for ChangePitchDialog

BEGIN_EVENT_TABLE(ChangePitchDialog, EffectDialog)
   EVT_CHOICE(ID_CHOICE_FROMPITCH, ChangePitchDialog::OnChoice_FromPitch)
   EVT_RADIOBUTTON(ID_RADIOBUTTON_PITCHUPDOWN, ChangePitchDialog::OnRadioButton_PitchUpDown)
   EVT_CHOICE(ID_CHOICE_TOPITCH, ChangePitchDialog::OnChoice_ToPitch)

   EVT_TEXT(ID_TEXT_SEMITONESCHANGE, ChangePitchDialog::OnText_SemitonesChange)

   EVT_TEXT(ID_TEXT_FROMFREQUENCY, ChangePitchDialog::OnText_FromFrequency)
   EVT_TEXT(ID_TEXT_TOFREQUENCY, ChangePitchDialog::OnText_ToFrequency)

   EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangePitchDialog::OnText_PercentChange)
   EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangePitchDialog::OnSlider_PercentChange)

   EVT_BUTTON(ID_EFFECT_PREVIEW, ChangePitchDialog::OnPreview)
END_EVENT_TABLE()

ChangePitchDialog::ChangePitchDialog(EffectChangePitch *effect, wxWindow *parent)
:  EffectDialog(parent, _("Change Pitch"), PROCESS_EFFECT),
   mEffect(effect)
{
   m_bLoopDetect = false;

   // NULL out these control members because there are some cases where the 
   // event table handlers get called during this method, and those handlers that 
   // can cause trouble check for NULL.
   m_pChoice_FromPitch = NULL;
   m_pRadioButton_PitchUp = NULL;
   m_pRadioButton_PitchDown = NULL;
   m_pChoice_ToPitch = NULL;
   
   m_pTextCtrl_SemitonesChange = NULL;

   m_pTextCtrl_FromFrequency = NULL;
   m_pTextCtrl_ToFrequency = NULL;
   
   m_pTextCtrl_PercentChange = NULL;
   m_pSlider_PercentChange = NULL;

   // effect parameters
   m_FromPitchIndex = -1;		// -1 => uninitialized
   m_bWantPitchDown = false;
   m_ToPitchIndex = -1;			// -1 => uninitialized

   m_SemitonesChange = 0.0;

   m_FromFrequency = 0.0;		// 0.0 => uninitialized
   m_ToFrequency = 0.0;			// 0.0 => uninitialized

   m_PercentChange = 0.0;

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

   S.SetBorder(10);
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("Change Pitch without Changing Tempo") +
                 wxString(wxT("\n\n")) +
                 _("by Vaughan Johnson && Dominic Mazzoni") +
                 wxString(wxT("\n")) +
                 _("using SoundTouch, by Olli Parviainen"));
   }
   S.EndHorizontalLay();
   S.SetBorder(5);

   //
   S.StartMultiColumn(6, wxCENTER);
   {
      /* i18n-hint: (noun) Musical pitch.*/
      S.AddUnits(_("Pitch:"));

      S.StartHorizontalLay(wxALIGN_CENTER_VERTICAL);
      {
         m_pChoice_FromPitch = S.Id(ID_CHOICE_FROMPITCH)
            .AddChoice(_("From:"), wxT(""), &pitch);
         m_pChoice_FromPitch->SetName(_("From Pitch"));
         m_pChoice_FromPitch->SetSizeHints(100, -1);
      }
      S.EndHorizontalLay();

      S.StartStatic(wxT(""));
      {
         S.StartVerticalLay();
         {
            S.SetBorder(3);
            m_pRadioButton_PitchUp = S.Id(ID_RADIOBUTTON_PITCHUPDOWN)
               .AddRadioButton(_("Up"));
         
            m_pRadioButton_PitchDown = S.Id(ID_RADIOBUTTON_PITCHUPDOWN)
               .AddRadioButtonToGroup(_("Down"));
            S.SetBorder(5);
         }
         S.EndVerticalLay();
      }
      S.EndStatic();

      S.StartHorizontalLay(wxALIGN_CENTER_VERTICAL);
      {
         m_pChoice_ToPitch = S.Id(ID_CHOICE_TOPITCH)
            .AddChoice(_("To:"), wxT(""), &pitch);
         m_pChoice_ToPitch->SetName(_("To Pitch"));
         m_pChoice_ToPitch->SetSizeHints(100, -1);
      }
      S.EndHorizontalLay();
   }
   S.EndMultiColumn();

   //
   S.StartMultiColumn(2, wxCENTER);
   {
      //
      S.AddPrompt(_("Semitones (half-steps):"));
      S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, false);
      {
         m_pTextCtrl_SemitonesChange = S.Id(ID_TEXT_SEMITONESCHANGE)
            .AddTextBox(wxT(""), wxT(""), 12);
         m_pTextCtrl_SemitonesChange->SetName(_("Semitones in half-steps"));
         m_pTextCtrl_SemitonesChange->SetValidator(nonNegNumValidator);
      }
      S.EndHorizontalLay();

      //
      S.AddPrompt(_("Frequency (Hz):"));
      S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, false);
      {
         m_pTextCtrl_FromFrequency = S.Id(ID_TEXT_FROMFREQUENCY)
            .AddTextBox(_("from"), wxT(""), 12);
         m_pTextCtrl_FromFrequency->SetName(_("From frequency in hertz"));
         m_pTextCtrl_FromFrequency->SetValidator(nullvld);

         m_pTextCtrl_ToFrequency = S.Id(ID_TEXT_TOFREQUENCY)
            .AddTextBox(_("to"), wxT(""), 12);
         m_pTextCtrl_ToFrequency->SetName(_("To frequency in seconds"));
         m_pTextCtrl_ToFrequency->SetValidator(nonNegNumValidator);
      }
      S.EndHorizontalLay();

      //
      S.AddPrompt(_("Percent Change:"));
      S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, false);
      {
         m_pTextCtrl_PercentChange = S.Id(ID_TEXT_PERCENTCHANGE)
            .AddTextBox(wxT(""), wxT(""), 12);
         m_pTextCtrl_PercentChange->SetName(_("Percent Change"));
         m_pTextCtrl_PercentChange->SetValidator(numvld);
      }
      S.EndHorizontalLay();
   }
   S.EndMultiColumn();

   //
   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetStyle(wxSL_HORIZONTAL);
      m_pSlider_PercentChange = S.Id(ID_SLIDER_PERCENTCHANGE)
         .AddSlider(wxT(""), 0, (int)PERCENTCHANGE_MAX, (int)PERCENTCHANGE_MIN);
      m_pSlider_PercentChange->SetName(_("Percent Change"));
   }
   S.EndHorizontalLay();

   return;
}

bool ChangePitchDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

   // from/to pitch controls
   if (m_pChoice_FromPitch) 
      m_pChoice_FromPitch->SetSelection(m_FromPitchIndex);

   this->Update_RadioButton_PitchUpDown();
   this->Update_Choice_ToPitch();


   // semitones change control
   this->Update_Text_SemitonesChange();


   // from/to frequency controls
   if (m_pTextCtrl_FromFrequency) {
      wxString str;
      if (m_FromFrequency > 0.0)
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
      m_FromPitchIndex = m_pChoice_FromPitch->GetSelection(); 

   if (m_pRadioButton_PitchUp)
      m_bWantPitchDown = (m_pRadioButton_PitchUp->GetValue() == false);

   if (m_pChoice_ToPitch) 
      m_ToPitchIndex = m_pChoice_ToPitch->GetSelection();


   // semitones change control
   if (m_pTextCtrl_SemitonesChange) {
      str = m_pTextCtrl_SemitonesChange->GetValue();
      str.ToDouble(&newDouble);
      m_SemitonesChange = newDouble;
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
      m_PercentChange = newDouble;
   }

   // Ignore Slider_PercentChange because TextCtrl_PercentChange 
   // always tracks it & is more precise (decimal points).


   return true;
}


// calculations

void ChangePitchDialog::Calc_ToFrequency()
{
   m_ToFrequency = (m_FromFrequency * (100.0 + m_PercentChange)) / 100.0;
}

void ChangePitchDialog::Calc_ToPitchIndex()
{
   m_ToPitchIndex = (m_FromPitchIndex + 
                     (int)(m_SemitonesChange + 
                           // Round in the right direction.
                           ((m_bWantPitchDown ? -1.0 : 1.0) * 0.5))) 
                     % 12;
}

void ChangePitchDialog::Calc_SemitonesChange_fromPitches()
{
   int sign = m_bWantPitchDown ? -1 : 1;
   m_SemitonesChange = sign * (((sign * (m_ToPitchIndex - m_FromPitchIndex)) + 12) % 12); 
}

void ChangePitchDialog::Calc_SemitonesChange_fromPercentChange()
{
   // Use m_PercentChange rather than m_FromFrequency & m_ToFrequency, because 
   // they start out uninitialized, but m_PercentChange is always valid.
   m_SemitonesChange = (12.0 * log((100.0 + m_PercentChange) / 100.0)) / log(2.0);
}

void ChangePitchDialog::Calc_PercentChange()
{
   m_PercentChange = 100.0 * (pow(2.0, (m_SemitonesChange / 12.0)) - 1.0);
}


// handlers

void ChangePitchDialog::OnChoice_FromPitch(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pChoice_FromPitch) {
      m_FromPitchIndex = m_pChoice_FromPitch->GetSelection();

      this->Calc_ToPitchIndex();

      m_bLoopDetect = true;
      this->Update_Choice_ToPitch();
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnRadioButton_PitchUpDown(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pRadioButton_PitchUp) {
      m_bWantPitchDown = (m_pRadioButton_PitchUp->GetValue() == false);

      this->Calc_SemitonesChange_fromPitches();
      this->Calc_PercentChange(); // Call *after* m_SemitonesChange is updated.
      this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.

      m_bLoopDetect = true;
      this->Update_Text_SemitonesChange();
      this->Update_Text_ToFrequency();
      this->Update_Text_PercentChange();
      this->Update_Slider_PercentChange();
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnChoice_ToPitch(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pChoice_ToPitch) {
      m_ToPitchIndex = m_pChoice_ToPitch->GetSelection();

      this->Calc_SemitonesChange_fromPitches();
      this->Calc_PercentChange(); // Call *after* m_SemitonesChange is updated.
      this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.

      m_bLoopDetect = true;
      this->Update_Text_SemitonesChange();
      this->Update_Text_ToFrequency();
      this->Update_Text_PercentChange();
      this->Update_Slider_PercentChange();
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnText_SemitonesChange(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_SemitonesChange) {
      wxString str = m_pTextCtrl_SemitonesChange->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
      m_SemitonesChange = newValue;

      this->Calc_PercentChange();
      this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.
      m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
      this->Calc_ToPitchIndex(); // Call *after* m_bWantPitchDown is updated.

      m_bLoopDetect = true;
      this->Update_RadioButton_PitchUpDown();
      if (m_pTextCtrl_SemitonesChange->IsModified())
         // See note at implementation of Update_RadioButton_PitchUpDown.
         m_pTextCtrl_SemitonesChange->SetFocus(); 
      this->Update_Choice_ToPitch();
      this->Update_Text_ToFrequency();
      this->Update_Text_PercentChange();
      this->Update_Slider_PercentChange();
      m_bLoopDetect = false;
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
      m_FromFrequency = newDouble;

      m_FromPitchIndex = PitchIndex(FreqToMIDInoteNumber(m_FromFrequency));
      this->Calc_ToFrequency();
      m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
      this->Calc_ToPitchIndex(); // Call *after* m_bWantPitchDown is updated.

      m_bLoopDetect = true;
      this->Update_RadioButton_PitchUpDown();
      if (m_pTextCtrl_FromFrequency->IsModified())
         // See note at implementation of Update_RadioButton_PitchUpDown.
         m_pTextCtrl_FromFrequency->SetFocus(); 
      this->Update_Choice_ToPitch();
      this->Update_Text_ToFrequency();
      m_bLoopDetect = false;
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
      m_ToFrequency = newDouble;

      m_PercentChange = (((double)(m_ToFrequency) * 100.0) / 
                           (double)(m_FromFrequency)) - 100.0;

      this->Calc_SemitonesChange_fromPercentChange();
      this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
      m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

      m_bLoopDetect = true;
      this->Update_RadioButton_PitchUpDown();
      if (m_pTextCtrl_ToFrequency->IsModified())
         // See note at implementation of Update_RadioButton_PitchUpDown.
         m_pTextCtrl_ToFrequency->SetFocus(); 
      this->Update_Choice_ToPitch();
      this->Update_Text_SemitonesChange();
      this->Update_Text_PercentChange();
      this->Update_Slider_PercentChange();
      m_bLoopDetect = false;
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
      m_PercentChange = newValue;

      this->Calc_SemitonesChange_fromPercentChange();
      this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
      this->Calc_ToFrequency();
      m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

      m_bLoopDetect = true;
      this->Update_RadioButton_PitchUpDown();
      if (m_pTextCtrl_PercentChange->IsModified())
         // See note at implementation of Update_RadioButton_PitchUpDown.
         m_pTextCtrl_PercentChange->SetFocus(); 
      this->Update_Choice_ToPitch();
      this->Update_Text_SemitonesChange();
      this->Update_Text_ToFrequency();
      this->Update_Slider_PercentChange();
      m_bLoopDetect = false;

      //v Probably better to override wxTextValidator to disallow negative values.
      // See comment in ChangePitchDialog::ChangePitchDialog.
      this->FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangePitchDialog::OnSlider_PercentChange(wxCommandEvent & WXUNUSED(event))
{
   if (m_bLoopDetect)
      return;

   if (m_pSlider_PercentChange) {
      m_PercentChange = (double)(m_pSlider_PercentChange->GetValue()); 
      // Warp positive values to actually go up faster & further than negatives.
      if (m_PercentChange > 0.0)
         m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);

      this->Calc_SemitonesChange_fromPercentChange();
      this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
      this->Calc_ToFrequency();
      m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

      m_bLoopDetect = true;
      this->Update_RadioButton_PitchUpDown();
      this->Update_Choice_ToPitch();
      this->Update_Text_SemitonesChange();
      this->Update_Text_ToFrequency();
      this->Update_Text_PercentChange();
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   double oldSemitonesChange = m_SemitonesChange;
   if( m_PercentChange < -99.0)
   {
      m_PercentChange = -99.0;
      this->Update_Text_PercentChange();
   }
   mEffect->m_SemitonesChange = m_SemitonesChange;
   mEffect->Preview();
   mEffect->m_SemitonesChange = oldSemitonesChange;
}

// helper fns

// NOTE: wxWidgets ref (C:\wxWidgets_2.4.0\docs\htmlhelp) says 
// wxRadioButton::SetSelection "does not cause a 
// wxEVT_COMMAND_RadioButton_SELECTED event to get emitted", but it 
// calls SetFocus, which sure as heck DOES select the radio button.
//
// So, any wxTextCtrl handler that calls Update_RadioButton_PitchUpDown 
// needs to call wxTextCtrl::SetFocus afterward, to return the 
// focus to the wxTextCtrl so the user can keep typing.
//
// Also, it turns out the wxTextCtrl handlers are sometimes 
// called before the dialog is displayed, so those SetFocus calls 
// need to be conditionalized on wxTextCtrl::IsModified.
void ChangePitchDialog::Update_RadioButton_PitchUpDown() 
{
   if (m_pRadioButton_PitchUp) {
      m_pRadioButton_PitchUp->SetValue(m_bWantPitchDown == false);
      m_pRadioButton_PitchDown->SetValue(m_bWantPitchDown == true);
   }
}

void ChangePitchDialog::Update_Choice_ToPitch() 
{
   if (m_pChoice_ToPitch) 
      m_pChoice_ToPitch->SetSelection(m_ToPitchIndex);
}


void ChangePitchDialog::Update_Text_SemitonesChange()
{
   if (m_pTextCtrl_SemitonesChange) {
      wxString str;
      str.Printf(wxT("%.2f"), m_SemitonesChange);
      m_pTextCtrl_SemitonesChange->SetValue(str);
   }
}

void ChangePitchDialog::Update_Text_ToFrequency() 
{
   if (m_pTextCtrl_ToFrequency) {
      wxString str;
      if (m_ToFrequency > 0.0)
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
      str.Printf(wxT("%.3f"), m_PercentChange);
      m_pTextCtrl_PercentChange->SetValue(str);
      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangePitchDialog::Update_Slider_PercentChange()
{
   if (m_pSlider_PercentChange) {
      double unwarped = m_PercentChange;
      if (unwarped > 0.0)
         // Un-warp values above zero to actually go up to PERCENTCHANGE_MAX.
         unwarped = pow(m_PercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));

      // Add 0.5 to unwarped so trunc -> round.
      m_pSlider_PercentChange->SetValue((int)(unwarped + 0.5)); 
   }
}


#endif // USE_SOUNDTOUCH

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0b070f91-579c-4b57-bc29-82ceb6775355

