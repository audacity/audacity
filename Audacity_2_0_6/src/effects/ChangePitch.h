/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

  ChangePitch.h
  Vaughan Johnson, Dominic Mazzoni, Steve Daulton

******************************************************************//**

\file ChangePitch.h
\brief Change Pitch effect provides raising or lowering
the pitch without changing the tempo.

*//*******************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGEPITCH__
#define __AUDACITY_EFFECT_CHANGEPITCH__

#include "SoundTouchEffect.h"

#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>

class EffectChangePitch : public EffectSoundTouch
{
 public:
   EffectChangePitch();

   virtual wxString GetEffectName() { return wxString(_("Change Pitch...")); }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#PitchPlugin"));
      result.insert(wxT("http://audacityteam.org/namespace#PitchAndTempo"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("ChangePitch"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Changing Pitch"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription();

   virtual bool Init();

   // Deduce m_FromFrequency from the samples at the beginning of
   // the selection. Then set some other params accordingly.
   virtual void DeduceFrequencies();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool CheckWhetherSkipEffect() { return (m_dPercentChange == 0.0); }
   virtual bool Process();

private:
   double m_dSemitonesChange;   // how many semitones to change pitch
   double m_dStartFrequency;    // starting frequency of first 0.2s of selection
   double m_dPercentChange;     // percent change to apply to frequency

friend class ChangePitchDialog;
};

//----------------------------------------------------------------------------
// ChangePitchDialog
//----------------------------------------------------------------------------

class wxChoice;
class wxRadioButton;
class wxString;
class wxTextCtrl;

class ChangePitchDialog : public EffectDialog
{
 public:
   ChangePitchDialog(EffectChangePitch * effect, wxWindow * parent,
                     double dSemitonesChange, double dStartFrequency);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   // calculations
   void Calc_ToPitch(); // Update m_nToPitch from new m_dSemitonesChange.
   void Calc_ToOctave();
   void Calc_SemitonesChange_fromPitches();
   void Calc_SemitonesChange_fromOctaveChange();
   void Calc_SemitonesChange_fromPercentChange();
   void Calc_ToFrequency(); // Update m_ToFrequency from m_FromFrequency & m_dPercentChange.
   void Calc_PercentChange(); // Update m_dPercentChange based on new m_dSemitonesChange.

   // handlers
   void OnChoice_FromPitch(wxCommandEvent & event);
   void OnSpin_FromOctave(wxCommandEvent & event);
   void OnChoice_ToPitch(wxCommandEvent & event);
   void OnSpin_ToOctave(wxCommandEvent & event);

   void OnText_SemitonesChange(wxCommandEvent & event);

   void OnText_FromFrequency(wxCommandEvent & event);
   void OnText_ToFrequency(wxCommandEvent & event);

   void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);

   void OnPreview( wxCommandEvent &event );

   // helper fns for controls
   void Update_Choice_FromPitch();
   void Update_Spin_FromOctave();
   void Update_Choice_ToPitch();
   void Update_Spin_ToOctave();

   void Update_Text_SemitonesChange();

   void Update_Text_FromFrequency();
   void Update_Text_ToFrequency();

   void Update_Text_PercentChange(); // Update control per current m_dPercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_dPercentChange.

 private:
   EffectChangePitch * mEffect;
   bool m_bLoopDetect; // Used to avoid loops in initialization and in event handling.

   // controls
   wxChoice *     m_pChoice_FromPitch;
   wxSpinCtrl *   m_pSpin_FromOctave;
   wxChoice *     m_pChoice_ToPitch;
   wxSpinCtrl *   m_pSpin_ToOctave;
   wxTextCtrl *   m_pTextCtrl_SemitonesChange;

   wxTextCtrl *   m_pTextCtrl_FromFrequency;
   wxTextCtrl *   m_pTextCtrl_ToFrequency;
   wxTextCtrl *   m_pTextCtrl_PercentChange;
   wxSlider *     m_pSlider_PercentChange;

 public:
   // effect parameters
   int      m_nFromPitch;    // per PitchIndex()
   int      m_nFromOctave;   // per PitchOctave()
   int      m_nToPitch;      // per PitchIndex()
   int      m_nToOctave;     // per PitchOctave()

   double   m_dSemitonesChange;   // how many semitones to change pitch

   double   m_FromFrequency;     // starting frequency of selection
   double   m_ToFrequency;       // target frequency of selection

   double   m_dPercentChange;     // percent change to apply to pitch
                                 // Slider is (-100, 200], but textCtrls can set higher.

 private:
   DECLARE_EVENT_TABLE()
};


#endif // __AUDACITY_EFFECT_CHANGEPITCH__

#endif // USE_SOUNDTOUCH
