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

#include <wx/choice.h>
#include <wx/event.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"

#include "SoundTouchEffect.h"

#define CHANGEPITCH_PLUGIN_SYMBOL XO("Change Pitch")

class EffectChangePitch : public EffectSoundTouch
{
public:
   EffectChangePitch();
   virtual ~EffectChangePitch();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool LoadFactoryDefaults();

   // Effect implementation

   virtual bool Init();
   virtual bool Process();
   virtual bool CheckWhetherSkipEffect();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectChangePitch implementation

   // Deduce m_FromFrequency from the samples at the beginning of
   // the selection. Then set some other params accordingly.
   virtual void DeduceFrequencies();

   // calculations
   void Calc_ToPitch(); // Update m_nToPitch from new m_dSemitonesChange.
   void Calc_ToOctave();
   void Calc_SemitonesChange_fromPitches();
   void Calc_SemitonesChange_fromOctaveChange();
   void Calc_SemitonesChange_fromPercentChange();
   void Calc_ToFrequency(); // Update m_ToFrequency from m_FromFrequency & m_dPercentChange.
   void Calc_PercentChange(); // Update m_dPercentChange based on new m_dSemitonesChange.

   // handlers
   void OnChoice_FromPitch(wxCommandEvent & evt);
   void OnSpin_FromOctave(wxCommandEvent & evt);
   void OnChoice_ToPitch(wxCommandEvent & evt);
   void OnSpin_ToOctave(wxCommandEvent & evt);

   void OnText_SemitonesChange(wxCommandEvent & evt);

   void OnText_FromFrequency(wxCommandEvent & evt);
   void OnText_ToFrequency(wxCommandEvent & evt);

   void OnText_PercentChange(wxCommandEvent & evt);
   void OnSlider_PercentChange(wxCommandEvent & evt);

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
   // effect parameters
   int    m_nFromPitch;          // per PitchIndex()
   int    m_nFromOctave;         // per PitchOctave()
   int    m_nToPitch;            // per PitchIndex()
   int    m_nToOctave;           // per PitchOctave()

   double m_FromFrequency;       // starting frequency of selection
   double m_ToFrequency;         // target frequency of selection

   double m_dSemitonesChange;    // how many semitones to change pitch
   double m_dStartFrequency;     // starting frequency of first 0.2s of selection
   double m_dPercentChange;      // percent change to apply to pitch
                                 // Slider is (-100, 200], but textCtrls can set higher.

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

   DECLARE_EVENT_TABLE();
};

#endif // __AUDACITY_EFFECT_CHANGEPITCH__

#endif // USE_SOUNDTOUCH
