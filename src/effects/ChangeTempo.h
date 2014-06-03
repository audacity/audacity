/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.h

  Vaughan Johnson, Dominic Mazzoni

  Change Tempo effect provides speeding up or
  slowing down tempo without changing pitch.

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGETEMPO__
#define __AUDACITY_EFFECT_CHANGETEMPO__

#include "SoundTouchEffect.h"

#include <wx/intl.h>
#include <wx/dialog.h>
#include <wx/slider.h>

class wxString;
class wxTextCtrl;


class EffectChangeTempo : public EffectSoundTouch {

 public:
   EffectChangeTempo();

   virtual wxString GetEffectName() {
      return wxString(_("Change Tempo..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://audacityteam.org/namespace#PitchAndTempo"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("ChangeTempo"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Changing Tempo"));
   }

   // Useful only after PromptUser values have been set.
   virtual wxString GetEffectDescription();

   virtual bool Init();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool CheckWhetherSkipEffect() { return (m_PercentChange == 0.0); }
   virtual bool Process();

   double CalcPreviewInputLength(double previewLength);

 private:
   double         m_PercentChange;  // percent change to apply to tempo
                                    // -100% is meaningless, but sky's the upper limit
   double         m_FromBPM;        // user-set beats-per-minute. Zero means not yet set.
   double         m_ToBPM;          // Zero value means not yet set.
   double         m_FromLength;     // starting length of selection
   double         m_ToLength;       // target length of selection

friend class ChangeTempoDialog;
};

//----------------------------------------------------------------------------
// ChangeTempoDialog
//----------------------------------------------------------------------------

class ChangeTempoDialog:public EffectDialog {
 public:
   ChangeTempoDialog(EffectChangeTempo * effect,
                     wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
   // handlers
   void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);
   void OnText_FromBPM(wxCommandEvent & event);
   void OnText_ToBPM(wxCommandEvent & event);
   void OnText_ToLength(wxCommandEvent & event);

   void OnPreview( wxCommandEvent &event );

   // helper fns
   void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
   void Update_Text_ToBPM(); // Use m_FromBPM & m_PercentChange to set new m_ToBPM & control.
   void Update_Text_ToLength(); // Use m_FromLength & m_PercentChange to set new m_ToLength & control.

 private:
   EffectChangeTempo * mEffect;
   bool m_bLoopDetect;

   // controls
   wxTextCtrl *	m_pTextCtrl_PercentChange;
   wxSlider *		m_pSlider_PercentChange;
   wxTextCtrl *	m_pTextCtrl_FromBPM;
   wxTextCtrl *	m_pTextCtrl_ToBPM;
   wxTextCtrl *	m_pTextCtrl_FromLength;
   wxTextCtrl *	m_pTextCtrl_ToLength;

 public:
   // effect parameters
   double         m_PercentChange;  // percent change to apply to tempo
                                    // -100% is meaningless, but sky's the upper limit.
                                    // Slider is (-100, 200], but textCtrls can set higher.
   double         m_FromBPM;        // user-set beats-per-minute. Zero means not yet set.
   double         m_ToBPM;          // Zero value means not yet set.
   double         m_FromLength;     // starting length of selection
   double         m_ToLength;       // target length of selection

 private:
   DECLARE_EVENT_TABLE()
};


#endif // __AUDACITY_EFFECT_CHANGETEMPO__

#endif // USE_SOUNDTOUCH
