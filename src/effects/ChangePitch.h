/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangePitch.h

  Vaughan Johnson, Dominic Mazzoni
  
  Change Pitch effect provides raising or lowering 
  the pitch without changing the tempo.

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGEPITCH__
#define __AUDACITY_EFFECT_CHANGEPITCH__

#include "SoundTouchEffect.h"

#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>

class EffectChangePitch : public EffectSoundTouch {

 public:
   EffectChangePitch();

   virtual wxString GetEffectName() {
      return wxString(_("Change Pitch..."));
   }
   
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

	// DeduceFrequencies is Dominic's extremely cool trick (Vaughan sez so!) 
	// to set deduce m_FromFrequency from the samples at the beginning of 
	// the selection. Then we set some other params accordingly.
	virtual void DeduceFrequencies(); 

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool CheckWhetherSkipEffect() { return (m_PercentChange == 0.0); }
   virtual bool Process();
   
 private:
   int				m_FromPitchIndex;		// pitch index, per PitchIndex
	bool				m_bWantPitchDown;		// up to ToPitchNum if false (default), else down
   int				m_ToPitchIndex;		// pitch index, per PitchIndex

	double			m_SemitonesChange;	// how many semitones to change pitch
	
   float				m_FromFrequency;		// starting frequency of selection
   float				m_ToFrequency;			// target frequency of selection

   double			m_PercentChange;		// percent change to apply to pitch

friend class ChangePitchDialog;
};

//----------------------------------------------------------------------------
// ChangePitchDialog
//----------------------------------------------------------------------------

class wxChoice;
class wxRadioButton;
class wxString;
class wxTextCtrl;

class ChangePitchDialog:public EffectDialog {
 public:
   ChangePitchDialog(EffectChangePitch * effect, wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
	// calculations
	void Calc_ToFrequency(); // Update m_ToFrequency from m_FromFrequency & m_PercentChange.
	void Calc_ToPitchIndex(); // Update m_ToPitchIndex from new m_SemitonesChange.
	void Calc_SemitonesChange_fromPitches(); // Update m_SemitonesChange from new m_*PitchIndex-es.
	void Calc_SemitonesChange_fromPercentChange(); // Update m_SemitonesChange from new m_PercentChange.
	void Calc_PercentChange(); // Update m_PercentChange based on new m_SemitonesChange.

	// handlers
   void OnChoice_FromPitch(wxCommandEvent & event); 
	void OnRadioButton_PitchUpDown(wxCommandEvent & event);
   void OnChoice_ToPitch(wxCommandEvent & event); 

   void OnText_SemitonesChange(wxCommandEvent & event); 
   
	void OnText_FromFrequency(wxCommandEvent & event); 
   void OnText_ToFrequency(wxCommandEvent & event); 

	void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);

   void OnPreview( wxCommandEvent &event );

	// helper fns for controls
	void Update_RadioButton_PitchUpDown();
	void Update_Choice_ToPitch(); 

	void Update_Text_SemitonesChange(); 
	
	void Update_Text_ToFrequency(); 

	void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.

 private:
	EffectChangePitch * mEffect;
   bool m_bLoopDetect;

   // controls
   wxChoice *		m_pChoice_FromPitch;
	wxRadioButton *m_pRadioButton_PitchUp;
	wxRadioButton *m_pRadioButton_PitchDown;
   wxChoice *		m_pChoice_ToPitch;
   
   wxTextCtrl *	m_pTextCtrl_SemitonesChange;

	wxTextCtrl *	m_pTextCtrl_FromFrequency;
   wxTextCtrl *	m_pTextCtrl_ToFrequency;
   
	wxTextCtrl *	m_pTextCtrl_PercentChange;
   wxSlider *		m_pSlider_PercentChange;

 public:
	// effect parameters
   int		m_FromPitchIndex;		// pitch index, per PitchIndex
	bool		m_bWantPitchDown;		// up to ToPitchNum if false (default), else down
   int		m_ToPitchIndex;		// pitch index, per PitchIndex

	double	m_SemitonesChange;	// how many semitones to change pitch
	
   float		m_FromFrequency;		// starting frequency of selection
   float		m_ToFrequency;			// target frequency of selection

   double	m_PercentChange;		// percent change to apply to pitch
											// Slider is (-100, 200], but textCtrls can set higher.

 private:
   DECLARE_EVENT_TABLE()
};


#endif // __AUDACITY_EFFECT_CHANGEPITCH__

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
// arch-tag: a2885d66-6848-451b-aa61-3d54cec833d6

