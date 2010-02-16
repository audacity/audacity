/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.h

  Vaughan Johnson, Dominic Mazzoni
  
  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGESPEED__
#define __AUDACITY_EFFECT_CHANGESPEED__

#include "Effect.h"
#include "../Resample.h"

#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/textctrl.h>

class EffectChangeSpeed : public Effect {

 public:
   EffectChangeSpeed();

   virtual wxString GetEffectName() {
      return wxString(_("Change Speed..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://audacityteam.org/namespace#PitchAndTempo"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("ChangeSpeed"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Changing Speed"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

 protected:
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool CheckWhetherSkipEffect() { return (m_PercentChange == 0.0); }
   virtual bool Process();

 private:
   bool ProcessOne(WaveTrack * t, sampleCount start, sampleCount end);
   bool ProcessLabelTrack(Track *t);

 private:
	// track related
   int    mCurTrackNum;
   double m_maxNewLength;
   double mCurT0;
   double mCurT1;

	// control values
   double	m_PercentChange;	// percent change to apply to tempo
										// -100% is meaningless, but sky's the upper limit.
										// Slider is (-100, 200], but textCtrls can set higher.
   int		m_FromVinyl;		// from standard vinyl speed (RPM)
   int		m_ToVinyl;			// to standard vinyl speed (RPM)
   double   mFactor;          // Scale factor calculated from percent change

friend class ChangeSpeedDialog;
};

//----------------------------------------------------------------------------
// ChangeSpeedDialog
//----------------------------------------------------------------------------

class ChangeSpeedDialog:public EffectDialog {
 public:
   ChangeSpeedDialog(EffectChangeSpeed * effect,
							wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

 private:
	// handlers
	void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);
   void OnChoice_FromVinyl(wxCommandEvent & event); 
   void OnChoice_ToVinyl(wxCommandEvent & event); 

   void OnPreview(wxCommandEvent &event);

	// helper fns
	void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
	void Update_Vinyl(); // Update Vinyl controls for new percent change.
	void Update_PercentChange(); // Update percent change controls for new Vinyl values.

 private:
	EffectChangeSpeed * mEffect;
	bool m_bLoopDetect;

   // controls
   wxTextCtrl *	m_pTextCtrl_PercentChange;
   wxSlider *		m_pSlider_PercentChange;
   wxChoice *		m_pChoice_FromVinyl;
   wxChoice *		m_pChoice_ToVinyl;

 public:
	// effect parameters
   double	m_PercentChange;	// percent change to apply to tempo
										// -100% is meaningless, but sky's the upper limit.
										// Slider is (-100, 200], but textCtrls can set higher.
   int		m_FromVinyl;		// from standard vinyl speed (RPM)
   int		m_ToVinyl;			// to standard vinyl speed (RPM)

 private:
   DECLARE_EVENT_TABLE()
};


#endif // __AUDACITY_EFFECT_CHANGESPEED__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 1b39309b-9855-4705-9637-6435a119be56

