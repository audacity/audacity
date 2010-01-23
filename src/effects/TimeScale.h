/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScale.h

  Clayton Otey

**********************************************************************/

#include "../Audacity.h"

#if USE_SBSMS

#ifndef __AUDACITY_EFFECT_TIMESCALE__
#define __AUDACITY_EFFECT_TIMESCALE__

#include "SBSMSEffect.h"

#include <wx/intl.h>
#include <wx/dialog.h>
#include <wx/slider.h>

class wxString;
class wxTextCtrl;

class EffectTimeScale : public EffectSBSMS {

 public:
   EffectTimeScale();

   virtual wxString GetEffectName() {
      return wxString(_("Sliding Time Scale/Pitch Shift..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
     std::set<wxString> result;
     result.insert(wxT("http://audacityteam.org/namespace#PitchAndTempo"));
     return result;
   }

   virtual wxString GetEffectIdentifier() {
     return wxString(wxT("TimeScale"));
   }

   virtual wxString GetEffectAction() {
     return wxString(_("Changing Tempo/Pitch"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool Init();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   virtual bool Process();
  
 private:
   double m_RateStart;
   double m_RateEnd;
   double m_HalfStepsStart;
   double m_HalfStepsEnd;
   bool m_PreAnalyze;

   friend class TimeScaleDialog;
};

//----------------------------------------------------------------------------
// TimeScaleDialog
//----------------------------------------------------------------------------

class TimeScaleDialog:public EffectDialog {
 public:
  TimeScaleDialog(EffectTimeScale * effect, 
		  wxWindow * parent);

  void PopulateOrExchange(ShuttleGui & S);
  bool TransferDataToWindow();
  bool TransferDataFromWindow();

 private:
  // handlers
  void OnText_RateStart(wxCommandEvent & event);
  void OnText_RateEnd(wxCommandEvent & event);
  void OnText_HalfStepsStart(wxCommandEvent & event);
  void OnText_HalfStepsEnd(wxCommandEvent & event);
  void OnSlider_RateStart(wxCommandEvent & event);
  void OnSlider_RateEnd(wxCommandEvent & event);
  void OnCheckBox_PreAnalyze(wxCommandEvent & event);

  // helper fns
  bool CheckParameters();
  void Update_Text_RateStart();
  void Update_Text_RateEnd();
  void Update_Text_HalfStepsStart();
  void Update_Text_HalfStepsEnd();
  void Update_Slider_RateStart();
  void Update_Slider_RateEnd();
  void Update_CheckBox_PreAnalyze();

 private:
  EffectTimeScale *mEffect;
  bool m_bLoopDetect;
  
  // controls
  wxTextCtrl *m_pTextCtrl_RateStart;
  wxSlider *m_pSlider_RateStart;
  wxTextCtrl *m_pTextCtrl_RateEnd;
  wxSlider *m_pSlider_RateEnd;
  wxTextCtrl *m_pTextCtrl_HalfStepsStart;
  wxTextCtrl *m_pTextCtrl_HalfStepsEnd;
  wxCheckBox *m_pCheckBox_PreAnalyze;

 public:
   double m_RateStart;
   double m_RateEnd;
   double m_HalfStepsStart;
   double m_HalfStepsEnd;
   bool m_PreAnalyze;

 private:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_TIMESCALE

#endif // USE_SBSMS
