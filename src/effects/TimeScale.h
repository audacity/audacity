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
   double m_RatePercentChangeStart;
   double m_RatePercentChangeEnd;
   double m_PitchHalfStepsStart;
   double m_PitchHalfStepsEnd;
   double m_PitchPercentChangeStart;
   double m_PitchPercentChangeEnd;
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
   void OnText_RatePercentChangeStart(wxCommandEvent & event);
   void OnText_RatePercentChangeEnd(wxCommandEvent & event);
   void OnText_PitchPercentChangeStart(wxCommandEvent & event);
   void OnText_PitchPercentChangeEnd(wxCommandEvent & event);
   void OnText_PitchHalfStepsStart(wxCommandEvent & event);
   void OnText_PitchHalfStepsEnd(wxCommandEvent & event);
   void OnSlider_RatePercentChangeStart(wxCommandEvent & event);
   void OnSlider_RatePercentChangeEnd(wxCommandEvent & event);
   void OnCheckBox_PreAnalyze(wxCommandEvent & event);

   // helper fns
   bool CheckParameters();
   void Update_Text_RatePercentChangeStart();
   void Update_Text_RatePercentChangeEnd();
   void Update_Text_PitchPercentChangeStart();
   void Update_Text_PitchPercentChangeEnd();
   void Update_Text_PitchHalfStepsStart();
   void Update_Text_PitchHalfStepsEnd();
   void Update_Slider_RatePercentChangeStart();
   void Update_Slider_RatePercentChangeEnd();
   void Update_CheckBox_PreAnalyze();

 private:
   EffectTimeScale *mEffect;
   bool m_bLoopDetect;

   // controls
   wxTextCtrl *m_pTextCtrl_RatePercentChangeStart;
   wxTextCtrl *m_pTextCtrl_RatePercentChangeEnd;
   wxSlider *m_pSlider_RatePercentChangeStart;
   wxSlider *m_pSlider_RatePercentChangeEnd;
   wxTextCtrl *m_pTextCtrl_PitchHalfStepsStart;
   wxTextCtrl *m_pTextCtrl_PitchHalfStepsEnd;
   wxTextCtrl *m_pTextCtrl_PitchPercentChangeStart;
   wxTextCtrl *m_pTextCtrl_PitchPercentChangeEnd;
   wxCheckBox *m_pCheckBox_PreAnalyze;

 public:
   double m_RatePercentChangeStart;
   double m_RatePercentChangeEnd;
   double m_PitchHalfStepsStart;
   double m_PitchHalfStepsEnd;
   double m_PitchPercentChangeStart;
   double m_PitchPercentChangeEnd;
   bool m_PreAnalyze;

 private:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_TIMESCALE

#endif // USE_SBSMS
