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
#include <wx/choice.h>

class wxString;
class wxArrayString;
class wxTextCtrl;
class wxCheckBox;
class wxChoice;

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
   int m_RateSlideType;
   int m_PitchSlideType;
   int m_RateSlideReference;
   int m_PitchSlideReference;
   bool m_LinkRatePitch;
   bool m_RateLinkInitialFinal;
   bool m_PitchLinkInitialFinal;

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
   void OnCheckBox_LinkRatePitch(wxCommandEvent & event);
   void OnChoice_RateSlideType(wxCommandEvent & event);
   void OnChoice_PitchSlideType(wxCommandEvent & event);
   void OnChoice_RateSlideReference(wxCommandEvent & event);
   void OnChoice_PitchSlideReference(wxCommandEvent & event);
   void OnCheckBox_RateLinkInitialFinal(wxCommandEvent & event);
   void OnCheckBox_PitchLinkInitialFinal(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);
   
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
   void Update_CheckBox_LinkRatePitch();
   void Update_Choice_RateSlideType();
   void Update_Choice_PitchSlideType();
   void Update_Choice_RateSlideReference();
   void Update_Choice_PitchSlideReference();
   void Update_CheckBox_RateLinkInitialFinal();
   void Update_CheckBox_PitchLinkInitialFinal();
   
 private:
   EffectTimeScale *mEffect;
   bool m_bLoopDetect;
   
   wxArrayString rateSlideTypes;
   wxArrayString pitchSlideTypes;
   wxArrayString rateSlideReferences;
   wxArrayString pitchSlideReferences;
   
   // controls
   wxTextCtrl *m_pTextCtrl_RatePercentChangeStart;
   wxTextCtrl *m_pTextCtrl_RatePercentChangeEnd;
   wxSlider *m_pSlider_RatePercentChangeStart;
   wxSlider *m_pSlider_RatePercentChangeEnd;
   wxTextCtrl *m_pTextCtrl_PitchHalfStepsStart;
   wxTextCtrl *m_pTextCtrl_PitchHalfStepsEnd;
   wxTextCtrl *m_pTextCtrl_PitchPercentChangeStart;
   wxTextCtrl *m_pTextCtrl_PitchPercentChangeEnd;
   wxCheckBox *m_pCheckBox_LinkRatePitch;
   wxChoice *m_pChoice_RateSlideType;
   wxChoice *m_pChoice_PitchSlideType;
   wxChoice *m_pChoice_RateSlideReference;
   wxChoice *m_pChoice_PitchSlideReference;
   wxCheckBox *m_pCheckBox_RateLinkInitialFinal;
   wxCheckBox *m_pCheckBox_PitchLinkInitialFinal;
   
   double m_RatePercentChangeStart;
   double m_RatePercentChangeEnd;
   double m_PitchHalfStepsStart;
   double m_PitchHalfStepsEnd;
   double m_PitchPercentChangeStart;
   double m_PitchPercentChangeEnd;
   bool m_LinkRatePitch;
   int m_RateSlideType;
   int m_PitchSlideType;
   int m_RateSlideReference;
   int m_PitchSlideReference;
   bool m_RateLinkInitialFinal;
   bool m_PitchLinkInitialFinal;

   friend class EffectTimeScale;
   
 private:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_TIMESCALE

#endif // USE_SBSMS
