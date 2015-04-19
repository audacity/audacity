/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScale.h

  Clayton Otey

**********************************************************************/

#include "../Audacity.h"

#if USE_SBSMS

#ifndef __AUDACITY_EFFECT_TIMESCALE__
#define __AUDACITY_EFFECT_TIMESCALE__

#include <wx/event.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"

#include "SBSMSEffect.h"

#define TIMESCALE_PLUGIN_SYMBOL XO("Time Scale")

class EffectTimeScale : public EffectSBSMS
{
public:
   EffectTimeScale();
   virtual ~EffectTimeScale();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool Init();
   virtual bool Process();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectTimeScale implementation

   inline double PercentChangeToRatio(double percentChange);
   inline double HalfStepsToPercentChange(double halfSteps);
   inline double PercentChangeToHalfSteps(double percentChange);

   void OnText_RatePercentChangeStart(wxCommandEvent & evt);
   void OnText_RatePercentChangeEnd(wxCommandEvent & evt);
   void OnText_PitchPercentChangeStart(wxCommandEvent & evt);
   void OnText_PitchPercentChangeEnd(wxCommandEvent & evt);
   void OnText_PitchHalfStepsStart(wxCommandEvent & evt);
   void OnText_PitchHalfStepsEnd(wxCommandEvent & evt);
   void OnSlider_RatePercentChangeStart(wxCommandEvent & evt);
   void OnSlider_RatePercentChangeEnd(wxCommandEvent & evt);
   void OnCheckBox_PreAnalyze(wxCommandEvent & evt);

   void Update_Text_RatePercentChangeStart();
   void Update_Text_RatePercentChangeEnd();
   void Update_Text_PitchPercentChangeStart();
   void Update_Text_PitchPercentChangeEnd();
   void Update_Text_PitchHalfStepsStart();
   void Update_Text_PitchHalfStepsEnd();
   void Update_Slider_RatePercentChangeStart();
   void Update_Slider_RatePercentChangeEnd();

private:
   double m_RatePercentChangeStart;
   double m_RatePercentChangeEnd;
   double m_PitchHalfStepsStart;
   double m_PitchHalfStepsEnd;
   double m_PitchPercentChangeStart;
   double m_PitchPercentChangeEnd;

   wxTextCtrl *m_pTextCtrl_RatePercentChangeStart;
   wxTextCtrl *m_pTextCtrl_RatePercentChangeEnd;
   wxSlider *m_pSlider_RatePercentChangeStart;
   wxSlider *m_pSlider_RatePercentChangeEnd;
   wxTextCtrl *m_pTextCtrl_PitchHalfStepsStart;
   wxTextCtrl *m_pTextCtrl_PitchHalfStepsEnd;
   wxTextCtrl *m_pTextCtrl_PitchPercentChangeStart;
   wxTextCtrl *m_pTextCtrl_PitchPercentChangeEnd;

   DECLARE_EVENT_TABLE();
};

#endif // __AUDACITY_EFFECT_TIMESCALE

#endif // USE_SBSMS
