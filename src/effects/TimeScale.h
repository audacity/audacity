/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScale.h

  Clayton Otey

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TIMESCALE__
#define __AUDACITY_EFFECT_TIMESCALE__



#if USE_SBSMS

#include "SBSMSEffect.h"

class wxSlider;
class wxTextCtrl;
class ShuttleGui;

class EffectTimeScale final : public EffectSBSMS
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectTimeScale();
   virtual ~EffectTimeScale();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   bool Init() override;
   void Preview(bool dryOnly) override;
   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;
   double CalcPreviewInputLength(double previewLength) override;

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
   bool bPreview;
   double previewSelectedDuration;
   SlideType slideTypeRate;
   SlideType slideTypePitch;
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

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_TIMESCALE

#endif // USE_SBSMS
