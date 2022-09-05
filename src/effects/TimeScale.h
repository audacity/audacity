/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeScale.h

  Clayton Otey

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TIMESCALE__
#define __AUDACITY_EFFECT_TIMESCALE__



#if USE_SBSMS

#include "SBSMSEffect.h"
#include "../ShuttleAutomation.h"

class wxSlider;
class wxTextCtrl;
class ShuttleGui;

class EffectTimeScale final : public EffectSBSMS
{
public:
   static inline EffectTimeScale *
   FetchParameters(EffectTimeScale &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectTimeScale();
   virtual ~EffectTimeScale();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   std::any BeginPreview(const EffectSettings &settings) override;
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;
   double CalcPreviewInputLength(const EffectContext &context,
      const EffectSettings &settings, double previewLength) const override;

private:
   // EffectTimeScale implementation

   static inline double PercentChangeToRatio(double percentChange);
   static inline double HalfStepsToPercentChange(double halfSteps);
   static inline double PercentChangeToHalfSteps(double percentChange);

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

   wxWeakRef<wxWindow> mUIParent{};

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

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter RatePercentStart{ &EffectTimeScale::m_RatePercentChangeStart,
   L"RatePercentChangeStart",  0.0,  -90.0,   500,   1  };
static constexpr EffectParameter RatePercentEnd{ &EffectTimeScale::m_RatePercentChangeEnd,
   L"RatePercentChangeEnd",    0.0,  -90.0,   500,   1  };
static constexpr EffectParameter HalfStepsStart{ &EffectTimeScale::m_PitchHalfStepsStart,
   L"PitchHalfStepsStart",     0.0,  -12.0,   12.0,  1  };
static constexpr EffectParameter HalfStepsEnd{ &EffectTimeScale::m_PitchHalfStepsEnd,
   L"PitchHalfStepsEnd",       0.0,  -12.0,   12.0,  1  };
static constexpr EffectParameter PitchPercentStart{ &EffectTimeScale::m_PitchPercentChangeStart,
   L"PitchPercentChangeStart", 0.0,  -50.0,   100.0, 1  };
static constexpr EffectParameter PitchPercentEnd{ &EffectTimeScale::m_PitchPercentChangeEnd,
   L"PitchPercentChangeEnd",   0.0,  -50.0,   100.0, 1  };
};

#endif // __AUDACITY_EFFECT_TIMESCALE

#endif // USE_SBSMS
