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

#if USE_SBSMS
#include "SBSMSEffect.h"
#endif

#include "SoundTouchEffect.h"
#include "ShuttleAutomation.h"

class wxSlider;
class wxCheckBox;
class wxTextCtrl;
class ShuttleGui;

class EffectChangeTempo final : public EffectSoundTouch
{
public:
   static inline EffectChangeTempo *
   FetchParameters(EffectChangeTempo &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectChangeTempo();
   virtual ~EffectChangeTempo();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool SupportsAutomation() const override;

   // Effect implementation

   bool Init() override;
   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   double CalcPreviewInputLength(
      const EffectSettings &settings, double previewLength) const override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectChangeTempo implementation

   // handlers
   void OnText_PercentChange(wxCommandEvent & evt);
   void OnSlider_PercentChange(wxCommandEvent & evt);
   void OnText_FromBPM(wxCommandEvent & evt);
   void OnText_ToBPM(wxCommandEvent & evt);
   void OnText_ToLength(wxCommandEvent & evt);

   // helper fns
   void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
   void Update_Text_ToBPM(); // Use m_FromBPM & m_PercentChange to set NEW m_ToBPM & control.
   void Update_Text_ToLength(); // Use m_FromLength & m_PercentChange to set NEW m_ToLength & control.

private:
   wxWeakRef<wxWindow> mUIParent{};

   bool           mUseSBSMS;
   double         m_PercentChange;  // percent change to apply to tempo
                                    // -100% is meaningless, but sky's the upper limit
   double         m_FromBPM;        // user-set beats-per-minute. Zero means not yet set.
   double         m_ToBPM;          // Zero value means not yet set.
   double         m_FromLength;     // starting length of selection
   double         m_ToLength;       // target length of selection

   bool m_bLoopDetect;

   // controls
   wxTextCtrl *	m_pTextCtrl_PercentChange;
   wxSlider *		m_pSlider_PercentChange;
   wxTextCtrl *	m_pTextCtrl_FromBPM;
   wxTextCtrl *	m_pTextCtrl_ToBPM;
   wxTextCtrl *	m_pTextCtrl_FromLength;
   wxTextCtrl *	m_pTextCtrl_ToLength;

#if USE_SBSMS
   wxCheckBox *   mUseSBSMSCheckBox;
#endif

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Percentage{ &EffectChangeTempo::m_PercentChange,
   L"Percentage", 0.0,  -95.0,   3000.0,  1  };
static constexpr EffectParameter UseSBSMS{ &EffectChangeTempo::mUseSBSMS,
   L"SBSMS",     false, false,   true,    1  };
};

#endif // __AUDACITY_EFFECT_CHANGETEMPO__

#endif // USE_SOUNDTOUCH
