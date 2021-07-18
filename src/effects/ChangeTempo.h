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

class wxSlider;
class wxCheckBox;
class wxTextCtrl;
class ShuttleGui;

class EffectChangeTempo final : public EffectSoundTouch
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectChangeTempo();
   virtual ~EffectChangeTempo();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   bool SupportsAutomation() override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   bool Init() override;
   bool CheckWhetherSkipEffect() override;
   bool Process() override;
   double CalcPreviewInputLength(double previewLength) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

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

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_CHANGETEMPO__

#endif // USE_SOUNDTOUCH
