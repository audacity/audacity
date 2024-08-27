/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2 or later.  See License.txt.

  ChangePitch.h
  Vaughan Johnson, Dominic Mazzoni, Steve Daulton

******************************************************************//**

\file ChangePitch.h
\brief Change Pitch effect provides raising or lowering
the pitch without changing the tempo.

*//*******************************************************************/


#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGEPITCH__
#define __AUDACITY_EFFECT_CHANGEPITCH__

#if USE_SBSMS
#include "SBSMSEffect.h"
#endif

#include "SoundTouchBase.h"
#include "ShuttleAutomation.h"
#include <wx/weakref.h>

class wxSlider;
class wxChoice;
class wxCheckBox;
class wxTextCtrl;
class wxSpinCtrl;
class ShuttleGui;

class ChangePitchBase : public SoundTouchBase
{
public:
   static inline ChangePitchBase *
   FetchParameters(ChangePitchBase &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   ChangePitchBase();
   virtual ~ChangePitchBase();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   OptionalMessage LoadFactoryDefaults(EffectSettings &settings)
      const override;
   OptionalMessage DoLoadFactoryDefaults(EffectSettings &settings);

   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;

protected:
   // ChangePitchBase implementation

   // Deduce m_FromFrequency from the samples at the beginning of
   // the selection. Then set some other params accordingly.
   void DeduceFrequencies();

   // calculations
   void Calc_ToPitch(); // Update m_nToPitch from NEW m_dSemitonesChange.
   void Calc_ToOctave();
   void Calc_SemitonesChange_fromPitches();
   void Calc_SemitonesChange_fromOctaveChange();
   void Calc_SemitonesChange_fromPercentChange();
   void Calc_ToFrequency(); // Update m_ToFrequency from m_FromFrequency & m_dPercentChange.
   void Calc_PercentChange(); // Update m_dPercentChange based on NEW m_dSemitonesChange.

   bool mUseSBSMS;
   // effect parameters
   int    m_nFromPitch;          // per PitchIndex()
   int    m_nFromOctave;         // per PitchOctave()
   int    m_nToPitch;            // per PitchIndex()
   int    m_nToOctave;           // per PitchOctave()

   double m_FromFrequency;       // starting frequency of selection
   double m_ToFrequency;         // target frequency of selection

   double m_dSemitonesChange;    // how many semitones to change pitch
   double m_dStartFrequency;     // starting frequency of first 0.2s of selection
   double m_dPercentChange;      // percent change to apply to pitch
                                 // Slider is (-100, 200], but textCtrls can set higher.

   bool m_bLoopDetect; // Used to avoid loops in initialization and in event handling.

   const EffectParameterMethods& Parameters() const override;

static constexpr EffectParameter Percentage{ &ChangePitchBase::m_dPercentChange,
   L"Percentage", 0.0,  -99.0,   3000.0,  1  };
static constexpr EffectParameter UseSBSMS{ &ChangePitchBase::mUseSBSMS,
   L"SBSMS",     false, false,   true,    1  };
};

class EffectChangePitch :
    public ChangePitchBase,
    public StatefulEffectUIServices
{
public:
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

   DECLARE_EVENT_TABLE()
private:
   // handlers
   void OnChoice_FromPitch(wxCommandEvent & evt);
   void OnSpin_FromOctave(wxCommandEvent & evt);
   void OnChoice_ToPitch(wxCommandEvent & evt);
   void OnSpin_ToOctave(wxCommandEvent & evt);

   void OnText_SemitonesChange(wxCommandEvent & evt);

   void OnText_FromFrequency(wxCommandEvent & evt);
   void OnText_ToFrequency(wxCommandEvent & evt);

   void OnText_PercentChange(wxCommandEvent & evt);
   void OnSlider_PercentChange(wxCommandEvent & evt);

   // helper fns for controls
   void Update_Choice_FromPitch();
   void Update_Spin_FromOctave();
   void Update_Choice_ToPitch();
   void Update_Spin_ToOctave();

   void Update_Text_SemitonesChange();

   void Update_Text_FromFrequency();
   void Update_Text_ToFrequency();

   void Update_Text_PercentChange(); // Update control per current m_dPercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_dPercentChange.

   wxWeakRef<wxWindow> mUIParent{};

   // controls
   wxChoice* m_pChoice_FromPitch = nullptr;
   wxSpinCtrl* m_pSpin_FromOctave = nullptr;
   wxChoice* m_pChoice_ToPitch = nullptr;
   wxSpinCtrl* m_pSpin_ToOctave = nullptr;
   wxTextCtrl* m_pTextCtrl_SemitonesChange = nullptr;

   wxTextCtrl* m_pTextCtrl_FromFrequency = nullptr;
   wxTextCtrl* m_pTextCtrl_ToFrequency = nullptr;
   wxTextCtrl* m_pTextCtrl_PercentChange = nullptr;
   wxSlider* m_pSlider_PercentChange = nullptr;

#if USE_SBSMS
   wxCheckBox* mUseSBSMSCheckBox = nullptr;
#endif
};

#   endif // __AUDACITY_EFFECT_CHANGEPITCH__

#endif // USE_SOUNDTOUCH
