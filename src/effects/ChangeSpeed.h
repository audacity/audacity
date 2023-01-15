/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.h

  Vaughan Johnson, Dominic Mazzoni

  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGESPEED__
#define __AUDACITY_EFFECT_CHANGESPEED__

#include "StatefulEffect.h"
#include "../ShuttleAutomation.h"

class wxSlider;
class wxChoice;
class wxTextCtrl;
class NumericTextCtrl;
class ShuttleGui;

class EffectChangeSpeed final : public StatefulEffect
{
public:
   static inline EffectChangeSpeed *
   FetchParameters(EffectChangeSpeed &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectChangeSpeed();
   virtual ~EffectChangeSpeed();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   OptionalMessage LoadFactoryDefaults(EffectSettings &settings)
      const override;
   OptionalMessage DoLoadFactoryDefaults(EffectSettings &settings);

   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;
   double CalcPreviewInputLength(const EffectContext &context,
      const EffectSettings &settings, double previewLength) const override;
   bool Init() override;
   bool Process(EffectContext &context,
      EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectChangeSpeed implementation

   bool ProcessOne(EffectContext &context, WaveTrack *t,
      sampleCount start, sampleCount end);
   bool ProcessLabelTrack(LabelTrack *t);

   // handlers
   void OnText_PercentChange(wxCommandEvent & evt);
   void OnText_Multiplier(wxCommandEvent & evt);
   void OnSlider_PercentChange(wxCommandEvent & evt);
   void OnChoice_Vinyl(wxCommandEvent & evt);
   void OnTimeCtrl_ToLength(wxCommandEvent & evt);
   void OnTimeCtrlUpdate(wxCommandEvent & evt);

   // helper functions
   void Update_Text_PercentChange();   // Update control per current m_PercentChange.
   void Update_Text_Multiplier();      // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
   void Update_Vinyl();                // Update Vinyl controls for NEW percent change.
   void Update_TimeCtrl_ToLength();    // Update target length controls for NEW percent change.
   void UpdateUI();                    // Enable / disable OK / preview.

private:
   wxWeakRef<wxWindow> mUIParent{};

   // track related
   int    mCurTrackNum;
   double mMaxNewLength;
   double mCurT0;
   double mCurT1;

   // control values
   double   m_PercentChange;  // percent change to apply to tempo
                              // -100% is meaningless, but sky's the upper limit.
                              // Slider is (-100, 200], but textCtrls can set higher.
   int      mFromVinyl;       // from standard vinyl speed (RPM) enum
   double   mFactor;          // scale factor calculated from percent change
   double   mFromLength;      // current selection length
   int      mTimeCtrlFormat;  // time control format index number
   double   mMultiplier;

   bool mbLoopDetect;

   // controls
   wxTextCtrl *      mpTextCtrl_PercentChange;
   wxTextCtrl *      mpTextCtrl_Multiplier;
   wxSlider *        mpSlider_PercentChange;
   wxChoice *        mpChoice_FromVinyl;
   wxChoice *        mpChoice_ToVinyl;
   NumericTextCtrl * mpFromLengthCtrl;
   NumericTextCtrl * mpToLengthCtrl;
   double mRate;

   // private effect parameters
   int      mToVinyl;         // to standard vinyl speed (rpm)
   double   mToLength;        // target length of selection
   NumericFormatSymbol mFormat;          // time control format

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Percentage{ &EffectChangeSpeed::m_PercentChange,
   L"Percentage",    0.0,  -99.0,   4900.0,  1  };
};

#endif // __AUDACITY_EFFECT_CHANGESPEED__
