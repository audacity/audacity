/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.h

  Dominic Mazzoni

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AMPLIFY_SL__
#define __AUDACITY_EFFECT_AMPLIFY_SL__

#include "Effect.h"
#include "../ShuttleAutomation.h"


class wxSlider;
class wxCheckBox;
class wxTextCtrl;
class ShuttleGui;

class EffectAmplifySL final : public Effect
{
public:
   struct Settings;
   static inline Settings*
   FetchParameters(EffectAmplifySL&e, EffectSettings &){ return &e.mSettings; }
   static const ComponentInterfaceSymbol Symbol;

   EffectAmplifySL();
   virtual ~EffectAmplifySL();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool LoadFactoryDefaults(EffectSettings &settings) const override;
   bool DoLoadFactoryDefaults();

   // EffectProcessor implementation

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   // Effect implementation

   bool Init() override;
   void Preview(EffectSettingsAccess &access, bool dryOnly) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   void ClampRatio();

   // EffectAmplify implementation

   void OnAmpText(wxCommandEvent & evt);
   void OnPeakText(wxCommandEvent & evt);
   void OnAmpSlider(wxCommandEvent & evt);
   void OnClipCheckBox(wxCommandEvent & evt);
   void CheckClip();

public:

   struct Settings
   {
      double mRatio;
      double mAmp;
      bool   mCanClip;
   };

   struct State
   {
      double mPeak;
      double mRatioClip;   // maximum value of mRatio which does not cause clipping
      double mNewPeak;
   };

   struct Validator;

private:

   Settings mSettings;

   State mState;   

   wxSlider *mAmpS;
   wxTextCtrl *mAmpT;
   wxTextCtrl *mNewPeakT;
   wxCheckBox *mClip;

   const EffectParameterMethods& Parameters() const override;

   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Ratio{ &Settings::mRatio,
   L"Ratio",            0.9f,       0.003162f,  316.227766f,   1.0f  };
// Amp is not saved in settings!
static constexpr EffectParameter Amp{ &Settings::mAmp,
   L"",                -0.91515f,  -50.0f,     50.0f,         10.0f };
static constexpr EffectParameter Clipping{ &Settings::mCanClip,
   L"AllowClipping",    false,    false,  true,    1  };
};

#endif // __AUDACITY_EFFECT_AMPLIFY__
