/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.h

  Dominic Mazzoni

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AMPLIFY__
#define __AUDACITY_EFFECT_AMPLIFY__

#include "Effect.h"


class wxSlider;
class wxCheckBox;
class wxTextCtrl;
class ShuttleGui;

class EffectAmplify final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectAmplify();
   virtual ~EffectAmplify();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool GetAutomationParameters(CommandParameters & parms) const override;
   bool SetAutomationParameters(const CommandParameters & parms) override;
   bool LoadFactoryDefaults() override;

   // EffectProcessor implementation

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool VisitSettings( SettingsVisitor & S ) override;

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

private:
   double mPeak;

public: // TODO remove
   double mRatio;
   double mRatioClip;   // maximum value of mRatio which does not cause clipping
   double mAmp;
   double mNewPeak;
   bool mCanClip;

   wxSlider *mAmpS;
   wxTextCtrl *mAmpT;
   wxTextCtrl *mNewPeakT;
   wxCheckBox *mClip;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_AMPLIFY__
