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

#include <wx/checkbox.h>
#include <wx/event.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"

#define AMPLIFY_PLUGIN_SYMBOL XO("Amplify")

class ShuttleGui;

class EffectAmplify final : public Effect
{
public:
   EffectAmplify();
   virtual ~EffectAmplify();

   // IdentInterface implementation

   wxString GetSymbol() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   unsigned GetAudioOutCount() override;
   size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) override;
   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;
   bool LoadFactoryDefaults() override;

   // Effect implementation

   bool Init() override;
   void Preview(bool dryOnly) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectAmplify implementation

   void OnAmpText(wxCommandEvent & evt);
   void OnPeakText(wxCommandEvent & evt);
   void OnAmpSlider(wxCommandEvent & evt);
   void OnClipCheckBox(wxCommandEvent & evt);
   void CheckClip();

private:
   double mPeak;

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
