/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NORMALIZE__
#define __AUDACITY_EFFECT_NORMALIZE__

#include "Effect.h"
#include "Biquad.h"

class wxCheckBox;
class wxStaticText;
class wxTextCtrl;
class ShuttleGui;

class EffectNormalize final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectNormalize();
   virtual ~EffectNormalize();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool GetAutomationParameters(CommandParameters & parms) const override;
   bool SetAutomationParameters(const CommandParameters & parms) override;

   // EffectProcessor implementation

   bool DefineParams( ShuttleParams & S ) override;

   // Effect implementation

   bool CheckWhetherSkipEffect() override;
   bool Process(EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;

private:
   // EffectNormalize implementation

   bool ProcessOne(
      WaveTrack * t, const TranslatableString &msg, double& progress, float offset);
   bool AnalyseTrack(const WaveTrack * track, const TranslatableString &msg,
                     double &progress, float &offset, float &extent);
   bool AnalyseTrackData(const WaveTrack * track, const TranslatableString &msg, double &progress,
                     float &offset);
   void AnalyseDataDC(float *buffer, size_t len);
   void ProcessData(float *buffer, size_t len, float offset);

   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   double mPeakLevel;
   bool   mGain;
   bool   mDC;
   bool   mStereoInd;

   double mCurT0;
   double mCurT1;
   float  mMult;
   double mSum;

   wxCheckBox *mGainCheckBox;
   wxCheckBox *mDCCheckBox;
   wxTextCtrl *mLevelTextCtrl;
   wxStaticText *mLeveldB;
   wxStaticText *mWarning;
   wxCheckBox *mStereoIndCheckBox;
   bool mCreating;


   DECLARE_EVENT_TABLE()
};

#endif
