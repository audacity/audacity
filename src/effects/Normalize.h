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
#include "ShuttleAutomation.h"

class wxCheckBox;
class wxStaticText;
class wxTextCtrl;
class ShuttleGui;

class EffectNormalize final : public StatefulEffect
{
public:
   static inline EffectNormalize *
   FetchParameters(EffectNormalize &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectNormalize();
   virtual ~EffectNormalize();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   bool CheckWhetherSkipEffect(const EffectSettings &settings) const override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

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
   wxWeakRef<wxWindow> mUIParent{};

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

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter PeakLevel{ &EffectNormalize::mPeakLevel,
   L"PeakLevel",           -1.0,    -145.0,  0.0,  1  };
static constexpr EffectParameter RemoveDC{ &EffectNormalize::mDC,
   L"RemoveDcOffset",      true,    false,   true, 1  };
static constexpr EffectParameter ApplyGain{ &EffectNormalize::mGain,
   L"ApplyGain",           true,    false,   true, 1  };
static constexpr EffectParameter StereoInd{ &EffectNormalize::mStereoInd,
   L"StereoIndependent",   false,   false,   true, 1  };
};

#endif
