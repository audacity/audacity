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
#include "../ShuttleAutomation.h"

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

   struct NormParams { // per track
      WaveTrack* mTrack;
      double mCurT0;
      double mCurT1;
      float  mMult;
      double mSum;
      std::vector<float> mOffsets; // per channel
      NormParams () : mTrack(nullptr), mCurT0(0), mCurT1(0), mMult(0), mSum(0) { }
   };

   // Note: 'offset' is always passed separately from 'params' because offsets are per channel,
   // not per track, and NormParams was added after the fact (used to be member vars). The
   // alternative would be to pass a channel index to each of these functions (to specify which
   // of params.mOffsets[] to use). Using a channel index would be a debatably better approach,
   // but would require some additional code changes, and I am trying to make the least amount
   // of changes possible. Possible TODO.           [JC3, 2023-Feb-04, while adding NormParams]

   bool ProcessOne(WaveTrack * t, const TranslatableString &msg, double& progress,
                   float offset, const NormParams &params);
   bool AnalyseTrack(const WaveTrack * track, const TranslatableString &msg,
                     double &progress, float &offset, float &extent, NormParams &params);
   bool AnalyseTrackData(const WaveTrack * track, const TranslatableString &msg, double &progress,
                     float &offset, NormParams &params);
   void AnalyseDataDC(float *buffer, size_t len, NormParams &params);
   void ProcessData(float *buffer, size_t len, float offset, const NormParams &params);

   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();

private:
   double mPeakLevel;
   bool   mGain;
   bool   mDC;
   bool   mStereoInd;

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
