/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NORMALIZE__
#define __AUDACITY_EFFECT_NORMALIZE__

#include "StatefulEffect.h"
#include "StatefulEffectUIServices.h"
#include "Biquad.h"
#include "ShuttleAutomation.h"
#include <wx/weakref.h>
#include <functional>

class wxCheckBox;
class wxStaticText;
class wxTextCtrl;
class ShuttleGui;
class WaveChannel;

class EffectNormalize final :
    public StatefulEffect,
    public StatefulEffectUIServices
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
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectNormalize implementation

   bool ProcessOne(WaveChannel &track,
      const TranslatableString &msg, double& progress, float offset);
   using ProgressReport = std::function<bool(double fraction)>;
   static bool AnalyseTrack(const WaveChannel &track,
      const ProgressReport &report,
      bool gain, bool dc, double curT0, double curT1,
      float &offset, float &extent);
   static bool AnalyseTrackData(const WaveChannel &track,
      const ProgressReport &report, double curT0, double curT1,
      float &offset);
   static double AnalyseDataDC(float *buffer, size_t len, double sum);
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
static constexpr EffectParameter ApplyVolume{ &EffectNormalize::mGain,
   L"ApplyVolume",           true,    false,   true, 1  };
static constexpr EffectParameter StereoInd{ &EffectNormalize::mStereoInd,
   L"StereoIndependent",   false,   false,   true, 1  };
};

#endif
