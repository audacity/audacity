/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

#include "TwoPassSimpleMono.h"
#include "../ShuttleAutomation.h"
#include "MemoryX.h"
#include "../widgets/wxPanelWrapper.h"

class wxCheckBox;
class wxSlider;
class wxStaticText;
class EffectCompressorPanel;
class ShuttleGui;

using Floats = ArrayOf<float>;
using Doubles = ArrayOf<double>;

class EffectCompressor final : public EffectTwoPassSimpleMono
{
public:
   static inline EffectCompressor *
   FetchParameters(EffectCompressor &e, EffectSettings &) { return &e; }

   static const ComponentInterfaceSymbol Symbol;

   EffectCompressor();
   virtual ~EffectCompressor();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription()  const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;

   // Effect implementation

   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool DoTransferDataFromWindow();
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

protected:
   // EffectTwoPassSimpleMono implementation

   bool InitPass1() override;
   bool InitPass2() override;
   bool NewTrackPass1() override;
   bool ProcessPass2(float *buffer, size_t len) override;
   bool TwoBufferProcessPass1
      (float *buffer1, size_t len1, float *buffer2, size_t len2) override;

private:
   // EffectCompressor implementation

   void FreshenCircle();
   float AvgCircle(float x);
   void Follow(float *buffer, float *env, size_t len, float *previous, size_t previous_len);
   float DoCompression(float x, double env);

   void OnSlider(wxCommandEvent & evt);
   void UpdateUI();

private:
   wxWeakRef<wxWindow> mUIParent{};

   double    mRMSSum;
   size_t    mCircleSize;
   size_t    mCirclePos;
   Doubles   mCircle;

   double    mAttackTime;
   double    mThresholdDB;
   double    mNoiseFloorDB;
   double    mRatio;
   bool      mNormalize;	//MJS
   bool      mUsePeak;

   double    mDecayTime;   // The "Release" time.
   double    mAttackFactor;
   double    mAttackInverseFactor;
   double    mDecayFactor;
   double    mThreshold;
   double    mCompression;
   double    mNoiseFloor;
   int       mNoiseCounter;
   double    mGain;
   double    mLastLevel;
   Floats mFollow1, mFollow2;
   size_t    mFollowLen;

   double    mMax;			//MJS

   EffectCompressorPanel *mPanel;

   wxStaticText *mThresholdLabel;
   wxSlider *mThresholdSlider;
   wxStaticText *mThresholdText;

   wxStaticText *mNoiseFloorLabel;
   wxSlider *mNoiseFloorSlider;
   wxStaticText *mNoiseFloorText;

   wxStaticText *mRatioLabel;
   wxSlider *mRatioSlider;
   wxStaticText *mRatioText;

   wxStaticText *mAttackLabel;
   wxSlider *mAttackSlider;
   wxStaticText *mAttackText;

   wxStaticText *mDecayLabel;
   wxSlider *mDecaySlider;
   wxStaticText *mDecayText;

   wxCheckBox *mGainCheckBox;
   wxCheckBox *mPeakCheckBox;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Threshold{ &EffectCompressor::mThresholdDB,
   L"Threshold",     -12.0,   -60.0,   -1.0,    1   };
static constexpr EffectParameter NoiseFloor{ &EffectCompressor::mNoiseFloorDB,
   L"NoiseFloor",    -40.0,   -80.0,   -20.0,   0.2   };
static constexpr EffectParameter Ratio{ &EffectCompressor::mRatio,
   L"Ratio",         2.0,     1.1,     10.0,    10  };
static constexpr EffectParameter AttackTime{ &EffectCompressor::mAttackTime,
   L"AttackTime",    0.2,     0.1,     5.0,     100 };
static constexpr EffectParameter ReleaseTime{ &EffectCompressor::mDecayTime,
   L"ReleaseTime",   1.0,     1.0,     30.0,    10  };
static constexpr EffectParameter Normalize{ &EffectCompressor::mNormalize,
   L"Normalize",     true,    false,   true,    1   };
static constexpr EffectParameter UsePeak{ &EffectCompressor::mUsePeak,
   L"UsePeak",       false,   false,   true,    1   };
};

class EffectCompressorPanel final : public wxPanelWrapper
{
public:
   EffectCompressorPanel(wxWindow *parent, wxWindowID winid,
                         double & threshold,
                         double & noiseFloor,
                         double & ratio);

private:
   void OnPaint(wxPaintEvent & evt);
   void OnSize(wxSizeEvent & evt);

private:
   double & threshold;
   double & noiseFloor;
   double & ratio;

   DECLARE_EVENT_TABLE()
};

#endif

