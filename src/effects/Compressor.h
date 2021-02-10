/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

#include "TwoPassSimpleMono.h"

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
   static const ComponentInterfaceSymbol Symbol;

   EffectCompressor();
   virtual ~EffectCompressor();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   ManualPageID ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // Effect implementation

   bool Startup() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

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

   DECLARE_EVENT_TABLE()
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

