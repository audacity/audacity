/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor2.h

  Max Maisel (based on Compressor effect)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR2__
#define __AUDACITY_EFFECT_COMPRESSOR2__

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "Effect.h"

class Plot;
class ShuttleGui;

class SamplePreprocessor
{
   public:
      virtual float ProcessSample(float value) = 0;
      virtual float ProcessSample(float valueL, float valueR) = 0;
};

class SlidingRmsPreprocessor : public SamplePreprocessor
{
   public:
      SlidingRmsPreprocessor(size_t windowSize, float gain = 2.0);

      virtual float ProcessSample(float value);
      virtual float ProcessSample(float valueL, float valueR);

      static const size_t REFRESH_WINDOW_EVERY = 1048576; // 1 MB

   private:
      float mSum;
      float mGain;
      std::vector<float> mWindow;
      size_t mPos;
      size_t mInsertCount;

      inline float DoProcessSample(float value);
      void Refresh();
};

class SlidingMaxPreprocessor : public SamplePreprocessor
{
   public:
      SlidingMaxPreprocessor(size_t windowSize);

      virtual float ProcessSample(float value);
      virtual float ProcessSample(float valueL, float valueR);

   private:
      std::vector<float> mWindow;
      std::vector<float> mMaxes;
      size_t mPos;

      inline float DoProcessSample(float value);
};

class EffectCompressor2 final : public Effect
{
public:
   static const ComponentInterfaceSymbol Symbol;

   EffectCompressor2();
   virtual ~EffectCompressor2();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;
   wxString ManualPage() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;

   // EffectClientInterface implementation

   bool DefineParams( ShuttleParams & S ) override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

   // Effect implementation

   bool CheckWhetherSkipEffect() override;
   bool Startup() override;
   bool Process() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // EffectCompressor2 implementation
   void InitGainCalculation();
   double CompressorGain(double env);

   bool UpdateProgress();
   void OnUpdateUI(wxCommandEvent & evt);
   void UpdateUI();
   void UpdateCompressorPlot();
   void UpdateResponsePlot();

   int    mAlgorithm;
   int    mCompressBy;
   bool   mStereoInd;

   double    mThresholdDB;
   double    mRatio;
   double    mKneeWidthDB;
   double    mAttackTime;
   double    mReleaseTime;
   double    mLookaheadTime;
   double    mLookbehindTime;
   double    mMakeupGainPct;
   double    mDryWetPct;

   // cached intermediate values
   double mMakeupGain;
   double mMakeupGainDB;

   static const size_t RESPONSE_PLOT_SAMPLES = 200;
   static const size_t RESPONSE_PLOT_TIME = 5;
   static const size_t RESPONSE_PLOT_STEP_START = 2;
   static const size_t RESPONSE_PLOT_STEP_STOP = 3;

   Plot* mGainPlot;
   Plot* mResponsePlot;
   bool mIgnoreGuiEvents;

   DECLARE_EVENT_TABLE()
};

#endif
