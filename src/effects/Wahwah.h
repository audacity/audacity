/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_WAHWAH__
#define __AUDACITY_EFFECT_WAHWAH__

#include "StatefulPerTrackEffect.h"
#include "../ShuttleAutomation.h"

class wxSlider;
class wxTextCtrl;
class ShuttleGui;

class EffectWahwahState
{
public:
   float samplerate;
   double depth;
   double freqofs;
   double phase;
   double outgain;
   double lfoskip;
   unsigned long skipcount;
   double xn1, xn2, yn1, yn2;
   double b0, b1, b2, a0, a1, a2;
};


struct EffectWahwahSettings
{
    /* Parameters:
    mFreq - LFO frequency
    mPhase - LFO startphase in RADIANS - useful for stereo WahWah
    mDepth - Wah depth
    mRes - Resonance
    mFreqOfs - Wah frequency offset
    mOutGain - output gain

    !!!!!!!!!!!!! IMPORTANT!!!!!!!!! :
    mDepth and mFreqOfs should be from 0(min) to 1(max) !
    mRes should be greater than 0 !  */

   static constexpr double freqDefault = 1.5;
   static constexpr double phaseDefault = 0.0;
   static constexpr int    depthDefault = 70;
   static constexpr double resDefault = 2.5;
   static constexpr int    freqOfsDefault = 30;
   static constexpr double outGainDefault = -6.0;


   double mFreq   { freqDefault };
   double mPhase  { phaseDefault };
   int    mDepth  { depthDefault };
   double mRes    { resDefault };
   int    mFreqOfs{ freqOfsDefault };
   double mOutGain{ outGainDefault };
};


class EffectWahwah final : public StatefulPerTrackEffect
{
public:

   static inline EffectWahwahSettings *
   FetchParameters(EffectWahwah &e, EffectSettings &) { return &e.mSettings; }
   static const ComponentInterfaceSymbol Symbol;

   EffectWahwah();
   virtual ~EffectWahwah();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   bool SupportsRealtime() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool RealtimeInitialize(EffectSettings &settings) override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   size_t RealtimeProcess(int group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;

   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectSettingsAccess &access) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;

private:
   // EffectWahwah implementation

   EffectWahwahSettings mSettings;

   void InstanceInit(EffectWahwahState & data, float sampleRate);
   size_t InstanceProcess(EffectSettings &settings, EffectWahwahState & data,
      const float *const *inBlock, float *const *outBlock, size_t blockLen);

   void OnFreqSlider(wxCommandEvent & evt);
   void OnPhaseSlider(wxCommandEvent & evt);
   void OnDepthSlider(wxCommandEvent & evt);
   void OnResonanceSlider(wxCommandEvent & evt);
   void OnFreqOffSlider(wxCommandEvent & evt);
   void OnGainSlider(wxCommandEvent & evt);

   void OnFreqText(wxCommandEvent & evt);
   void OnPhaseText(wxCommandEvent & evt);
   void OnDepthText(wxCommandEvent & evt);
   void OnResonanceText(wxCommandEvent & evt);
   void OnFreqOffText(wxCommandEvent & evt);
   void OnGainText(wxCommandEvent & evt);

   EffectWahwahState mMaster;
   std::vector<EffectWahwahState> mSlaves;

   wxTextCtrl *mFreqT;
   wxTextCtrl *mPhaseT;
   wxTextCtrl *mDepthT;
   wxTextCtrl *mResT;
   wxTextCtrl *mFreqOfsT;
   wxTextCtrl *mOutGainT;

   wxSlider *mFreqS;
   wxSlider *mPhaseS;
   wxSlider *mDepthS;
   wxSlider *mResS;
   wxSlider *mFreqOfsS;
   wxSlider *mOutGainS;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Freq   { &EffectWahwahSettings::mFreq,    L"Freq",       EffectWahwahSettings::freqDefault,      0.1,       4.0,  10  };
static constexpr EffectParameter Phase  { &EffectWahwahSettings::mPhase,   L"Phase",      EffectWahwahSettings::phaseDefault,     0.0,     360.0,   1  };
static constexpr EffectParameter Depth  { &EffectWahwahSettings::mDepth,   L"Depth",      EffectWahwahSettings::depthDefault,     0,       100,     1  }; // scaled to 0-1 before processing
static constexpr EffectParameter Res    { &EffectWahwahSettings::mRes,     L"Resonance",  EffectWahwahSettings::resDefault,       0.1,      10.0,  10  };
static constexpr EffectParameter FreqOfs{ &EffectWahwahSettings::mFreqOfs, L"Offset",     EffectWahwahSettings::freqOfsDefault,   0,       100,     1  }; // scaled to 0-1 before processing
static constexpr EffectParameter OutGain{ &EffectWahwahSettings::mOutGain, L"Gain",       EffectWahwahSettings::outGainDefault, -30.0,      30.0,   1  };
};

#endif
