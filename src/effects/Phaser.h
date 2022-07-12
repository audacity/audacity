/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_PHASER__
#define __AUDACITY_EFFECT_PHASER__

#include "StatefulPerTrackEffect.h"
#include "../ShuttleAutomation.h"

class wxSlider;
class wxTextCtrl;
class ShuttleGui;

#define NUM_STAGES 24

class EffectPhaserState
{
public:
   // state variables
   float samplerate;
   sampleCount skipcount;
   double old[NUM_STAGES]; // must be as large as MAX_STAGES
   double gain;
   double fbout;
   double outgain;
   double lfoskip;
   double phase;
   int laststages;
};

class EffectPhaser final : public StatefulPerTrackEffect
{
public:
   static inline EffectPhaser *
   FetchParameters(EffectPhaser &e, EffectSettings &) { return &e; }
   static const ComponentInterfaceSymbol Symbol;

   EffectPhaser();
   virtual ~EffectPhaser();

   // ComponentInterface implementation

   ComponentInterfaceSymbol GetSymbol() const override;
   TranslatableString GetDescription() const override;
   ManualPageID ManualPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   RealtimeSince RealtimeSupport() const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;
   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      sampleCount totalLen, ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;
   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   size_t RealtimeProcess(size_t group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;

   // Effect implementation

   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access)
   override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

private:
   // EffectPhaser implementation

   void InstanceInit(EffectPhaserState & data, float sampleRate);
   size_t InstanceProcess(EffectSettings &settings, EffectPhaserState & data,
      const float *const *inBlock, float *const *outBlock, size_t blockLen);

   void OnStagesSlider(wxCommandEvent & evt);
   void OnDryWetSlider(wxCommandEvent & evt);
   void OnFeedbackSlider(wxCommandEvent & evt);
   void OnDepthSlider(wxCommandEvent & evt);
   void OnPhaseSlider(wxCommandEvent & evt);
   void OnFreqSlider(wxCommandEvent & evt);
   void OnGainSlider(wxCommandEvent & evt);

   void OnStagesText(wxCommandEvent & evt);
   void OnDryWetText(wxCommandEvent & evt);
   void OnFeedbackText(wxCommandEvent & evt);
   void OnDepthText(wxCommandEvent & evt);
   void OnPhaseText(wxCommandEvent & evt);
   void OnFreqText(wxCommandEvent & evt);
   void OnGainText(wxCommandEvent & evt);
/*
    Phaser Parameters

 mFreq       - Phaser's LFO frequency
 mPhase      - Phaser's LFO startphase (radians), needed for stereo Phasers
 mDepth      - Phaser depth (0 - no depth, 255 - max depth)
 mStages     - Phaser stages (recomanded from 2 to 16-24, and EVEN NUMBER)
 mDryWet     - Dry/wet mix, (0 - dry, 128 - dry=wet, 255 - wet)
 mFeedback   - Phaser FeedBack (0 - no feedback, 100 = 100% Feedback,
                               -100 = -100% FeedBack)
*/

   EffectPhaserState mMaster;
   std::vector<EffectPhaserState> mSlaves;

   // parameters
   int mStages;
   int mDryWet;
   double mFreq;
   double mPhase;
   int mDepth;
   int mFeedback;
   double mOutGain;

   wxTextCtrl *mStagesT;
   wxTextCtrl *mDryWetT;
   wxTextCtrl *mFreqT;
   wxTextCtrl *mPhaseT;
   wxTextCtrl *mDepthT;
   wxTextCtrl *mFeedbackT;
   wxTextCtrl *mOutGainT;

   wxSlider *mStagesS;
   wxSlider *mDryWetS;
   wxSlider *mFreqS;
   wxSlider *mPhaseS;
   wxSlider *mDepthS;
   wxSlider *mFeedbackS;
   wxSlider *mOutGainS;

   const EffectParameterMethods& Parameters() const override;
   DECLARE_EVENT_TABLE()

static constexpr EffectParameter Stages{ &EffectPhaser::mStages,
   L"Stages",     2,    2,    NUM_STAGES, 1  };
static constexpr EffectParameter DryWet{ &EffectPhaser::mDryWet,
   L"DryWet",     128,  0,    255,        1  };
static constexpr EffectParameter Freq{ &EffectPhaser::mFreq,
   L"Freq",       0.4,  0.001,4.0,        10.0 };
static constexpr EffectParameter Phase{ &EffectPhaser::mPhase,
   L"Phase",      0.0,  0.0,  360.0,      1  };
static constexpr EffectParameter Depth{ &EffectPhaser::mDepth,
   L"Depth",      100,  0,    255,        1  };
static constexpr EffectParameter Feedback{ &EffectPhaser::mFeedback,
   L"Feedback",   0,    -100, 100,        1  };
static constexpr EffectParameter OutGain{ &EffectPhaser::mOutGain,
   L"Gain",      -6.0,    -30.0,    30.0,    1   };
};

#endif
