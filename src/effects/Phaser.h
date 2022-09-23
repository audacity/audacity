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


struct EffectPhaserSettings
{
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

   static constexpr int    stagesDefault   = 2;
   static constexpr int    dryWetDefault   = 128;
   static constexpr double freqDefault     = 0.4;
   static constexpr double phaseDefault    = 0.0;
   static constexpr int    depthDefault    = 100;
   static constexpr int    feedbackDefault = 0;
   static constexpr double outGainDefault  = -6.0;

   int    mStages  { stagesDefault   };
   int    mDryWet  { dryWetDefault   };
   double mFreq    { freqDefault     };
   double mPhase   { phaseDefault    };
   int    mDepth   { depthDefault    };
   int    mFeedback{ feedbackDefault };
   double mOutGain { outGainDefault  };
};

class EffectPhaser final : public StatefulPerTrackEffect
{
public:
   static inline EffectPhaserSettings *
   FetchParameters(EffectPhaser &e, EffectSettings &) { return &e.mSettings; }
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
      ChannelNames chanMap) override;
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

   EffectPhaserState mMaster;
   std::vector<EffectPhaserState> mSlaves;

   EffectPhaserSettings mSettings;

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

static constexpr EffectParameter Stages
{ &EffectPhaserSettings::mStages, L"Stages",
   EffectPhaserSettings::stagesDefault,  2,    NUM_STAGES, 1  };

static constexpr EffectParameter DryWet
{ &EffectPhaserSettings::mDryWet, L"DryWet",
   EffectPhaserSettings::dryWetDefault,  0,    255,        1  };

static constexpr EffectParameter Freq
{ &EffectPhaserSettings::mFreq,     L"Freq",
   EffectPhaserSettings::freqDefault,  0.001, 4.0,        10.0 };

static constexpr EffectParameter Phase
{ &EffectPhaserSettings::mPhase,   L"Phase",
   EffectPhaserSettings::phaseDefault,  0.0,  360.0,      1  };

static constexpr EffectParameter Depth
{ &EffectPhaserSettings::mDepth,   L"Depth",
   EffectPhaserSettings::depthDefault,  0,    255,        1  };

static constexpr EffectParameter Feedback
{ &EffectPhaserSettings::mFeedback, L"Feedback",
   EffectPhaserSettings::feedbackDefault,    -100, 100,        1  };

static constexpr EffectParameter OutGain
{ &EffectPhaserSettings::mOutGain, L"Gain",
   EffectPhaserSettings::outGainDefault,    -30.0,    30.0,    1   };
};

#endif
