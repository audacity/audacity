/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/
#pragma once

#include "PerTrackEffect.h"
#include "SampleCount.h"
#include "ShuttleAutomation.h"

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

    static constexpr int stagesDefault = 2;
    static constexpr int dryWetDefault = 128;
    static constexpr double freqDefault = 0.4;
    static constexpr double phaseDefault = 0.0;
    static constexpr int depthDefault = 100;
    static constexpr int feedbackDefault = 0;
    static constexpr double outGainDefault = -6.0;

    int mStages { stagesDefault };
    int mDryWet { dryWetDefault };
    double mFreq { freqDefault };
    double mPhase { phaseDefault };
    int mDepth { depthDefault };
    int mFeedback { feedbackDefault };
    double mOutGain { outGainDefault };
};

class BUILTIN_EFFECTS_API PhaserBase : public EffectWithSettings<EffectPhaserSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    PhaserBase();
    virtual ~PhaserBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    RealtimeSince RealtimeSupport() const override;

protected:
    // PhaserBase implementation

    struct BUILTIN_EFFECTS_API Instance : public PerTrackEffect::Instance, public EffectInstanceWithBlockSize
    {
        explicit Instance(const PerTrackEffect& effect)
            : PerTrackEffect::Instance {effect}
        {
        }

        bool ProcessInitialize(
            EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;

        size_t ProcessBlock(
            EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

        bool RealtimeInitialize(EffectSettings& settings, double) override;

        bool RealtimeAddProcessor(
            EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels, float sampleRate) override;

        bool RealtimeFinalize(EffectSettings& settings) noexcept override;

        size_t RealtimeProcess(
            size_t group, EffectSettings& settings, const float* const* inbuf, float* const* outbuf, size_t numSamples) override;

        unsigned GetAudioInCount() const override;
        unsigned GetAudioOutCount() const override;

        void InstanceInit(
            EffectSettings& settings, EffectPhaserState& data, float sampleRate);

        size_t InstanceProcess(
            EffectSettings& settings, EffectPhaserState& data, const float* const* inBlock, float* const* outBlock, size_t blockLen);

        void Coefficients(
            double hz, double slope, double gain, double samplerate, int type, double& a0, double& a1, double& a2, double& b0, double& b1,
            double& b2);

        float DoFilter(EffectPhaserState& data, float in);

        EffectPhaserState mState;
        std::vector<PhaserBase::Instance> mSlaves;
    };

    std::shared_ptr<EffectInstance> MakeInstance() const override;

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Stages {
        &EffectPhaserSettings::mStages,
        L"Stages",
        EffectPhaserSettings::stagesDefault,
        2,
        NUM_STAGES,
        1
    };

    static constexpr EffectParameter DryWet {
        &EffectPhaserSettings::mDryWet,
        L"DryWet",
        EffectPhaserSettings::dryWetDefault,
        0,
        255,
        1
    };

    static constexpr EffectParameter Freq { &EffectPhaserSettings::mFreq,
                                            L"Freq",
                                            EffectPhaserSettings::freqDefault,
                                            0.001,
                                            4.0,
                                            10.0 };

    static constexpr EffectParameter Phase { &EffectPhaserSettings::mPhase,
                                             L"Phase",
                                             EffectPhaserSettings::phaseDefault,
                                             0.0,
                                             360.0,
                                             1 };

    static constexpr EffectParameter Depth { &EffectPhaserSettings::mDepth,
                                             L"Depth",
                                             EffectPhaserSettings::depthDefault,
                                             0,
                                             255,
                                             1 };

    static constexpr EffectParameter Feedback {
        &EffectPhaserSettings::mFeedback,
        L"Feedback",
        EffectPhaserSettings::feedbackDefault,
        -100,
        100,
        1
    };

    static constexpr EffectParameter OutGain {
        &EffectPhaserSettings::mOutGain,
        L"Gain",
        EffectPhaserSettings::outGainDefault,
        -30.0,
        30.0,
        1
    };
};
