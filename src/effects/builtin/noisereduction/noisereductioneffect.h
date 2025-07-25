/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseReductionEffect.h

  Dominic Mazzoni
  Paul Licameli

**********************************************************************/

#include "libraries/lib-effects/StatefulEffect.h"
#include "libraries/lib-components/SettingsVisitor.h"

namespace au::effects {
// Define both of these to make the radio button three-way
#define RESIDUE_CHOICE
//#define ISOLATE_CHOICE

// Define for Attack and release controls.
// #define ATTACK_AND_RELEASE

// Define to expose other advanced, experimental dialog controls
//#define ADVANCED_SETTINGS

// Define to make the old statistical methods an available choice
//#define OLD_METHOD_AVAILABLE

enum NoiseReductionChoice
{
    NRC_REDUCE_NOISE = 0,
    NRC_LEAVE_RESIDUE,
    NRC_COUNT,
};

// Deprecated
static constexpr auto NRC_ISOLATE_NOISE = static_cast<NoiseReductionChoice>(NRC_COUNT);

enum WindowTypes : unsigned
{
    WT_RECTANGULAR_HANN = 0, // 2.0.6 behavior, requires 1/2 step
    WT_HANN_RECTANGULAR,    // requires 1/2 step
    WT_HANN_HANN,           // requires 1/4 step
    WT_BLACKMAN_HANN,       // requires 1/4 step
    WT_HAMMING_RECTANGULAR, // requires 1/2 step
    WT_HAMMING_HANN,        // requires 1/4 step
    // WT_HAMMING_INV_HAMMING, // requires 1/2 step

    WT_N_WINDOW_TYPES,
    WT_DEFAULT_WINDOW_TYPES = WT_HANN_HANN
};

enum
{
    DEFAULT_WINDOW_SIZE_CHOICE = 8, // corresponds to 2048
    DEFAULT_STEPS_PER_WINDOW_CHOICE
        =1 // corresponds to 4, minimum for WT_HANN_HANN
};

enum DiscriminationMethod
{
    DM_MEDIAN,
    DM_SECOND_GREATEST,

    DM_N_METHODS,
    DM_DEFAULT_METHOD = DM_SECOND_GREATEST,
};

class NoiseReductionSettings
{
public:
    size_t WindowSize() const
    {
        return 1u << (3 + DEFAULT_WINDOW_SIZE_CHOICE);
    }

    unsigned StepsPerWindow() const
    {
        return 1u << (1 + DEFAULT_STEPS_PER_WINDOW_CHOICE);
    }

    size_t SpectrumSize() const
    {
        return 1 + WindowSize() / 2;
    }

    size_t StepSize() const
    {
        return WindowSize() / StepsPerWindow();
    }

    bool mDoProfile{ false };

    // Stored in preferences:

    static constexpr auto sensitivityDefault = 6.0;
    static constexpr auto freqSmoothingBandsDefault = 6;
    static constexpr auto noiseGainDefault = 6;
    static constexpr auto noiseReductionChoiceDefault = static_cast<int>(NRC_REDUCE_NOISE);

    // Basic:
    double mNewSensitivity = sensitivityDefault; // - log10 of a probability... yeah.
    int mFreqSmoothingBands = freqSmoothingBandsDefault;
    int mNoiseGain = noiseGainDefault; // in dB, positive
    int mNoiseReductionChoice = noiseReductionChoiceDefault;

    // These could be parametrized in ancient times ; to reduce the diff we make them const now.
    static constexpr int mWindowTypes = WT_DEFAULT_WINDOW_TYPES;
    static constexpr int mWindowSizeChoice = DEFAULT_WINDOW_SIZE_CHOICE;
    static constexpr int mStepsPerWindowChoice = DEFAULT_STEPS_PER_WINDOW_CHOICE;
    static constexpr int mMethod = DM_DEFAULT_METHOD;
    static constexpr double mAttackTime = 0.02;
    static constexpr double mReleaseTime = 0.1;
};

class NoiseReductionEffect : public EffectWithSettings<NoiseReductionSettings, StatefulEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    NoiseReductionEffect() = default;
    virtual ~NoiseReductionEffect();

    void ResetLastError();

    using Effect::TrackProgress;

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    // EffectDefinitionInterface implementation

    ::EffectType GetType() const override;

    // Effect implementation

    const EffectParameterMethods& Parameters() const override;
    // In case the user intends to get a noise profile, selecting the whole project makes little sense.
    bool applyEffectToAllAudio() const override { return false; }
    bool Process(::EffectInstance& instance, ::EffectSettings& settings) override;

    static constexpr EffectParameter sensitivity {
        &NoiseReductionSettings::mNewSensitivity,
        L"Sensitivity",
        NoiseReductionSettings::sensitivityDefault,
        0.01,
        24.0,
        1.0 };

    static constexpr EffectParameter frequencySmoothingBands {
        &NoiseReductionSettings::mFreqSmoothingBands,
        L"Frequency Smoothing Bands",
        NoiseReductionSettings::freqSmoothingBandsDefault,
        0,
        12,
        1 };

    static constexpr EffectParameter noiseGain {
        &NoiseReductionSettings::mNoiseGain,
        L"Noise Gain",
        NoiseReductionSettings::noiseGainDefault,
        0,
        48,
        1 };

    static constexpr EffectParameter noiseReductionChoice {
        &NoiseReductionSettings::mNoiseReductionChoice,
        L"Noise Reduction Choice",
        NoiseReductionSettings::noiseReductionChoiceDefault,
        0,
        NRC_COUNT - 1 };

    class Statistics
    {
    public:
        Statistics(size_t spectrumSize, double rate, int windowTypes)
            : mRate{rate}
            , mWindowSize{(spectrumSize - 1) * 2}
            , mTotalWindows{0}
            , mTrackWindows{0}
            , mSums(spectrumSize)
            , mMeans(spectrumSize)
#ifdef OLD_METHOD_AVAILABLE
            , mNoiseThreshold(spectrumSize)
#endif
        {
        }

        // Noise profile statistics follow

        double mRate; // Rate of profile track(s) -- processed tracks must match
        size_t mWindowSize;

        unsigned mTotalWindows;
        unsigned mTrackWindows;
        std::vector<float> mSums;
        std::vector<float> mMeans;

#ifdef OLD_METHOD_AVAILABLE
        // Old statistics:
        std::vector<float> mNoiseThreshold;
#endif
    };

    class Worker;

public:
    std::unique_ptr<Statistics> mStatistics;
};
}
