/**********************************************************************

  Audacity: A Digital Audio Editor

  DistortionBase.h

  Steve Daulton

**********************************************************************/
#pragma once

#include "PerTrackEffect.h"
#include "SampleCount.h"
#include "SettingsVisitor.h"
#include <queue>

#define TABLESIZE 2049 // size of lookup table (steps * 2 + 1)

const bool defaultDCBlock = false;

class EffectDistortionState
{
public:
    float samplerate;
    sampleCount skipcount;
    int tablechoiceindx;
    bool dcblock;
    double threshold;
    double noisefloor;
    double param1;
    double param2;
    int repeats;

    // DC block filter variables
    std::queue<float> queuesamples;
    double queuetotal;

    bool mbSavedFilterState { defaultDCBlock };

    // mMakeupGain is used by some distortion types to pass the
    // amount of gain required to bring overall effect gain to unity
    double mMakeupGain { 1.0 };
};

struct EffectDistortionSettings
{
    static constexpr int mDefaultTableChoiceIndx = 0;
    static constexpr bool mDefaultDCBlock = defaultDCBlock;
    static constexpr double mDefaultThreshold_dB = -6.0;
    static constexpr double mDefaultNoiseFloor = -70.0;
    static constexpr double mDefaultParam1 = 50.0;
    static constexpr double mDefaultParam2 = 50.0;
    static constexpr int mDefaultRepeats = 1;

    int mTableChoiceIndx { mDefaultTableChoiceIndx };
    bool mDCBlock { mDefaultDCBlock };
    double mThreshold_dB { mDefaultThreshold_dB };
    double mNoiseFloor { mDefaultNoiseFloor };
    double mParam1 { mDefaultParam1 };
    double mParam2 { mDefaultParam2 };
    int mRepeats { mDefaultRepeats };
};

class BUILTIN_EFFECTS_API DistortionBase : public EffectWithSettings<EffectDistortionSettings, PerTrackEffect>
{
public:
    struct Params;

    static const ComponentInterfaceSymbol Symbol;

    DistortionBase();
    virtual ~DistortionBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    RealtimeSince RealtimeSupport() const override;
    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage
    LoadFactoryPreset(int id, EffectSettings& settings) const override;
    OptionalMessage DoLoadFactoryPreset(int id, EffectSettings& settings);

    // Effect implementation

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

        void InstanceInit(
            EffectDistortionState& data, EffectSettings& settings, float sampleRate);

        size_t InstanceProcess(
            EffectSettings& settings, EffectDistortionState& data, const float* const* inBlock, float* const* outBlock, size_t blockLen);

        void MakeTable(
            EffectDistortionState& state, const EffectDistortionSettings& ms);

        void HardClip(
            EffectDistortionState&, const EffectDistortionSettings&); // hard clipping

        void SoftClip(
            EffectDistortionState&, const EffectDistortionSettings&); // soft clipping

        void
        ExponentialTable(const EffectDistortionSettings&); // exponential mapping
        void
        LogarithmicTable(const EffectDistortionSettings&); // logarithmic mapping
        void HalfSinTable(const EffectDistortionSettings&);
        void CubicTable(const EffectDistortionSettings&);
        void EvenHarmonicTable(const EffectDistortionSettings&);
        void SineTable(const EffectDistortionSettings&);
        void
        Leveller(const EffectDistortionSettings&); // 'Leveller' wavetable is
                                                   // modeled on the legacy effect
                                                   // of the same name.
        void
        Rectifier(const EffectDistortionSettings&); // 0% = Dry, 50% = half-wave
                                                    // rectified, 100% = full-wave
                                                    // rectified (abs value).
        void HardLimiter(
            EffectDistortionState& state, const EffectDistortionSettings&); // Same effect as the LADSPA
                                                                            // "hardLimiter 1413"

        void CopyHalfTable(); // for symmetric tables

        // Used by Soft Clipping but could be used for other tables.
        // Log curve formula: y = T + (((e^(RT - Rx)) - 1) / -R)
        // where R is the ratio, T is the threshold, and x is from T to 1.
        inline float LogCurve(double threshold, float value, double ratio);

        // Used by Cubic curve but could be used for other tables
        // Cubic formula: y = x - (x^3 / 3.0)
        inline double Cubic(const EffectDistortionSettings&, double x);

        float WaveShaper(float sample, EffectDistortionSettings& ms);
        float DCFilter(EffectDistortionState& data, float sample);

        unsigned GetAudioInCount() const override;
        unsigned GetAudioOutCount() const override;

        double mTable[TABLESIZE];

        EffectDistortionState mMaster;
        std::vector<EffectDistortionState> mSlaves;
    };

    std::shared_ptr<EffectInstance> MakeInstance() const override;

protected:
    const EffectParameterMethods& Parameters() const override;

    enum kTableType
    {
        kHardClip,
        kSoftClip,
        kHalfSinCurve,
        kExpCurve,
        kLogCurve,
        kCubic,
        kEvenHarmonics,
        kSinCurve,
        kLeveller,
        kRectifier,
        kHardLimiter,
        nTableTypes
    };

    static const EnumValueSymbol kTableTypeStrings[nTableTypes];

    // (Note: 'Repeats' is the total number of times the effect is applied.)
    static constexpr EnumParameter TableTypeIndx {
        &EffectDistortionSettings::mTableChoiceIndx,
        L"Type",
        EffectDistortionSettings::mDefaultTableChoiceIndx,
        0,
        nTableTypes - 1,
        1,
        kTableTypeStrings,
        nTableTypes
    };

    static constexpr EffectParameter DCBlock {
        &EffectDistortionSettings::mDCBlock,
        L"DC Block",
        EffectDistortionSettings::mDefaultDCBlock,
        false,
        true,
        1
    };

    static constexpr EffectParameter Threshold_dB {
        &EffectDistortionSettings::mThreshold_dB,
        L"Threshold dB",
        EffectDistortionSettings::mDefaultThreshold_dB,
        -100.0,
        0.0,
        1000.0f
    };

    static constexpr EffectParameter NoiseFloor {
        &EffectDistortionSettings::mNoiseFloor,
        L"Noise Floor",
        EffectDistortionSettings::mDefaultNoiseFloor,
        -80.0,
        -20.0,
        1
    };

    static constexpr EffectParameter Param1 {
        &EffectDistortionSettings::mParam1,
        L"Parameter 1",
        EffectDistortionSettings::mDefaultParam1,
        0.0,
        100.0,
        1
    };

    static constexpr EffectParameter Param2 {
        &EffectDistortionSettings::mParam2,
        L"Parameter 2",
        EffectDistortionSettings::mDefaultParam2,
        0.0,
        100.0,
        1
    };

    static constexpr EffectParameter Repeats {
        &EffectDistortionSettings::mRepeats,
        L"Repeats",
        EffectDistortionSettings::mDefaultRepeats,
        0,
        5,
        1
    };
};
