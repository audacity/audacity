#pragma once

#include "PerTrackEffect.h"
#include "Reverb_libSoX.h"
#include "SettingsVisitor.h"

namespace au::effects {
struct ReverbSettings
{
    static constexpr double roomSizeDefault = 75.0;
    static constexpr double preDelayDefault = 10.0;
    static constexpr double reverberanceDefault = 50.0;
    static constexpr double hfDampingDefault = 50.0;
    static constexpr double toneLowDefault = 100.0;
    static constexpr double toneHighDefault = 100.0;
    static constexpr double wetGainDefault = -1.0;
    static constexpr double dryGainDefault = -1.0;
    static constexpr double stereoWidthDefault = 100.0;
    static constexpr bool wetOnlyDefault = false;

    double mRoomSize { roomSizeDefault };
    double mPreDelay { preDelayDefault };
    double mReverberance { reverberanceDefault };
    double mHfDamping { hfDampingDefault };
    double mToneLow { toneLowDefault };
    double mToneHigh { toneHighDefault };
    double mWetGain { wetGainDefault };
    double mDryGain { dryGainDefault };
    double mStereoWidth { stereoWidthDefault };
    bool mWetOnly { wetOnlyDefault };

    friend bool operator==(const ReverbSettings& a, const ReverbSettings& b);

    friend bool OnlySimpleParametersChanged(
        const ReverbSettings& a, const ReverbSettings& b);
};

struct Reverb_priv_t
{
    reverb_t reverb;
    float* dry;
    float* wet[2];
};

struct Reverb_priv_ex : Reverb_priv_t
{
    Reverb_priv_ex()
        : Reverb_priv_t{}
    {
    }

    ~Reverb_priv_ex()
    {
        reverb_delete(&reverb);
    }
};

struct ReverbState
{
    unsigned mNumChans {};
    std::unique_ptr<Reverb_priv_ex[]> mP {};
};

class ReverbEffect : public EffectWithSettings<ReverbSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    ReverbEffect();
    virtual ~ReverbEffect();

    std::shared_ptr<::EffectInstance> MakeInstance() const override;

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage LoadFactoryPreset(int id, EffectSettings& settings) const override;

    RealtimeSince RealtimeSupport() const override;

    struct Instance : public PerTrackEffect::Instance, public EffectInstanceWithBlockSize
    {
        explicit Instance(const PerTrackEffect& effect);

        bool ProcessInitialize(
            EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;

        size_t ProcessBlock(
            EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

        bool ProcessFinalize(void) noexcept override;

        // Realtime section

        bool RealtimeInitialize(EffectSettings& settings, double sampleRate) override;

        bool RealtimeAddProcessor(
            EffectSettings& settings, EffectOutputs*, unsigned numChannels, float sampleRate) override;

        bool RealtimeFinalize(EffectSettings& settings) noexcept override;

        size_t RealtimeProcess(
            size_t group, EffectSettings& settings, const float* const* inbuf, float* const* outbuf, size_t numSamples) override;

        bool RealtimeSuspend() override;

        unsigned GetAudioOutCount() const override;

        unsigned GetAudioInCount() const override;

        bool InstanceInit(
            const EffectSettings& settings, double sampleRate, ReverbState& data, ChannelNames chanMap, bool forceStereo);

        size_t InstanceProcess(
            EffectSettings& settings, ReverbState& data, const float* const* inBlock, float* const* outBlock, size_t blockLen);

        ReverbState mState;
        std::vector<ReverbEffect::Instance> mSlaves;

        unsigned mChannels { 2 };

        ReverbSettings mLastAppliedSettings;
        double mLastSampleRate { 0 };
    };

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter RoomSize {
        &ReverbSettings::mRoomSize,
        L"RoomSize",
        ReverbSettings::roomSizeDefault,
        0,
        100,
        1 };

    static constexpr EffectParameter PreDelay {
        &ReverbSettings::mPreDelay,
        L"Delay",
        ReverbSettings::preDelayDefault,
        0,
        200,
        1 };

    static constexpr EffectParameter Reverberance {
        &ReverbSettings::mReverberance,
        L"Reverberance",
        ReverbSettings::reverberanceDefault,
        0,
        100,
        1
    };

    static constexpr EffectParameter HfDamping {
        &ReverbSettings::mHfDamping,
        L"HfDamping",
        ReverbSettings::hfDampingDefault,
        0,
        100,
        1
    };

    static constexpr EffectParameter ToneLow {
        &ReverbSettings::mToneLow,
        L"ToneLow",
        ReverbSettings::toneLowDefault,
        0,
        100,
        1 };

    static constexpr EffectParameter ToneHigh {
        &ReverbSettings::mToneHigh,
        L"ToneHigh",
        ReverbSettings::toneHighDefault,
        0,
        100,
        1 };

    static constexpr EffectParameter WetGain {
        &ReverbSettings::mWetGain,
        L"WetGain",
        ReverbSettings::wetGainDefault,
        -20,
        10,
        1 };

    static constexpr EffectParameter DryGain {
        &ReverbSettings::mDryGain,
        L"DryGain",
        ReverbSettings::dryGainDefault,
        -20,
        10,
        1 };

    static constexpr EffectParameter StereoWidth {
        &ReverbSettings::mStereoWidth,
        L"StereoWidth",
        ReverbSettings::stereoWidthDefault,
        0,
        100,
        1
    };

    static constexpr EffectParameter WetOnly {
        &ReverbSettings::mWetOnly,
        L"WetOnly",
        ReverbSettings::wetOnlyDefault,
        false,
        true,
        1 };
};
} // namespace au::effects
