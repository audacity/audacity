/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2016 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   BassTrebleBase.h (two shelf filters)
   Steve Daulton

**********************************************************************/
#pragma once

#include "PerTrackEffect.h"
#include "SettingsVisitor.h"

// Used to communicate the type of the filter.
enum kShelfType
{
    kBass,
    kTreble
};

class BassTrebleState
{
public:
    float samplerate;
    double treble;
    double bass;
    double gain;
    double slope, hzBass, hzTreble;
    double a0Bass, a1Bass, a2Bass, b0Bass, b1Bass, b2Bass;
    double a0Treble, a1Treble, a2Treble, b0Treble, b1Treble, b2Treble;
    double xn1Bass, xn2Bass, yn1Bass, yn2Bass;
    double xn1Treble, xn2Treble, yn1Treble, yn2Treble;
};

struct BassTrebleSettings
{
    static constexpr double bassDefault = 0.0;
    static constexpr double trebleDefault = 0.0;
    static constexpr double gainDefault = 0.0;
    static constexpr bool linkDefault = false;

    double mBass { bassDefault };
    double mTreble { trebleDefault };
    double mGain { gainDefault };
    bool mLink { linkDefault };
};

class BUILTIN_EFFECTS_API BassTrebleBase : public EffectWithSettings<BassTrebleSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    BassTrebleBase();
    virtual ~BassTrebleBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    RealtimeSince RealtimeSupport() const override;

    // Effect Implementation

    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;

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

        static void InstanceInit(
            EffectSettings& settings, BassTrebleState& data, float sampleRate);

        static size_t InstanceProcess(
            EffectSettings& settings, BassTrebleState& data, const float* const* inBlock, float* const* outBlock, size_t blockLen);

        static void Coefficients(
            double hz, double slope, double gain, double samplerate, int type, double& a0, double& a1, double& a2, double& b0, double& b1,
            double& b2);

        static float DoFilter(BassTrebleState& data, float in);

        BassTrebleState mState;
        std::vector<BassTrebleBase::Instance> mSlaves;
    };

protected:
    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Bass { &BassTrebleSettings::mBass,
                                            L"Bass",
                                            BassTrebleSettings::bassDefault,
                                            -30.0,
                                            30.0,
                                            1 };

    static constexpr EffectParameter Treble { &BassTrebleSettings::mTreble,
                                              L"Treble",
                                              BassTrebleSettings::trebleDefault,
                                              -30.0,
                                              30.0,
                                              1 };

    static constexpr EffectParameter Gain { &BassTrebleSettings::mGain,
                                            L"Gain",
                                            BassTrebleSettings::gainDefault,
                                            -30.0,
                                            30.0,
                                            1 };

    static constexpr EffectParameter Link { &BassTrebleSettings::mLink,
                                            L"Link Sliders",
                                            BassTrebleSettings::linkDefault,
                                            false,
                                            true,
                                            1 };
};
