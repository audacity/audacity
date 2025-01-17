#pragma once

#include "PerTrackEffect.h"
#include "common/generatoreffect.h"
#include <array>

namespace au::effects {
struct DtmfSettings
{
    static constexpr char DefaultSequence[] = "audacity";
    static constexpr double DefaultDutyCycle = 55.0;
    static constexpr double DefaultAmplitude = 0.8;
    static constexpr double AmplitudeMin = 0.0;
    static constexpr double AmplitudeMax = 1.0;
    static constexpr double DutyCycleMin = 0.0;
    static constexpr double DutyCycleMax = 100.0;

    std::string dtmfSequence { DefaultSequence }; // dtmf tone string
    size_t dtmfNTones = dtmfSequence.length(); // total number of tones to generate
    double dtmfTone {};      // duration of a single tone in ms
    double dtmfSilence {};   // duration of silence between tones in ms
    double dtmfDutyCycle {
        DefaultDutyCycle
    }; // ratio of dtmfTone/(dtmfTone+dtmfSilence)
    double dtmfAmplitude {
        DefaultAmplitude
    }; // amplitude of dtmf tone sequence, restricted to (0-1)

    void Recalculate(EffectSettings& settings);
    bool isApplyAllowed() const;
};

class DtmfGenerator : public GeneratorEffect, public EffectWithSettings<DtmfSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    static constexpr std::array kSymbols {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '*', '#', 'A', 'B',
        'C', 'D', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
        'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
    };

    DtmfGenerator();
    virtual ~DtmfGenerator();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    //! Temporary state of the computation
    struct Instance : PerTrackEffect::Instance, EffectInstanceWithBlockSize
    {
        Instance(const PerTrackEffect& effect, double t0)
            : PerTrackEffect::Instance(effect), mT0(t0)
        {
        }

        bool ProcessInitialize(
            EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
        size_t ProcessBlock(
            EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

        unsigned GetAudioInCount() const override
        {
            return 0;
        }

        unsigned GetAudioOutCount() const override
        {
            return 1;
        }

        const double mT0;
        double mSampleRate {};

        sampleCount numSamplesSequence; // total number of samples to generate
        sampleCount numSamplesTone;   // number of samples in a tone block
        sampleCount numSamplesSilence; // number of samples in a silence block
        sampleCount diff;             // number of extra samples to redistribute
        sampleCount
            numRemaining; // number of samples left to produce in the current block
        sampleCount curTonePos; // position in tone to start the wave
        bool isTone;          // true if block is tone, otherwise silence
        int curSeqPos;        // index into dtmf tone string
    };

    std::shared_ptr<::EffectInstance> MakeInstance() const override;

private:
    // DtmfGenerator implementation

    static bool MakeDtmfTone(float* buffer, size_t len, float fs, char tone, sampleCount last, sampleCount total, float amplitude);
};
}
