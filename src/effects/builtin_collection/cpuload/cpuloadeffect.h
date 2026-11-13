/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <vector>

#include "au3-effects/PerTrackEffect.h"
#include "au3-components/SettingsVisitor.h"

namespace au::effects {
struct CpuLoadSettings
{
    static constexpr double loadDefault = 10.0;
    static constexpr double spikeDefault = 0.0;
    static constexpr double spikeIntervalDefault = 5.0;

    //! Percent of a CPU core, at realtime
    double load { loadDefault };
    //! Milliseconds of CPU time, once per interval
    double spike { spikeDefault };
    //! Seconds of audio between spikes
    double spikeInterval { spikeIntervalDefault };
};

inline bool operator==(const CpuLoadSettings& a, const CpuLoadSettings& b)
{
    return a.load == b.load && a.spike == b.spike && a.spikeInterval == b.spikeInterval;
}

inline bool operator!=(const CpuLoadSettings& a, const CpuLoadSettings& b) { return !(a == b); }

//! Passes the audio through, costing CPU time: to compare how much processing the audio engines keep up with
class CpuLoadEffect : public EffectWithSettings<CpuLoadSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    CpuLoadEffect();

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;
    ::EffectType GetType() const override;
    EffectGroup GetGroup() const override;
    RealtimeSince RealtimeSupport() const override;

    std::shared_ptr<::EffectInstance> MakeInstance() const override;

    static constexpr EffectParameter Load { &CpuLoadSettings::load, L"Load", CpuLoadSettings::loadDefault, 0.0, 200.0, 1 };
    static constexpr EffectParameter Spike { &CpuLoadSettings::spike, L"Spike", CpuLoadSettings::spikeDefault, 0.0, 500.0, 1 };
    static constexpr EffectParameter SpikeInterval { &CpuLoadSettings::spikeInterval, L"SpikeInterval",
                                                     CpuLoadSettings::spikeIntervalDefault, 1.0, 30.0, 1 };

    struct Instance : public PerTrackEffect::Instance, public EffectInstanceWithBlockSize
    {
        explicit Instance(const PerTrackEffect& effect)
            : PerTrackEffect::Instance{effect} {}

        bool ProcessInitialize(EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
        size_t ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

        bool RealtimeInitialize(EffectSettings& settings, double sampleRate, size_t audioThreadBufferSize) override;
        bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs* outputs, unsigned numChannels, float sampleRate) override;
        bool RealtimeFinalize(EffectSettings& settings) noexcept override;
        size_t RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf, float* const* outBuf,
                               size_t numSamples) override;

        unsigned GetAudioInCount() const override { return 1; }
        unsigned GetAudioOutCount() const override { return 1; }

    private:
        //! One per channel
        struct Processor {
            double sampleRate = 44100.0;
            //! What the work computes, kept so that it isn't optimized away
            double state = 1.0;
            //! The fraction of an iteration left over from the last block
            double carry = 0.0;
            double sinceSpike = 0.0;
        };

        size_t process(size_t group, const EffectSettings& settings, const float* const* inBuf, float* const* outBuf, size_t numSamples);

        std::vector<Processor> m_processors;
    };

protected:
    const EffectParameterMethods& Parameters() const override;
};
}
