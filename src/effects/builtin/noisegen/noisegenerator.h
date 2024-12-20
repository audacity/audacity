#pragma once

#include "PerTrackEffect.h"
#include "SettingsVisitor.h"
#include "common/generatoreffect.h"

namespace au::effects {
struct NoiseSettings
{
    enum Type
    {
        White,
        Pink,
        Brownian,
        Count
    };

    static constexpr Type typeDefault = Type::White;
    static constexpr double amplitudeDefault = 0.8;
    static constexpr double amplitudeMax = 1.0;
    static constexpr double amplitudeMin = 0.0;

    Type type { typeDefault };
    double amplitude { amplitudeDefault };

    bool isApplyAllowed() const;
};

class NoiseGenerator : public GeneratorEffect, public EffectWithSettings <NoiseSettings, PerTrackEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    NoiseGenerator();
    virtual ~NoiseGenerator();

    std::shared_ptr<::EffectInstance> MakeInstance() const override;

    // ComponentInterface implementation
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation
    EffectType GetType() const override;
    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage LoadFactoryPreset(int id, EffectSettings& settings) const override;

    struct Instance : public PerTrackEffect::Instance, public EffectInstanceWithBlockSize {
        explicit Instance(const PerTrackEffect& effect);
        unsigned GetAudioOutCount() const override;
        unsigned GetAudioInCount() const override;

        bool ProcessInitialize(
            EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;
        size_t ProcessBlock(
            EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

        bool InstanceInit(
            EffectSettings& settings, double sampleRate, ChannelNames chanMap);

        size_t InstanceProcess(
            EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen);

        double mSampleRate {};
        float y, z, buf0, buf1, buf2, buf3, buf4, buf5, buf6;
    };
};
}
