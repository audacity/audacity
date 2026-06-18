/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-components/SettingsVisitor.h"
#include "au3-effects/StatefulEffect.h"

class WaveChannel;
class WaveClipChannel;

namespace au::effects {
struct DeepFilterNetSettings
{
    double mAttenuationLimitDb = 100.0;
    double mMix = 1.0;
};

constexpr bool operator==(const DeepFilterNetSettings& a, const DeepFilterNetSettings& b)
{
    return a.mAttenuationLimitDb == b.mAttenuationLimitDb
           && a.mMix == b.mMix;
}

constexpr bool operator!=(const DeepFilterNetSettings& a, const DeepFilterNetSettings& b)
{
    return !(a == b);
}

class DeepFilterNetEffect : public EffectWithSettings<DeepFilterNetSettings, StatefulEffect>
{
public:
    static const ComponentInterfaceSymbol Symbol;

    DeepFilterNetEffect() = default;
    ~DeepFilterNetEffect() override = default;

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    ::EffectType GetType() const override;
    EffectGroup GetGroup() const override { return EffectGroup::NoiseRemovalAndRepair; }

    const EffectParameterMethods& Parameters() const override;
    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;
    bool Process(::EffectInstance& instance, EffectSettings& settings) override;

    static constexpr EffectParameter attenuationLimit {
        &DeepFilterNetSettings::mAttenuationLimitDb,
        L"AttenuationLimit",
        100.0,
        0.0,
        100.0,
        1.0
    };

    static constexpr EffectParameter mix {
        &DeepFilterNetSettings::mMix,
        L"Mix",
        1.0,
        0.0,
        1.0,
        0.01
    };

private:
    bool ProcessOne(
        WaveClipChannel& clip, double t0, double t1, const DeepFilterNetSettings& settings, int count);
};
}
