/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-effects/StatefulEffect.h"

namespace au::effects {
class DeepFilterNet3Effect final : public StatefulEffect
{
public:
    static inline DeepFilterNet3Effect*
    FetchParameters(DeepFilterNet3Effect& effect, EffectSettings&)
    {
        return &effect;
    }

    static const ComponentInterfaceSymbol Symbol;

    DeepFilterNet3Effect();

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ::EffectType GetType() const override;
    EffectGroup GetGroup() const override { return EffectGroup::NoiseRemovalAndRepair; }
    bool IsInteractive() const override { return false; }

    bool Process(::EffectInstance& instance, ::EffectSettings& settings) override;

private:
    const EffectParameterMethods& Parameters() const override;
};
}
