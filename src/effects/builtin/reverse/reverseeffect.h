#pragma once

#include "libraries/lib-effects/StatefulEffect.h"

namespace au::effects {
class ReverseEffect : public StatefulEffect
{
public:
    ReverseEffect();
    virtual ~ReverseEffect();

    static const ComponentInterfaceSymbol Symbol;

    // ComponentInterface implementation
    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    // EffectDefinitionInterface implementation
    EffectType GetType() const override;
    bool IsInteractive() const override;

    // Effect implementation
    bool Process(::EffectInstance& instance, ::EffectSettings& settings) override;
};
}
