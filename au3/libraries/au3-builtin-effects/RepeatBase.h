#pragma once

#include "ShuttleAutomation.h"
#include "StatefulEffect.h"

class BUILTIN_EFFECTS_API RepeatBase : public StatefulEffect
{
public:
    static inline RepeatBase*
    FetchParameters(RepeatBase& e, EffectSettings&) { return &e; }
    static const ComponentInterfaceSymbol Symbol;

    RepeatBase();
    virtual ~RepeatBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    // Effect implementation

    bool Process(EffectInstance& instance, EffectSettings& settings) override;

    bool NeedsDither() const override;

protected:
    // RepeatBase implementation
    int repeatCount;

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Count{ &RepeatBase::repeatCount,
                                            L"Count", 1,  1,    INT_MAX,  1 };
};
