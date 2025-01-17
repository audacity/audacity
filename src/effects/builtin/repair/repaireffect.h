/**********************************************************************

  Audacity: A Digital Audio Editor

  Repair.h

  Dominic Mazzoni

**********************************************************************/
#pragma once

#include "libraries/lib-effects/StatefulEffect.h"

class WaveChannel;

class BUILTIN_EFFECTS_API Repair : public StatefulEffect
{
public:
    static const ComponentInterfaceSymbol Symbol;

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    bool IsInteractive() const override;

    // Effect implementation

    bool Process(EffectInstance& instance, EffectSettings& settings) override;

    bool NeedsDither() const override;

private:
    // EffectRepair implementation

    bool ProcessOne(
        int count, WaveChannel& track, sampleCount start, size_t len, size_t repairStart, // offset relative to start
        size_t repairLen);
};
