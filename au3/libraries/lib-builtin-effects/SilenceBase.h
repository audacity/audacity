/**********************************************************************

  Audacity: A Digital Audio Editor

  SilenceBase.h

  Dominic Mazzoni

**********************************************************************/
#pragma once

#include "Generator.h"

class BUILTIN_EFFECTS_API SilenceBase : public Generator
{
public:
    static const ComponentInterfaceSymbol Symbol;

    SilenceBase();
    virtual ~SilenceBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

protected:
    // Generator implementation

    bool GenerateTrack(const EffectSettings& settings, WaveTrack& tmp) override;
};
