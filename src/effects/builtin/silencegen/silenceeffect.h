/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/SilenceBase.h"
#include "../common/generatoreffect.h"

struct EffectSettings;

namespace au::effects {
class SilenceEffect : public ::SilenceBase, public GeneratorEffect
{
public:
    SilenceEffect();

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;

private:
    void doInit() override;
};
}
