/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/generatoreffect.h"
#include "libraries/lib-builtin-effects/ToneGenBase.h"
#include "toneeffect.h"

namespace au::effects {
class ChirpEffect : public ToneEffect
{
public:
    ChirpEffect();

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;
};
}  // namespace au::effects
