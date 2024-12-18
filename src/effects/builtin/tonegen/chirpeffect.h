/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/ToneGenBase.h"
#include "../common/generatoreffect.h"
#include "toneeffect.h"

namespace au::effects {
class ChirpEffect : public ToneEffect
{
public:
    ChirpEffect();

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;
};
}
