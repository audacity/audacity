/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/ToneGenBase.h"

namespace au::effects {
class ChirpEffect : public ::ToneGenBase
{
public:
    ChirpEffect();

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;
};
}
