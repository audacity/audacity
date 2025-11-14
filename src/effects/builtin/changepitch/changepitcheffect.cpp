/*
* Audacity: A Digital Audio Editor
*/
#include "changepitcheffect.h"
#include "LoadEffects.h"

namespace au::effects {
const ComponentInterfaceSymbol ChangePitchEffect::Symbol { XO("Change Pitch") };

ChangePitchEffect::ChangePitchEffect()
{
}

ChangePitchEffect::~ChangePitchEffect()
{
}

ComponentInterfaceSymbol ChangePitchEffect::GetSymbol() const
{
    return Symbol;
}
}
