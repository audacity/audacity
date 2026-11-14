/*
* Audacity: A Digital Audio Editor
*/
#include "changepitcheffect.h"

#include "au3-strings/TranslatableString.h"
#include "au3-effects/LoadEffects.h"

namespace au::effects {
const ComponentInterfaceSymbol ChangePitchEffect::Symbol { TranslatableString("effects-changepitch", "Change pitch") };

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
