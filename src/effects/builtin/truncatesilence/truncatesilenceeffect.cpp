/*
* Audacity: A Digital Audio Editor
*/
#include "truncatesilenceeffect.h"
#include "LoadEffects.h"

namespace au::effects {
const ComponentInterfaceSymbol TruncateSilenceEffect::Symbol { XO("Truncate Silence") };

TruncateSilenceEffect::TruncateSilenceEffect()
{
}

TruncateSilenceEffect::~TruncateSilenceEffect()
{
}

ComponentInterfaceSymbol TruncateSilenceEffect::GetSymbol() const
{
    return Symbol;
}
}
