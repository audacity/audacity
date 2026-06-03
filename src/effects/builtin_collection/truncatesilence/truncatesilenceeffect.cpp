/*
* Audacity: A Digital Audio Editor
*/
#include "truncatesilenceeffect.h"

#include "au3-effects/LoadEffects.h"
#include "au3-strings/TranslatableString.h"

namespace au::effects {
const ComponentInterfaceSymbol TruncateSilenceEffect::Symbol { TranslatableString("effects-truncatesilence", "Truncate silence") };

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
