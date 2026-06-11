/*
 * Audacity: A Digital Audio Editor
 */
#include "graphiceq.h"

#include "au3-strings/TranslatableString.h"

namespace au::effects {
const ComponentInterfaceSymbol GraphicEq::Symbol{ wxT("Graphic EQ"), TranslatableString("effects-graphiceq", "Graphic EQ") };

GraphicEq::GraphicEq()
    : EqualizationBase{kEqOptionGraphic}
{
}

ComponentInterfaceSymbol GraphicEq::GetSymbol() const
{
    return Symbol;
}

int GraphicEq::GetOptions() const
{
    return mOptions;
}
}
