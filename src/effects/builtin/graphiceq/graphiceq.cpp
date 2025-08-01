/*
 * Audacity: A Digital Audio Editor
 */
#include "graphiceq.h"

namespace au::effects {
const ComponentInterfaceSymbol GraphicEq::Symbol{ wxT("Graphic EQ"), XO("Graphic EQ") };

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
