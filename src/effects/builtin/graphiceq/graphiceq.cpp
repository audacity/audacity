/*
 * Audacity: A Digital Audio Editor
 */
#include "graphiceq.h"

namespace au::effects {
const ComponentInterfaceSymbol GraphicEq::Symbol{ wxT("Graphic EQ"), XO("Graphic EQ") };

ComponentInterfaceSymbol GraphicEq::GetSymbol() const
{
    return Symbol;
}
}
