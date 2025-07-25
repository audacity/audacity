/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "libraries/lib-builtin-effects/EqualizationBase.h"

namespace au::effects {
class GraphicEq : public EqualizationBase
{
public:
    using EqualizationBase::EqualizationBase;

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;
};
}
