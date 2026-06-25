/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-builtin-effects/EqualizationBase.h"

namespace au::effects {
class FilterCurveEq : public EqualizationBase
{
public:
    FilterCurveEq();

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;

    int GetOptions() const;
};
}
