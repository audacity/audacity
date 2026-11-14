/*
 * Audacity: A Digital Audio Editor
 */
#include "filtercurveeq.h"

#include "au3-strings/TranslatableString.h"

namespace au::effects {
const ComponentInterfaceSymbol FilterCurveEq::Symbol{ wxT("Filter Curve"), TranslatableString("effects-filtercurveeq", "Filter Curve EQ") };

FilterCurveEq::FilterCurveEq()
    : EqualizationBase{kEqOptionCurve}
{
}

ComponentInterfaceSymbol FilterCurveEq::GetSymbol() const
{
    return Symbol;
}

int FilterCurveEq::GetOptions() const
{
    return mOptions;
}
}
