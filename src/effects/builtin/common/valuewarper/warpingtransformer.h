/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "valuewarpertypes.h"

namespace au::effects {
class WarpingTransformer : public IValueTransformer
{
public:
    WarpingTransformer(double min, double middle, double max);

    double forward(double x) const override;
    double inverse(double y) const override;

private:
    const double m_C;
    const double m_expC;
    const double m_min;
    const double m_max;
};
} // namespace au::effects
