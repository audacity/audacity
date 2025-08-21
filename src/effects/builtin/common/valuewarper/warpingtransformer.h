/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "valuewarpertypes.h"

namespace au::effects {
class WarpingTransformer : public IValueTransformer
{
public:
    WarpingTransformer(ValueWarpingType warpingType);

    double forward(double x, double min, double max) const override;
    double inverse(double y, double min, double max) const override;

private:
    const double m_C;
    const double m_expC;
};
} // namespace au::effects
