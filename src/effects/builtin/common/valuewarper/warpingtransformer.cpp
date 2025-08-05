/*
 * Audacity: A Digital Audio Editor
 */

#include "warpingtransformer.h"

#include "global/types/number.h"

#include <cmath>

namespace au::effects {
namespace {
struct Normalized {
    Normalized(double v, double min, double max)
        : value{(std::clamp(v, min, max) - min) / (max - min)} {}
    const double value;
};

struct Expanded {
    Expanded(double v, double min, double max)
        : value{v* (max - min) + min} {}
    const double value;
};

/*!
 * We use a warping function that maps f(min) = min and f(max) = max.
 * For simpler maths, we first normalize the warped value to [0, 1] range.
 * Then we use `y(x) = (exp(C * x) - 1) / (exp(C) - 1)`.
 * `softC` is solving this equation for `y(1/2) = 1/4`, meaning that when the control is at 50%, the warped value is at 25%.
 * `aggressiveC` is solving this equation so that when the control is at 50%, the warped value is at 12.5%.
 */
constexpr double softC = 2.1972245773362196;
constexpr double aggressiveC = 3.8918202981106265;
static const double softExpC = std::exp(softC);
static const double aggressiveExpC = std::exp(aggressiveC);
}

WarpingTransformer::WarpingTransformer(ValueWarpingType warpingType)
    : m_C{warpingType == ValueWarpingType::Aggressive ? aggressiveC : softC}
    , m_expC{warpingType == ValueWarpingType::Aggressive ? aggressiveExpC : softExpC}
{}

double WarpingTransformer::forward(double x, double min, double max) const
{
    if (muse::is_equal(min, max)) {
        return 0.0;
    }

    const Normalized u{ x, min, max };
    const auto v = 1 / m_C * std::log(u.value * (m_expC - 1.0) + 1.0);
    const Expanded y{ v, min, max };

    return y.value;
}

double WarpingTransformer::inverse(double y, double min, double max) const
{
    if (muse::is_equal(min, max)) {
        return 0.0;
    }

    const Normalized v{ y, min, max };
    const auto u = (std::exp(m_C * v.value) - 1.0) / (m_expC - 1.0);
    const Expanded x{ u, min, max };

    return x.value;
}
}
