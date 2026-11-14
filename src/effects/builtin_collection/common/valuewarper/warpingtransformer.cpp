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
 * Now we let y(1/2) = middle, i.e., the value when in the middle, and solve the equation for C:
 * C = 2 * log((1 - u) / u)
 */
double solveC(double min, double middle, double max)
{
    const double u = Normalized{ middle, min, max }.value;
    const auto C = 2 * std::log((1 - u) / u);
    return C;
}
}

WarpingTransformer::WarpingTransformer(double min, double middle, double max)
    : m_C{solveC(min, middle, max)}
    , m_expC{std::exp(m_C)},
    m_min{min},
    m_max{max}
{}

double WarpingTransformer::forward(double x) const
{
    if (muse::is_equal(m_min, m_max)) {
        return 0.0;
    }

    const Normalized u{ x, m_min, m_max };
    const auto v = 1 / m_C * std::log(u.value * (m_expC - 1.0) + 1.0);
    const Expanded y{ v, m_min, m_max };

    return y.value;
}

double WarpingTransformer::inverse(double y) const
{
    if (muse::is_equal(m_min, m_max)) {
        return 0.0;
    }

    const Normalized v{ y, m_min, m_max };
    const auto u = (std::exp(m_C * v.value) - 1.0) / (m_expC - 1.0);
    const Expanded x{ u, m_min, m_max };

    return x.value;
}
}
