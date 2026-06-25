/*
 * Audacity: A Digital Audio Editor
 */
#include "axisticks.h"

#include "numberscale.h"

#include "framework/global/log.h"
#include "framework/global/types/number.h"

#include <algorithm>
#include <cmath>

namespace au {
namespace {
// Sorted by decreasing value.
std::vector<double> getTicksValues(double maxVal, double minVal, double step, shared::AxisScale scale)
{
    using namespace shared;

    std::vector<double> values;
    auto tick = std::floor(maxVal / step) * step;
    while (tick >= minVal) {
        if (scale == AxisScale::Logarithmic && tick <= 0) {
            break;
        }
        values.push_back(tick);
        tick -= step;
    }
    return values;
}

std::vector<shared::AxisTick> toTicks(const std::vector<double>& values, const shared::NumberScale& numberScale)
{
    using namespace shared;

    std::vector<AxisTick> ticks;
    ticks.reserve(values.size());
    std::transform(values.begin(), values.end(), std::back_inserter(ticks), [&numberScale](double value) {
        return AxisTick { value, static_cast<double>(numberScale.valueToPosition(static_cast<float>(value))) };
    });

    // Sort ascending by position so the overlap-rejection loop is orientation-agnostic.
    std::sort(ticks.begin(), ticks.end(), [](const AxisTick& a, const AxisTick& b) {
        return a.position < b.position;
    });

    return ticks;
}

std::vector<shared::AxisTick> getTicks(double min, double max, shared::AxisScale scale, const shared::NumberScale& numberScale, double step)
{
    const std::vector<double> values = getTicksValues(max, min, step, scale);
    return toTicks(values, numberScale);
}
}

shared::AxisTicks shared::axisTicks(double min, double max, AxisScale scale)
{
    const auto range = max - min;
    if (range <= 0) {
        LOGE() << "Invalid range: " << range;
        return {};
    }

    const auto safeMin = scale == AxisScale::Logarithmic ? std::max(min, 1e-6) : min;
    const auto numberScale = NumberScale{ scale, safeMin, max };

    auto minorPerMajor = 10;

    auto majorStep = std::pow(10, std::floor(std::log10(range)));
    if (muse::is_equal(majorStep, range)) {
        majorStep = range / 10;
    }
    // Too sparse major ticks don't look good. Double the rate of major ticks,
    // but preserve 10 steps per decade.
    if (range / majorStep < kMinMajorTicks) {
        majorStep /= 2;
        minorPerMajor = 5;
    }

    // Major ticks: ensure min and max are bookends, on top of the on-grid values.
    auto majorValues = getTicksValues(max, safeMin, majorStep, scale);
    if (majorValues.empty() || !muse::is_equal(majorValues.front(), max)) {
        majorValues.insert(majorValues.begin(), max);
    }
    if (!muse::is_equal(majorValues.back(), safeMin)) {
        majorValues.push_back(safeMin);
    }
    const std::vector<AxisTick> majorTicks = toTicks(majorValues, numberScale);

    const auto minorStep = majorStep / minorPerMajor;
    std::vector<AxisTick> minorTicks = getTicks(safeMin, max, scale, numberScale, minorStep);

    // Remove minor ticks that overlap with major ticks.
    minorTicks.erase(std::remove_if(minorTicks.begin(), minorTicks.end(), [&majorTicks](const AxisTick& minorTick) {
        return std::any_of(majorTicks.begin(), majorTicks.end(), [&minorTick](const AxisTick& majorTick) {
            return muse::is_equal(minorTick.val, majorTick.val);
        });
    }),
                     minorTicks.end());

    return { majorTicks, minorTicks };
}
}
