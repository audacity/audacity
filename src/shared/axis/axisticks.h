/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "axisscale.h"
#include "axistypes.h"

namespace au::shared {
/** Minimum number of major ticks returned by `axisTicks()`. */
static constexpr auto kMinMajorTicks = 4;

/**
 * @brief Picks "good" round-number major and minor ticks across [min, max]
 *        under the given scale, dropping any that would visually overlap.
 *
 * @param min         Lower end of the data range.
 * @param max         Upper end of the data range.
 * @param scale       Scale used to map values to positions (linear, log, ...).
 *
 * @return Major and minor ticks to display on the axis, with values and positions.
 * Major steps contain at least `kMinMajorTicks` ticks.
 */
AxisTicks axisTicks(double min, double max, AxisScale scale);
}
