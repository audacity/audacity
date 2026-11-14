/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "axistypes.h"

#include <string>
#include <vector>

namespace au::shared {
/**
 * @brief Builds labels for the given ticks using the smallest number of
 *        decimal digits that keeps adjacent labels distinct.
 *
 * @param ticks            Ticks to label, expected sorted by value.
 * @param labelSize        Size of the label.
 * @param axisSize         Size of the axis.
 * @param maxDecimalDigits Cap on the number of decimal digits to try.
 */
std::vector<std::string> labelsForTicks(const std::vector<AxisTick>& ticks, double labelSize, double axisSize, int maxDecimalDigits = 3);
}
