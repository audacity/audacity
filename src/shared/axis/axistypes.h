/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <vector>

namespace au::shared {
struct AxisTick {
    double val = 0.;
    double position = 0.;   ///< Fractional position along the axis, in [0, 1].
};

struct AxisTicks {
    std::vector<AxisTick> major;
    std::vector<AxisTick> minor;
};
}
