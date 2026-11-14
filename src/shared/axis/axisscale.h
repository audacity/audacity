/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::shared {
enum class AxisScale {
    Undefined = -1,
    Linear,
    Logarithmic,
    Mel,
    Bark,
    ERB,
    Period,
    _count
};
}
