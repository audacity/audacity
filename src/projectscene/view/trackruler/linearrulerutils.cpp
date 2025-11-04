/*
* Audacity: A Digital Audio Editor
*/
#include "global/realfn.h"

#include "linearrulerutils.h"

constexpr std::array<std::pair<double, double>, 4> STEP_INCREMENT = { { { 0.1, 0.05 }, { 0.5, 0.1 }, { 0.5, 0.25 }, { 1.0, 0.5 } } };
constexpr std::pair<double, double> DEFAULT_INCREMENT = { 1.0, 0.5 };
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 20;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 10;

constexpr double MAX_VALUE = 1.0;
constexpr double MIN_VALUE = -1.0;

namespace au::projectscene::linearrulerutils {
bool isBold(double value)
{
    return muse::RealIsEqual(value, 1.0) || muse::RealIsEqual(value, 0.0) || muse::RealIsEqual(value, -1.0);
}

int getAlignment(double value)
{
    if (muse::RealIsEqual(value, 1.0)) {
        return -1;
    }

    if (muse::RealIsEqual(value, -1.0)) {
        return 1;
    }

    return 0;
}

double valueToPosition(double value, double height)
{
    return (0.5 - value / 2.0) * height;
}

std::pair<double, double> stepsIncrement(double height)
{
    std::pair<double, double> increment = DEFAULT_INCREMENT;
    for (const auto& stepInc : STEP_INCREMENT) {
        const auto& [fs, ss] = stepInc;
        if ((valueToPosition(0.0, height) - valueToPosition(fs, height) >= MIN_ADJACENT_FULL_STEPS_HEIGHT)
            && (valueToPosition(0.0, height) - valueToPosition(ss, height) >= MIN_ADJACENT_SMALL_STEPS_HEIGHT)) {
            increment = stepInc;
            break;
        }
    }

    return increment;
}

std::vector<double> fullStepsValues(double height)
{
    const std::pair<double, double> increment = stepsIncrement(height);
    std::vector<double> steps;
    for (double v = MIN_VALUE; v <= MAX_VALUE; v += increment.first) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> smallStepsValues(double height)
{
    const std::pair<double, double> increment = stepsIncrement(height);
    std::vector<double> steps;
    for (double v = MIN_VALUE; v <= MAX_VALUE; v += increment.second) {
        steps.push_back(v);
    }
    return steps;
}
}
