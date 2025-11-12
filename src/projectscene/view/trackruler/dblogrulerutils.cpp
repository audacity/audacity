/*
* Audacity: A Digital Audio Editor
*/
#include "global/realfn.h"

#include "dblogrulerutils.h"

namespace au::projectscene::dblogrulerutils {
bool isBold(double value, double maxValue, double dbRange)
{
    return (std::round(value) == std::round(maxValue)) || (muse::RealIsEqual(value, dbRange));
}

double valueToPosition(double value, double height, double dbRange, double maxDisplayValue, bool isNegativeSample)
{
    double middlePosition = height / 2.0;

    if (muse::RealIsEqual(value, dbRange)) {
        return middlePosition;
    }

    value = std::min(value, maxDisplayValue);
    const double percentage = (value - maxDisplayValue) / (dbRange - maxDisplayValue);

    if (isNegativeSample) {
        return height - (middlePosition * percentage);
    }

    return middlePosition * percentage;
}

std::pair<double, double> stepsIncrement(double height, const StepSettings& settings)
{
    std::pair<double, double> increment = { settings.maxDisplayValue - settings.dbRange,
                                            (settings.maxDisplayValue - settings.dbRange) / 3.0 };

    int maxDisplayDb = static_cast<int>(settings.maxDisplayValue);
    int dbRangeInt = static_cast<int>(settings.dbRange);

    for (int i = 3; i < (std::abs(dbRangeInt) - std::abs(maxDisplayDb)); i += 3) {
        double rate = static_cast<double>(maxDisplayDb - dbRangeInt) / i;
        if (!(std::abs(rate - std::round(rate)) < 1e-6)) {
            continue;
        }

        if ((valueToPosition(-i + maxDisplayDb, height, settings.dbRange,
                             settings.maxDisplayValue, false)
             - valueToPosition(maxDisplayDb, height, settings.dbRange, settings.maxDisplayValue,
                               false) >= settings.minAdjacentFullStepsHeight)) {
            increment = { i, i / 3.0 };
            break;
        }
    }

    return increment;
}

std::vector<double> fullStepsValues(double height, const StepSettings& settings)
{
    const std::pair<double, double> increment = stepsIncrement(height, settings);

    std::vector<double> steps;
    for (double v = settings.dbRange; v <= std::round(settings.maxDisplayValue); v += increment.first) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> smallStepsValues(double height, const StepSettings& settings)
{
    const std::pair<double, double> increment = stepsIncrement(height, settings);

    std::vector<double> steps;
    for (double v = settings.dbRange; v <= std::round(settings.maxDisplayValue); v += increment.second) {
        steps.push_back(v);
    }
    return steps;
}
}
