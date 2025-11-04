/*
* Audacity: A Digital Audio Editor
*/
#include "global/realfn.h"

#include "dblogrulerutils.h"

namespace au::projectscene::dblogrulerutils {
bool isBold(double value, double dbRange)
{
    return muse::RealIsEqual(value, 0.0) || muse::RealIsEqual(value, dbRange);
}

double valueToPosition(double value, double height, double dbRange, bool isNegativeSample)
{
    double middlePosition = height / 2.0;

    if (muse::RealIsEqual(value, dbRange)) {
        return middlePosition;
    }

    if (isNegativeSample) {
        return height - (middlePosition * (value / dbRange));
    }

    return middlePosition * (value / dbRange);
}

std::pair<double, double> stepsIncrement(double height, const StepSettings& settings)
{
    std::pair<double, double> increment = settings.defaultIncrement;
    for (const auto& stepInc : settings.stepIncrements) {
        const auto& [fs, ss] = stepInc;
        if ((valueToPosition(settings.dbRange * fs, height, settings.dbRange,
                             false) - valueToPosition(0, height, settings.dbRange, false) >= settings.minAdjacentFullStepsHeight)
            && (valueToPosition(settings.dbRange * ss, height, settings.dbRange,
                                false) - valueToPosition(0, height, settings.dbRange, false) >= settings.minAdjacentSmallStepsHeight)) {
            increment = stepInc;
            break;
        }
    }

    return increment;
}

std::vector<double> fullStepsValues(double height, const StepSettings& settings)
{
    const std::pair<double, double> increment = stepsIncrement(height, settings);

    std::vector<double> steps;
    for (double v = settings.dbRange; v <= 0; v += (increment.first * std::abs(settings.dbRange))) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> smallStepsValues(double height, const StepSettings& settings)
{
    const std::pair<double, double> increment = stepsIncrement(height, settings);

    std::vector<double> steps;
    for (double v = settings.dbRange; v <= 0; v += (increment.second * std::abs(settings.dbRange))) {
        steps.push_back(v);
    }
    return steps;
}
}
