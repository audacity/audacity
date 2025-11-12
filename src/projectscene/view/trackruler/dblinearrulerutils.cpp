/*
* Audacity: A Digital Audio Editor
*/
#include "global/types/ratio.h"

#include "dblinearrulerutils.h"

namespace au::projectscene::dblinearrulerutils {
double valueToPosition(double value, double height, double maxValue, bool isNegativeSample)
{
    const double maxLinearValue = std::abs(muse::db_to_linear(maxValue));
    const double minLinearValue = -maxLinearValue;
    const double linearValue = muse::db_to_linear(value) * (isNegativeSample ? -1.0 : 1.0);
    const double clampedLinearValue = std::clamp(linearValue, minLinearValue, maxLinearValue);

    return (0.5 - clampedLinearValue / (maxLinearValue - minLinearValue)) * height;
}

int computeLowestFullStepValue(double height, double m_dbRange, double maxValue, int minHeightToZero)
{
    auto const middlePoint = height / 2.0;
    int lowestFullStep = static_cast<int>(m_dbRange);
    for (int i = lowestFullStep + 3; i < 0; i += 3) {
        const double position = valueToPosition(i, height, maxValue);
        if (middlePoint - position > minHeightToZero) {
            return lowestFullStep;
        }
        lowestFullStep = i;
    }

    return 0;
}

std::vector<int> fullStepsValues(double height, double dbRange, double maxValue, int minHeightToZero, int minAdjacentStepsHeight,
                                 const std::vector<int>& fullStepSizes)
{
    const int lowestFullStep = computeLowestFullStepValue(height, dbRange, maxValue, minHeightToZero);

    std::vector<int> steps;
    if (lowestFullStep != 0) {
        steps.push_back(lowestFullStep);
        int previousValidStep = lowestFullStep;

        for (int step = lowestFullStep + 1; step < maxValue; step++) {
            const int diff = step - previousValidStep;
            if (std::find(fullStepSizes.begin(), fullStepSizes.end(), diff) == fullStepSizes.end()) {
                continue;
            }

            const double position = valueToPosition(step, height, maxValue);
            if (position < minHeightToZero) {
                break;
            }

            const double previousPosition = valueToPosition(previousValidStep, height, maxValue);
            if (previousPosition - position >= minAdjacentStepsHeight) {
                steps.push_back(step);
                previousValidStep = step;
            }
        }
    }

    return steps;
}
}
