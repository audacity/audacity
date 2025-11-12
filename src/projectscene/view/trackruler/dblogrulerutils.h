/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene::dblogrulerutils {
struct StepSettings {
    double dbRange;
    double minAdjacentFullStepsHeight;
    double minAdjacentSmallStepsHeight;
    double maxDisplayValue;
};

bool isBold(double value, double maxValue, double dbRange);
double valueToPosition(double value, double height, double dbRange, double maxDisplayValue, bool isNegativeSample);
std::pair<double, double> stepsIncrement(double height, const StepSettings& settings);
std::vector<double> fullStepsValues(double height, const StepSettings& settings);
std::vector<double> smallStepsValues(double height, const StepSettings& settings);
}
