/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene::dblogrulerutils {
struct StepSettings {
    double dbRange;
    double minAdjacentFullStepsHeight;
    double minAdjacentSmallStepsHeight;
    std::pair<double, double> defaultIncrement;
    std::vector<std::pair<double, double> > stepIncrements;
};

bool isBold(double value, double dbRange);
double valueToPosition(double value, double height, double dbRange, bool isNegativeSample);
std::pair<double, double> stepsIncrement(double height, const StepSettings& settings);
std::vector<double> fullStepsValues(double height, const StepSettings& settings);
std::vector<double> smallStepsValues(double height, const StepSettings& settings);
}
