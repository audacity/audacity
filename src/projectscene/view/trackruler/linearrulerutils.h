/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene::linearrulerutils {
bool isBold(double value);
int getAlignment(double value);
double valueToPosition(double value, double height);
std::pair<double, double> stepsIncrement(double height);
std::vector<double> fullStepsValues(double height);
std::vector<double> smallStepsValues(double height);
}
