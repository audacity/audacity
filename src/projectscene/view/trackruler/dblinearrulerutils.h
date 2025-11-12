/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene::dblinearrulerutils {
double valueToPosition(double value, double height, double maxValue, bool isNegativeSample = false);
int computeLowestFullStepValue(double height, double m_dbRange, double maxValue, int minHeightToZero);
std::vector<int> fullStepsValues(double height, double dbRange, double maxValue, int minHeightToZero, int minAdjacentStepsHeight,
                                 const std::vector<int>& fullStepSizes);
}
