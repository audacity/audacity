/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene::dblinearrulerutils {
double valueToPosition(double value, double height, bool isNegativeSample = false);
int computeLowestFullStepValue(double height, double m_dbRange, int minHeightToZero);
std::vector<int> fullStepsValues(double height, double dbRange, int minHeightToZero, int minAdjacentStepsHeight,
                                 const std::vector<int>& fullStepSizes);
}
