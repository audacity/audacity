#include "TimeAndPitchInterface.h"

#include <cmath>

TimeAndPitchSource::~TimeAndPitchSource() = default;

TimeAndPitchInterface::~TimeAndPitchInterface() = default;

bool TimeAndPitchInterface::IsPassThroughMode(double stretchRatio)
{
    return std::fabs(stretchRatio - 1.) < 1e-6;
}
