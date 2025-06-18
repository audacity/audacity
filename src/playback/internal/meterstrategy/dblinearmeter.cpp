/*
* Audacity: A Digital Audio Editor
*/

#include "dblinearmeter.h"

#include <cmath>
#include <sstream>
#include <iomanip>
#include <algorithm>

static constexpr double MIN_VOLUME_DB = -60.0;
static constexpr double MAX_VOLUME_DB = 0.0;

using namespace au::playback;

static double dbToValuePercentage(double dbValue)
{
    if (dbValue > 0) {
        return 0;
    } else if (dbValue > -20.0) {
        return (100 + 2.5 * dbValue) / 100.0;
    } else if (dbValue > -30.0) {
        return (50 + 2.0 * (dbValue + 20.0)) / 100.0;
    } else if (dbValue > -40.0) {
        return (30 + 1.5 * (dbValue + 30.0)) / 100.0;
    } else if (dbValue > -50.0) {
        return (15 + 0.75 * (dbValue + 40.0)) / 100.0;
    } else if (dbValue > -60.0) {
        return (7.5 + 0.5 * (dbValue + 50.0)) / 100.0;
    } else {
        return 0.0;
    }
}

double DbLinearMeter::stepToPosition(double step) const
{
    const double clampedValue = std::clamp(step, MIN_VOLUME_DB, MAX_VOLUME_DB);
    return dbToValuePercentage(clampedValue);
}

double DbLinearMeter::sampleToPosition(double sample) const
{
    if (sample == -60) {
        return 0.0;
    }

    return stepToPosition(sample);
}

std::string DbLinearMeter::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(0) << std::abs(sample);
    return ss.str();
}

std::vector<double> DbLinearMeter::fullSteps(int meterSize) const
{
    if (meterSize < 500) {
        return { -60, -30, -20, -15, -10, -5, 0 };
    }

    return { 0.0, -3.0, -6.0, -9.0, -12.0, -18.0, -24.0, -30.0, -40.0, -50.0, -60.0 };
}

std::vector<double> DbLinearMeter::smallSteps(int meterSize) const
{
    if (meterSize < 500) {
        return {};
    }

    return { -1, -2, -4, -5, -7, -8, -10, -11, -13.5, -15.0, -16.5, -20, -22, -26, -28, -35, -45 };
}
