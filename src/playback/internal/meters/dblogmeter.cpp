/*
* Audacity: A Digital Audio Editor
*/

#include "dblogmeter.h"

#include <cmath>
#include <sstream>
#include <iomanip>
#include <algorithm>

using namespace au::playback;

static constexpr double MIN_VOLUME_DB = -60.0;
static constexpr double MAX_VOLUME_DB = 0.0;

static constexpr int LOW_RESOLUTION_METER_THRESHOLD = 400;
static constexpr int HIGH_RESOLUTION_METER_THRESHOLD = 1500;
static constexpr int MEDIUM_RESOLUTION_METER_THRESHOLD = 700;

struct StepValues {
    int threshold;
    int fullStep;
    int smallStep;
};

static StepValues roundUpToFixedValue(int meterSize)
{
    const std::vector<StepValues> steps = {
        { HIGH_RESOLUTION_METER_THRESHOLD, 1, 0 },
        { MEDIUM_RESOLUTION_METER_THRESHOLD, 3, 1 },
        { LOW_RESOLUTION_METER_THRESHOLD, 6, 2 },
        { 0, 12, 3 }
    };

    for (const auto& step : steps) {
        if (meterSize >= step.threshold) {
            return step;
        }
    }

    return steps.back();
}

double DbLogMeter::stepToPosition(double step) const
{
    const double clampedValue = std::clamp(step, MIN_VOLUME_DB, MAX_VOLUME_DB);

    return (clampedValue - MIN_VOLUME_DB) / (MAX_VOLUME_DB - MIN_VOLUME_DB);
}

double DbLogMeter::sampleToPosition(double sample) const
{
    return stepToPosition(sample);
}

std::string DbLogMeter::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(0) << std::abs(sample);
    return ss.str();
}

std::vector<double> DbLogMeter::fullSteps(int meterSize) const
{
    if (meterSize <= 0) {
        return {};
    }

    const StepValues stepValues = roundUpToFixedValue(meterSize);

    if (stepValues.fullStep == 0) {
        return {};
    }

    std::vector<double> steps;
    int value = MIN_VOLUME_DB;

    while (value <= MAX_VOLUME_DB) {
        steps.push_back(value);
        value += stepValues.fullStep;
    }

    return steps;
}

std::vector<double> DbLogMeter::smallSteps(int meterSize) const
{
    if (meterSize <= 0) {
        return {};
    }

    const StepValues stepValues = roundUpToFixedValue(meterSize);

    if (stepValues.smallStep == 0) {
        return {};
    }

    std::vector<double> steps;
    int value = MIN_VOLUME_DB;

    while (value < MAX_VOLUME_DB) {
        if (value % stepValues.fullStep != 0) {
            steps.push_back(value);
        }
        value += stepValues.smallStep;
    }

    return steps;
}
