/*
* Audacity: A Digital Audio Editor
*/

#include "linearmeter.h"

#include "MemoryX.h"

#include <sstream>
#include <iomanip>
#include <cmath>
#include <algorithm>

using namespace au::playback;

static constexpr double MIN_VOLUME_DB = 0.0;
static constexpr double MAX_VOLUME_DB = 1.0;

static constexpr int LOW_RESOLUTION_METER_THRESHOLD = 400;
static constexpr int HIGH_RESOLUTION_METER_THRESHOLD = 1500;

struct StepValue {
    int threshold;
    double fullStep;
    double smallStep;
};

static StepValue roundUpToFixedValue(int meterSize)
{
    const std::vector<StepValue> steps = {
        { HIGH_RESOLUTION_METER_THRESHOLD, 0.05, 0.05 / 2 },
        { LOW_RESOLUTION_METER_THRESHOLD, 0.1, 0.1 / 3 },
        { 0, 0.25, 0.25 / 3 }
    };

    for (const auto& step : steps) {
        if (meterSize >= step.threshold) {
            return step;
        }
    }

    return steps.back();
}

double LinearMeter::stepToPosition(double step) const
{
    const double clampedValue = std::clamp(step, MIN_VOLUME_DB, MAX_VOLUME_DB);
    return (clampedValue - MIN_VOLUME_DB) / (MAX_VOLUME_DB - MIN_VOLUME_DB);
}

double LinearMeter::sampleToPosition(double sample) const
{
    const double linearValue = DB_TO_LINEAR(sample);
    const double clampedValue = std::clamp(linearValue, MIN_VOLUME_DB, MAX_VOLUME_DB);
    return (clampedValue - MIN_VOLUME_DB) / (MAX_VOLUME_DB - MIN_VOLUME_DB);
}

std::string LinearMeter::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(2) << std::abs(sample);
    return ss.str();
}

std::vector<double> LinearMeter::fullSteps(int meterSize) const
{
    if (meterSize <= 0) {
        return {};
    }

    const StepValue stepValue = roundUpToFixedValue(meterSize);

    if (stepValue.fullStep == 0) {
        return {};
    }

    std::vector<double> steps;
    double value = MIN_VOLUME_DB;

    while (value <= MAX_VOLUME_DB) {
        steps.push_back(value);
        value += stepValue.fullStep;
    }

    return steps;
}

std::vector<double> LinearMeter::smallSteps(int meterSize) const
{
    if (meterSize <= 0) {
        return {};
    }

    const auto fullStepsValue = fullSteps(meterSize);

    const StepValue stepValue = roundUpToFixedValue(meterSize);

    if (stepValue.smallStep == 0) {
        return {};
    }

    std::vector<double> steps;
    double value = MIN_VOLUME_DB;

    while (value < MAX_VOLUME_DB) {
        if (std::find(fullStepsValue.begin(), fullStepsValue.end(), value) == fullStepsValue.end()) {
            steps.push_back(value);
        }
        value += stepValue.smallStep;
    }

    return steps;
}
