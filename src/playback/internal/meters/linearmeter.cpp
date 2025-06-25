/*
* Audacity: A Digital Audio Editor
*/

#include "linearmeter.h"

#include "global/types/ratio.h"

#include <sstream>
#include <iomanip>
#include <cmath>
#include <algorithm>

using namespace au::playback;

static constexpr double MIN_VOLUME_LIN = 0.0;
static constexpr double MAX_VOLUME_LIN = 1.0;

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
    const double clampedValue = std::clamp(step, MIN_VOLUME_LIN, MAX_VOLUME_LIN);
    return (clampedValue - MIN_VOLUME_LIN) / (MAX_VOLUME_LIN - MIN_VOLUME_LIN);
}

double LinearMeter::sampleToPosition(double sample) const
{
    return stepToPosition(muse::db_to_linear(sample));
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

    const StepValue selectedStep = roundUpToFixedValue(meterSize);

    if (selectedStep.fullStep == 0) {
        return {};
    }

    std::vector<double> steps;
    double value = MIN_VOLUME_LIN;

    while (value <= MAX_VOLUME_LIN) {
        steps.push_back(value);
        value += selectedStep.fullStep;
    }

    return steps;
}

std::vector<double> LinearMeter::smallSteps(int meterSize) const
{
    if (meterSize <= 0) {
        return {};
    }

    const auto fullSteps = this->fullSteps(meterSize);

    const StepValue selectedStep = roundUpToFixedValue(meterSize);

    if (selectedStep.smallStep == 0) {
        return {};
    }

    std::vector<double> steps;
    double value = MIN_VOLUME_LIN;

    while (value < MAX_VOLUME_LIN) {
        if (std::find(fullSteps.begin(), fullSteps.end(), value) == fullSteps.end()) {
            steps.push_back(value);
        }
        value += selectedStep.smallStep;
    }

    return steps;
}
