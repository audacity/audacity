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

namespace {
constexpr double MIN_VOLUME_LIN = 0.0;
constexpr double MAX_VOLUME_LIN = 1.0;

constexpr int LOW_RESOLUTION_METER_THRESHOLD = 400;
constexpr int HIGH_RESOLUTION_METER_THRESHOLD = 1500;

constexpr int MIN_STEP_DISTANCE = 50;

struct StepValue {
    int threshold;
    double fullStep;
    double smallStep;
};

StepValue roundUpToFixedValue(int meterSize)
{
    const auto fullStepsInc = { 0.05, 0.1, 0.2, 0.25, 0.5 };

    for (const auto& step : fullStepsInc) {
        if (meterSize / MIN_STEP_DISTANCE >= MAX_VOLUME_LIN / step) {
            return { meterSize, step, 0.0 };
        }
    }

    return { meterSize, 0.1, 0.0 };
}
}

LinearMeter::LinearMeter(int meterSize, double dbRange)
    : m_meterSize(meterSize), m_dbRange(dbRange)
{
}

void LinearMeter::setMeterSize(int meterSize)
{
    m_meterSize = meterSize;
}

void LinearMeter::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

double LinearMeter::stepToPosition(double step) const
{
    const double clampedValue = std::clamp(step, MIN_VOLUME_LIN, MAX_VOLUME_LIN);
    return (clampedValue - MIN_VOLUME_LIN) / (MAX_VOLUME_LIN - MIN_VOLUME_LIN);
}

double LinearMeter::sampleToPosition(double sample) const
{
    if ((sample < m_dbRange) || muse::is_equal(sample, m_dbRange)) {
        return MIN_VOLUME_LIN;
    }

    return stepToPosition(muse::db_to_linear(sample));
}

double LinearMeter::positionToSample(double position) const
{
    double value = muse::linear_to_db(position);
    return std::clamp(value, m_dbRange, 0.0);
}

std::string LinearMeter::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(2) << std::abs(sample);
    return ss.str();
}

std::vector<double> LinearMeter::fullSteps() const
{
    if (m_meterSize <= 0) {
        return {};
    }

    const StepValue selectedStep = roundUpToFixedValue(m_meterSize);

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

std::vector<double> LinearMeter::smallSteps() const
{
    if (m_meterSize <= 0) {
        return {};
    }

    const auto fullSteps = this->fullSteps();

    const StepValue selectedStep = roundUpToFixedValue(m_meterSize);

    if (selectedStep.smallStep == 0) {
        return {};
    }

    std::vector<double> steps;
    double value = MIN_VOLUME_LIN;

    while (value < MAX_VOLUME_LIN) {
        bool isCloseToFullStep = std::any_of(fullSteps.begin(), fullSteps.end(),
                                             [value](double fullStep) {
            return std::abs(fullStep - value) < 0.001;
        });

        if (!isCloseToFullStep) {
            steps.push_back(value);
        }
        value += selectedStep.smallStep;
    }

    return steps;
}
