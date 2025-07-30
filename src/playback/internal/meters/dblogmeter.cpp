/*
* Audacity: A Digital Audio Editor
*/

#include "dblogmeter.h"

#include <cmath>
#include <sstream>
#include <iomanip>
#include <algorithm>

using namespace au::playback;

namespace {
constexpr double MAX_VOLUME_DB = 0.0;
constexpr int MIN_STEP_DISTANCE = 30;
struct StepValues {
    int threshold;
    int fullStep;
    int smallStep;
};

StepValues roundUpToFixedValue(int meterSize, double dbRange)
{
    const std::array<std::array<int, 2>, 7> fullStepsInc = { { { 1, 0 }, { 2, 1 }, { 3, 1 }, { 6, 2 }, { 12, 3 }, { 24, 6 }, { 48, 6 } } };

    for (const auto& [fullStep, smallStep] : fullStepsInc) {
        if (meterSize / MIN_STEP_DISTANCE >= std::abs(dbRange) / fullStep) {
            return { meterSize, fullStep, smallStep };
        }
    }

    return {};
}
}

DbLogMeter::DbLogMeter(int meterSize, double dbRange)
    : m_meterSize(meterSize), m_dbRange(dbRange)
{
}

void DbLogMeter::setMeterSize(int meterSize)
{
    m_meterSize = meterSize;
}

void DbLogMeter::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

double DbLogMeter::stepToPosition(double step) const
{
    const double clampedValue = std::clamp(step, m_dbRange, MAX_VOLUME_DB);

    return (clampedValue - m_dbRange) / (MAX_VOLUME_DB - m_dbRange);
}

double DbLogMeter::sampleToPosition(double sample) const
{
    return stepToPosition(sample);
}

double DbLogMeter::positionToSample(double position) const
{
    const double clampedValue = std::clamp(position, 0.0, 1.0);
    return m_dbRange * (1.0 - clampedValue);
}

std::string DbLogMeter::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(0) << std::abs(sample);
    return ss.str();
}

std::vector<double> DbLogMeter::fullSteps() const
{
    if (m_meterSize <= 0) {
        return {};
    }

    const StepValues stepValues = roundUpToFixedValue(m_meterSize, m_dbRange);

    if (stepValues.fullStep == 0) {
        return {};
    }

    std::vector<double> steps;
    int value = m_dbRange;
    while (value <= MAX_VOLUME_DB) {
        steps.push_back(value);
        value += stepValues.fullStep;
    }

    return steps;
}

std::vector<double> DbLogMeter::smallSteps() const
{
    if (m_meterSize <= 0) {
        return {};
    }

    const StepValues stepValues = roundUpToFixedValue(m_meterSize, m_dbRange);

    if (stepValues.smallStep == 0) {
        return {};
    }

    std::vector<double> steps;
    int value = m_dbRange;

    while (value < MAX_VOLUME_DB) {
        if (value % stepValues.fullStep != 0) {
            steps.push_back(value);
        }
        value += stepValues.smallStep;
    }

    return steps;
}
