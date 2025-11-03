/*
* Audacity: A Digital Audio Editor
*/
#include <string>

#include "global/realfn.h"

#include "dblogmonoruler.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 17;
constexpr double MIN_ADJACENT_SMALL_STEPS_HEIGHT = 4;
constexpr std::array<std::pair<double, double>, 4> STEP_INCREMENT = {
    { { 1.0 / 6.0, 1.0 / 30.0 },
        { 1.0 / 6.0, 1.0 / 12.0 },
        { 1.0 / 3.0, 1.0 / 6.0 },
        { 1.0, 1.0 / 3.0 } } };
constexpr std:: pair<double, double> DEFAULT_INCREMENT = { 1.0, 1.0 / 3.0 };

bool isBold(double value, double dbRange)
{
    return muse::RealIsEqual(value, 0.0) || muse::RealIsEqual(value, dbRange);
}

int getAlignment(double value, bool isNegativeSample)
{
    if (muse::RealIsEqual(value, 0.0)) {
        return isNegativeSample ? 1 : -1;
    }
    return 0;
}

double valueToPosition(double value, double height, double dbRange, bool isNegativeSample)
{
    double middlePosition = height / 2.0;

    if (muse::RealIsEqual(value, dbRange)) {
        return middlePosition;
    }

    if (isNegativeSample) {
        return height - (middlePosition * (value / dbRange));
    }

    return middlePosition * (value / dbRange);
}

std::pair<double, double> stepsIncrement(double height, double dbRange)
{
    std::pair<double, double> increment = DEFAULT_INCREMENT;
    for (const auto& stepInc : STEP_INCREMENT) {
        const auto& [fs, ss] = stepInc;
        if ((valueToPosition(dbRange * fs, height, dbRange,
                             false) - valueToPosition(0, height, dbRange, false) >= MIN_ADJACENT_FULL_STEPS_HEIGHT)
            && (valueToPosition(dbRange * ss, height, dbRange,
                                false) - valueToPosition(0, height, dbRange, false) >= MIN_ADJACENT_SMALL_STEPS_HEIGHT)) {
            increment = stepInc;
            break;
        }
    }

    return increment;
}

std::vector<double> fullStepsValues(double height, double dbRange)
{
    const std::pair<double, double> increment = stepsIncrement(height, dbRange);

    std::vector<double> steps;
    for (double v = dbRange; v <= 0; v += (increment.first * std::abs(dbRange))) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> smallStepsValues(double height, double dbRange)
{
    const std::pair<double, double> increment = stepsIncrement(height, dbRange);

    std::vector<double> steps;
    for (double v = dbRange; v <= 0; v += (increment.second * std::abs(dbRange))) {
        steps.push_back(v);
    }
    return steps;
}
}

double DbLogMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, bool isNegativeSample) const
{
    return valueToPosition(step, m_height, m_dbRange, isNegativeSample);
}

void DbLogMonoRuler::setHeight(int height)
{
    m_height = height;
}

void DbLogMonoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void DbLogMonoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void DbLogMonoRuler::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

std::string DbLogMonoRuler::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(0) << std::abs(sample);
    return ss.str();
}

std::vector<TrackRulerFullStep> DbLogMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { TrackRulerFullStep { m_dbRange, 0, 0, true, true, false } };
    }

    std::vector<double> steps = fullStepsValues(m_height, m_dbRange);
    std::vector<TrackRulerFullStep> result;
    result.reserve((2 * steps.size()) + 1);
    for (double value : steps) {
        result.push_back(TrackRulerFullStep { value, 0, getAlignment(value, false), isBold(value, m_dbRange), false, false });
        result.push_back(TrackRulerFullStep { value, 0, getAlignment(value, true), isBold(value, m_dbRange), false, true });
    }
    result.push_back(TrackRulerFullStep { m_dbRange, 0, 0, true, true, false });

    return result;
}

std::vector<TrackRulerSmallStep> DbLogMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { TrackRulerSmallStep { 0.0, 0, false }, TrackRulerSmallStep { 0.0, 0, true } };
    }

    std::vector<double> steps = smallStepsValues(m_height, m_dbRange);
    std::vector<double> fullSteps = fullStepsValues(m_height, m_dbRange);
    std::vector<TrackRulerSmallStep> result;
    for (double value : steps) {
        if (std::find(fullSteps.begin(), fullSteps.end(), value) != fullSteps.end()) {
            continue;
        }
        result.push_back(TrackRulerSmallStep { value, 0, false });
        result.push_back(TrackRulerSmallStep { value, 0, true });
    }
    return result;
}
