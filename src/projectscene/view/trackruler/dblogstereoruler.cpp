/*
* Audacity: A Digital Audio Editor
*/
#include <cstddef>
#include <string>

#include "global/realfn.h"

#include "dblogstereoruler.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 17;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 6;
constexpr std::array<std::pair<double, double>, 3> STEP_INCREMENT = {
    { { 1.0 / 3.0, 1.0 / 15.0 },
        { 1.0 / 3.0, 1.0 / 6.0 },
        { 1.0, 1.0 / 2.0 } } };
constexpr std:: pair<double, double> DEFAULT_INCREMENT = { 1.0, 1.0 / 3.0 };
constexpr double MIN_CHANNEL_HEIGHT = 40.0;

int getAlignment(double value, size_t channel, bool isNegativeSample)
{
    if (muse::RealIsEqual(value, 0.0)) {
        if (channel == 0) {
            return isNegativeSample ? 0 : -1;
        } else {
            return isNegativeSample ? 1 : 0;
        }
    }

    return 0;
}

bool isBold(double value, double dbRange)
{
    return muse::RealIsEqual(value, 0.0) || muse::RealIsEqual(value, dbRange);
}

double valueToPosition(double value, double height, double dbRange, bool isNegativeSample)
{
    double middlePosition = height / 2;

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

double DbLogStereoRuler::stepToPosition(double step, size_t channel, bool isNegativeSample) const
{
    double middlePosition = m_height * m_channelHeightRatio;

    if (channel > 1) {
        return middlePosition;
    }

    const double startPosition = channel == 0 ? 0.0 : middlePosition;
    const double height = channel == 0 ? middlePosition : m_height - middlePosition;
    return startPosition + valueToPosition(step, height, m_dbRange, isNegativeSample);
}

void DbLogStereoRuler::setHeight(int height)
{
    m_height = height;
}

void DbLogStereoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void DbLogStereoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void DbLogStereoRuler::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

std::string DbLogStereoRuler::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(0) << std::abs(sample);
    return ss.str();
}

std::vector<TrackRulerFullStep> DbLogStereoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { TrackRulerFullStep { m_dbRange, 0, 0, true, true, false },
                 TrackRulerFullStep { m_dbRange, 1, 0, true, true, true } };
    }

    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    std::vector<TrackRulerFullStep> steps;
    for (size_t channel = 0; channel < channelHeights.size(); ++channel) {
        if (channelHeights[channel] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerFullStep { m_dbRange, channel, 0, true, true, false });
            continue;
        }

        const auto values = fullStepsValues(channelHeights[channel], m_dbRange);
        for (double value : values) {
            for (bool isNegativeSample : { false, true }) {
                steps.push_back(TrackRulerFullStep {
                    value, channel, getAlignment(value, channel, isNegativeSample), isBold(value,
                                                                                           m_dbRange), value == 0.0, isNegativeSample });
            }
        }
    }
    steps.push_back(TrackRulerFullStep { 0.0, 2, 0, true, true, false });

    return steps;
}

std::vector<TrackRulerSmallStep> DbLogStereoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { TrackRulerSmallStep { 0.0, 0, false }, TrackRulerSmallStep { 0.0, 1, true } };
    }

    std::vector<TrackRulerSmallStep> steps;

    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t channel = 0; channel < channelHeights.size(); ++channel) {
        if (channelHeights[channel] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerSmallStep { 0.0, channel, channel == 1 });
            continue;
        }

        const auto values = smallStepsValues(channelHeights[channel], m_dbRange);
        const auto fullSteps = fullStepsValues(channelHeights[channel], m_dbRange);
        for (double v : values) {
            for (bool isNegativeSample : { false, true }) {
                if (std::find_if(fullSteps.begin(), fullSteps.end(),
                                 [&](double fullStepValue) {
                    return muse::RealIsEqual(fullStepValue, v);
                }) != fullSteps.end()) {
                    continue;
                }
                steps.push_back(TrackRulerSmallStep { v, channel, isNegativeSample });
            }
        }
    }

    return steps;
}
