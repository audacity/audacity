/*
* Audacity: A Digital Audio Editor
*/
#include "projectscene/view/trackruler/dblogstereoruler.h"
#include "view/trackruler/itrackrulermodel.h"

#include <cstddef>
#include <string>

using namespace au::projectscene;

namespace {
constexpr double LOW_RESOLUTION_MIN_HEIGHT = 95.0;
constexpr double MIN_CHANNEL_HEIGHT = 30.0;

constexpr std::array<TrackRulerFullStep, 2> LOW_RESOLUTION_FULL_STEPS_CH0 = { {
    TrackRulerFullStep{ 0.0, 0, -1, true, false, false },
    TrackRulerFullStep{ 1.0, 0, 0, true, true, false },
} };

constexpr std::array<TrackRulerFullStep, 2> LOW_RESOLUTION_FULL_STEPS_CH1 = { {
    TrackRulerFullStep{ 1.0, 1, 0, true, false, false },
    TrackRulerFullStep{ 0.0, 1, 1, true, false, true },
} };

constexpr std::array<TrackRulerFullStep, 6> HIGH_RESOLUTION_FULL_STEPS_CH0 = { {
    TrackRulerFullStep{ 0.0, 0, -1, true, false, false },
    TrackRulerFullStep{ 1.0 / 3.0, 0, 0, false, false, false },
    TrackRulerFullStep{ 2.0 / 3.0, 0, 0, false, false, false },
    TrackRulerFullStep{ 1.0, 0, 0, true, true, false },
    TrackRulerFullStep{ 1.0 / 3.0, 0, 0, false, false, true },
    TrackRulerFullStep{ 2.0 / 3.0, 0, 0, false, false, true },
} };

constexpr std::array<TrackRulerFullStep, 6> HIGH_RESOLUTION_FULL_STEPS_CH1 = { {
    TrackRulerFullStep{ 1.0 / 3.0, 1, 0, false, false, false },
    TrackRulerFullStep{ 2.0 / 3.0, 1, 0, false, false, false },
    TrackRulerFullStep{ 1.0, 1, 0, true, true, false },
    TrackRulerFullStep{ 1.0 / 3.0, 1, 0, false, false, true },
    TrackRulerFullStep{ 2.0 / 3.0, 1, 0, false, false, true },
    TrackRulerFullStep{ 0.0, 1, 1, true, false, true },
} };

constexpr std::array<double, 4> HIGH_RESOLUTION_SMALL_STEPS = { 1.0 / 6.0, 1.0 / 2.0, 2.0 / 3.0, 5.0 / 6.0 };

std::vector<TrackRulerFullStep> fullStepsValues(double height, size_t channel, double dbRange)
{
    std::vector<TrackRulerFullStep> steps;
    if (height >= LOW_RESOLUTION_MIN_HEIGHT) {
        channel == 0 ? steps = { HIGH_RESOLUTION_FULL_STEPS_CH0.begin(), HIGH_RESOLUTION_FULL_STEPS_CH0.end() }
        : steps = { HIGH_RESOLUTION_FULL_STEPS_CH1.begin(), HIGH_RESOLUTION_FULL_STEPS_CH1.end() };
    } else {
        channel == 0 ? steps = { LOW_RESOLUTION_FULL_STEPS_CH0.begin(), LOW_RESOLUTION_FULL_STEPS_CH0.end() }
        : steps = { LOW_RESOLUTION_FULL_STEPS_CH1.begin(), LOW_RESOLUTION_FULL_STEPS_CH1.end() };
    }

    for (auto& step : steps) {
        step.value *= dbRange;
    }

    return steps;
}

std::vector<double> smallStepsValues(double height, double dbRange)
{
    if (height < LOW_RESOLUTION_MIN_HEIGHT) {
        return {};
    }

    std::vector<double> smallSteps = { HIGH_RESOLUTION_SMALL_STEPS.begin(), HIGH_RESOLUTION_SMALL_STEPS.end() };
    for (auto& step : smallSteps) {
        step *= dbRange;
    }

    return smallSteps;
}
}

double DbLogStereoRuler::stepToPosition(double step, size_t channel, bool isNegativeSample) const
{
    double middlePosition = m_height * m_channelHeightRatio;

    if (channel > 1) {
        return middlePosition;
    }

    const double startPosition = channel == 0 ? 0.0 : middlePosition;
    const double endPosition = channel == 0 ? middlePosition : m_height;
    const double channelMiddlePosition = (endPosition - startPosition) / 2.0;

    if (isNegativeSample) {
        return endPosition - (channelMiddlePosition * (step / m_dbRange));
    }

    return startPosition + (channelMiddlePosition * (step / m_dbRange));
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

        auto channelSteps = fullStepsValues(channelHeights[channel], channel, m_dbRange);
        for (auto& step : channelSteps) {
            step.channel = channel;
            steps.push_back(step);
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

        auto channelSteps = smallStepsValues(channelHeights[channel], m_dbRange);
        for (auto& step : channelSteps) {
            steps.push_back(TrackRulerSmallStep { step, channel, false });
            steps.push_back(TrackRulerSmallStep { step, channel, true });
        }
    }

    return steps;
}
