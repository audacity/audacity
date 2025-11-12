/*
* Audacity: A Digital Audio Editor
*/
#include <cstddef>
#include <string>

#include "global/realfn.h"
#include "global/types/ratio.h"

#include "dblogstereoruler.h"
#include "dblogrulerutils.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 17;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 6;

constexpr double MIN_CHANNEL_HEIGHT = 40.0;

int getAlignment(double value, size_t channel, double maxValue, bool isNegativeSample)
{
    if (std::round(value) == std::round(maxValue)) {
        if (channel == 0) {
            return isNegativeSample ? 0 : -1;
        }

        return isNegativeSample ? 1 : 0;
    }

    return 0;
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
    return startPosition + dblogrulerutils::valueToPosition(step, height, m_dbRange, m_maxDisplayValue, isNegativeSample);
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

        const auto values = dblogrulerutils::fullStepsValues(channelHeights[channel], dblogrulerutils::StepSettings {
            m_dbRange,
            static_cast<double>(MIN_ADJACENT_FULL_STEPS_HEIGHT),
            static_cast<double>(MIN_ADJACENT_SMALL_STEPS_HEIGHT),
            m_maxDisplayValue,
        });

        for (double value : values) {
            for (bool isNegativeSample : { false, true }) {
                steps.push_back(TrackRulerFullStep {
                    value, channel, getAlignment(value, channel, m_maxDisplayValue, isNegativeSample),
                    dblogrulerutils::isBold(value, m_maxDisplayValue,
                                            m_dbRange), value == 0.0,
                    isNegativeSample });
            }
        }
    }
    steps.push_back(TrackRulerFullStep { m_maxDisplayValue, 2, 0, true, true, false });

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

        const dblogrulerutils::StepSettings settings = {
            m_dbRange,
            static_cast<double>(MIN_ADJACENT_FULL_STEPS_HEIGHT),
            static_cast<double>(MIN_ADJACENT_SMALL_STEPS_HEIGHT),
            m_maxDisplayValue,
        };

        const auto values = dblogrulerutils::smallStepsValues(channelHeights[channel], settings);
        const auto fullSteps = dblogrulerutils::fullStepsValues(channelHeights[channel], settings);
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

void DbLogStereoRuler::setVerticalZoom(float verticalZoom)
{
    m_maxDisplayValue = muse::linear_to_db(verticalZoom);
}
