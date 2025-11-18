/*
* Audacity: A Digital Audio Editor
*/
#include "dblogstereoruler.h"

#include "framework/global/realfn.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 17;
constexpr double MIN_CHANNEL_HEIGHT = 40.0;
}

DbLogStereoRuler::DbLogStereoRuler()
    : DbLogBaseRuler(DbLogRulerUiSettings { MIN_ADJACENT_FULL_STEPS_HEIGHT })
{
}

int DbLogStereoRuler::getAlignment(double value, size_t channel, bool isNegativeSample) const
{
    if (std::round(value) == std::round(m_maxDisplayValueDB)) {
        if (channel == 0) {
            return isNegativeSample ? 0 : -1;
        }

        return isNegativeSample ? 1 : 0;
    }

    return 0;
}

double DbLogStereoRuler::stepToPosition(double step, size_t channel, bool isNegativeSample) const
{
    double middlePosition = m_height * m_channelHeightRatio;

    if (channel > 1) {
        return middlePosition;
    }

    const double startPosition = channel == 0 ? 0.0 : middlePosition;
    const double height = channel == 0 ? middlePosition : m_height - middlePosition;
    return startPosition + valueToPosition(step, height, isNegativeSample);
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

        const auto values = fullStepsValues(channelHeights[channel]);

        for (double value : values) {
            for (bool isNegativeSample : { false, true }) {
                steps.push_back(TrackRulerFullStep {
                    value, channel, getAlignment(value, channel, isNegativeSample), isBold(value), value == 0.0,
                    isNegativeSample });
            }
        }
    }
    steps.push_back(TrackRulerFullStep { m_maxDisplayValueDB, 2, 0, true, true, false });

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

        const auto values = smallStepsValues(channelHeights[channel]);
        const auto fullSteps = fullStepsValues(channelHeights[channel]);
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
