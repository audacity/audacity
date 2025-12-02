/*
* Audacity: A Digital Audio Editor
*/
#include "dblogstereoruler.h"

#include "framework/global/realfn.h"
#include "view/trackruler/itrackruler.h"

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
    if (m_isHalfWave && muse::RealIsEqual(value, m_dbRange)) {
        return 1;
    }

    if (std::round(value) == std::round(m_maxDisplayValueDB)) {
        if (channel == 0) {
            return isNegativeSample ? 0 : -1;
        }

        if (m_isHalfWave) {
            return -1;
        }

        return isNegativeSample ? 1 : 0;
    }

    return 0;
}

bool DbLogStereoRuler::isFullTick(double value, size_t channel, bool isNegativeSample) const
{
    if (muse::RealIsEqual(value, m_dbRange)) {
        return true;
    }

    if ((channel == 0 && isNegativeSample) || (channel == 1 && !isNegativeSample)) {
        return muse::RealIsEqual(value, m_maxDisplayValueDB);
    }

    return false;
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

    return m_isHalfWave ? fullStepsForHalfWave() : fullStepsForFullWave();
}

std::vector<TrackRulerFullStep> DbLogStereoRuler::fullStepsForHalfWave() const
{
    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    std::vector<TrackRulerFullStep> steps;
    for (size_t channel = 0; channel < channelHeights.size(); ++channel) {
        if (channelHeights[channel] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerFullStep { m_dbRange, channel, 0, true, true, false });
            continue;
        }

        const auto values = fullStepsValues(channelHeights[channel]);
        for (double value : values) {
            steps.push_back(TrackRulerFullStep {
                value, channel, getAlignment(value, channel, false), isBold(value),
                isFullTick(value, channel, false), false });
        }
    }
    return steps;
}

std::vector<TrackRulerFullStep> DbLogStereoRuler::fullStepsForFullWave() const
{
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
                    value, channel, getAlignment(value, channel, isNegativeSample), isBold(value),
                    isFullTick(value, channel, isNegativeSample),
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
        return { TrackRulerSmallStep { m_maxDisplayValueDB, 0, false }, TrackRulerSmallStep { 0.0, 1, true } };
    }

    return m_isHalfWave ? smallStepsForHalfWave() : smallStepsForFullWave();
}

std::vector<TrackRulerSmallStep> DbLogStereoRuler::smallStepsForHalfWave() const
{
    std::vector<TrackRulerSmallStep> steps;

    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t channel = 0; channel < channelHeights.size(); ++channel) {
        if (channelHeights[channel] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerSmallStep { m_maxDisplayValueDB, channel, channel == 1 });
            continue;
        }

        const auto values = smallStepsValues(channelHeights[channel]);
        const auto fullSteps = fullStepsValues(channelHeights[channel]);

        for (double value : values) {
            if (std::find_if(fullSteps.begin(), fullSteps.end(),
                             [&](double fullStepValue) {
                return muse::RealIsEqual(fullStepValue, value);
            }) != fullSteps.end()) {
                continue;
            }
            steps.push_back(TrackRulerSmallStep { value, channel, false });
        }
    }

    return steps;
}

std::vector<TrackRulerSmallStep> DbLogStereoRuler::smallStepsForFullWave() const
{
    std::vector<TrackRulerSmallStep> steps;

    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t channel = 0; channel < channelHeights.size(); ++channel) {
        if (channelHeights[channel] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerSmallStep { m_maxDisplayValueDB, channel, channel == 1 });
            continue;
        }

        const auto values = smallStepsValues(channelHeights[channel]);
        const auto fullSteps = fullStepsValues(channelHeights[channel]);

        for (double value : values) {
            for (bool isNegativeSample : { false, true }) {
                if (std::find_if(fullSteps.begin(), fullSteps.end(),
                                 [&](double fullStepValue) {
                    return muse::RealIsEqual(fullStepValue, value);
                }) != fullSteps.end()) {
                    continue;
                }
                steps.push_back(TrackRulerSmallStep { value, channel, isNegativeSample });
            }
        }
    }

    return steps;
}
