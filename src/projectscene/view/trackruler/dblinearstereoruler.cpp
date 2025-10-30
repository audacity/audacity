/*
* Audacity: A Digital Audio Editor
*/
#include "global/realfn.h"

#include "projectscene/view/trackruler/dblinearstereoruler.h"
#include "types/ratio.h"
#include "view/trackruler/itrackrulermodel.h"

#include <cstddef>
#include <string>

using namespace au::projectscene;

namespace {
constexpr int MIN_LOWEST_FULL_STEP_TO_ZERO_HEIGHT = 12;
constexpr int MIN_ADJACENT_STEPS_HEIGHT = 10;
constexpr int MIN_HEIGHT_TO_ZERO = 10;
constexpr int MIN_CHANNEL_HEIGHT = 30;
constexpr std::array<int, 5> FULL_STEP_SIZES = { 1, 2, 3, 6, 9 };

double valueToPosition(double value, double height, bool isNegativeSample = false)
{
    const auto linearValue = muse::db_to_linear(value) * (isNegativeSample ? -1.0 : 1.0);
    return (1.0 - (linearValue / 2.0 + 0.5)) * height;
}

int computeLowestFullStepValue(double height, double m_dbRange)
{
    auto const middlePoint = height / 2.0;
    int lowestFullStep = static_cast<int>(m_dbRange);
    for (int i = lowestFullStep + 3; i < 0; i += 3) {
        const double position = valueToPosition(i, height);
        if (middlePoint - position > MIN_LOWEST_FULL_STEP_TO_ZERO_HEIGHT) {
            return lowestFullStep;
        }
        lowestFullStep = i;
    }

    return 0;
}

std::vector<int> fullStepsValues(double height, double dbRange)
{
    const int lowestFullStep = computeLowestFullStepValue(height, dbRange);

    std::vector<int> steps;
    if (lowestFullStep != 0) {
        steps.push_back(lowestFullStep);
        int previousValidStep = lowestFullStep;

        for (int step = lowestFullStep + 1; step < 0; step++) {
            const int diff = step - previousValidStep;
            if (std::find(FULL_STEP_SIZES.begin(), FULL_STEP_SIZES.end(), diff) == FULL_STEP_SIZES.end()) {
                continue;
            }

            const double position = valueToPosition(step, height);
            if (position < MIN_HEIGHT_TO_ZERO) {
                break;
            }

            const double previousPosition = valueToPosition(previousValidStep, height);
            if (previousPosition - position >= MIN_ADJACENT_STEPS_HEIGHT) {
                steps.push_back(step);
                previousValidStep = step;
            }
        }
    }

    return steps;
}
}

double DbLinearStereoRuler::stepToPosition(double step, size_t channel, bool isNegativeSample) const
{
    double middlePosition = m_height * m_channelHeightRatio;

    if (channel > 1) {
        return middlePosition;
    }

    const double startPosition = channel == 0 ? 0.0 : middlePosition;
    const double endPosition = channel == 0 ? middlePosition : m_height;
    const double channelMiddleOffset = (endPosition - startPosition) / 2.0;

    if (muse::RealIsEqual(step, m_dbRange)) {
        return startPosition + channelMiddleOffset;
    }

    const auto linearValue = muse::db_to_linear(step) * (isNegativeSample ? -1.0 : 1.0);
    return startPosition + ((1.0 - (linearValue / 2.0 + 0.5)) * (endPosition - startPosition));
}

void DbLinearStereoRuler::setHeight(int height)
{
    m_height = height;
}

void DbLinearStereoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void DbLinearStereoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void DbLinearStereoRuler::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

std::string DbLinearStereoRuler::sampleToText(double sample) const
{
    std::stringstream ss;

    if (muse::RealIsEqual(sample, m_dbRange)) {
        ss << "\u221E";
    } else {
        ss << std::fixed << std::setprecision(0) << std::abs(sample);
    }

    return ss.str();
}

std::vector<TrackRulerFullStep> DbLinearStereoRuler::fullSteps() const
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

        std::vector<TrackRulerFullStep> channelSteps { TrackRulerFullStep { m_dbRange, channel, 0, false, true, false },
                                                       TrackRulerFullStep { 0.0, channel, 0, true, true, false },
                                                       TrackRulerFullStep { 0.0, channel, 0, true, true, true }
        };

        auto valuesList = fullStepsValues(channelHeights[channel], m_dbRange);
        for (const auto& stepValue : valuesList) {
            channelSteps.push_back(TrackRulerFullStep { static_cast<double>(stepValue), channel, 0, false, false, false });
            channelSteps.push_back(TrackRulerFullStep { static_cast<double>(stepValue), channel, 0, false, false, true });
        }
        steps.insert(steps.end(), channelSteps.begin(), channelSteps.end());
    }

    return steps;
}

std::vector<TrackRulerSmallStep> DbLinearStereoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { TrackRulerSmallStep { 0.0, 0, false }, TrackRulerSmallStep { 0.0, 1, true } };
    }

    std::vector<double> channelHeights = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    std::vector<TrackRulerSmallStep> steps;
    for (size_t channel = 0; channel < 2; ++channel) {
        const int lowestFullStep = computeLowestFullStepValue(channelHeights[channel], m_dbRange);
        const std::vector<int> valuesList = fullStepsValues(channelHeights[channel], m_dbRange);
        for (int i = lowestFullStep; i < 0; i++) {
            if (std::find(valuesList.begin(), valuesList.end(), i) != valuesList.end()) {
                continue;
            }

            steps.push_back(TrackRulerSmallStep { static_cast<double>(i), channel, false });
            steps.push_back(TrackRulerSmallStep { static_cast<double>(i), channel, true });
        }
    }

    return steps;
}
