#include "global/realfn.h"

#include "projectscene/view/trackruler/dblinearmonoruler.h"
#include "types/ratio.h"
#include "view/trackruler/itrackrulermodel.h"

#include <string>

using namespace au::projectscene;

namespace {
constexpr int MIN_LOWEST_FULL_STEP_TO_ZERO_HEIGHT = 23;
constexpr int MIN_ADJACENT_STEPS_HEIGHT = 10;
constexpr int MIN_HEIGHT_TO_ZERO = 14;
constexpr std::array<int, 4> FULL_STEP_SIZES = { 1, 2, 3, 6 };

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

std::vector<int> fullStepValues(double height, double dbRange)
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

double DbLinearMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, bool isNegativeSample) const
{
    if (muse::RealIsEqual(step, m_dbRange)) {
        return m_height / 2.0;
    }

    return valueToPosition(step, m_height, isNegativeSample);
}

void DbLinearMonoRuler::setHeight(int height)
{
    m_height = height;
}

void DbLinearMonoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void DbLinearMonoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

void DbLinearMonoRuler::setDbRange(double dbRange)
{
    m_dbRange = dbRange;
}

std::string DbLinearMonoRuler::sampleToText(double sample) const
{
    std::stringstream ss;

    if (muse::RealIsEqual(sample, m_dbRange)) {
        ss << "\u221E";
    } else {
        ss << std::fixed << std::setprecision(0) << std::abs(sample);
    }

    return ss.str();
}

std::vector<TrackRulerFullStep> DbLinearMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { TrackRulerFullStep { m_dbRange, 0, 0, true, true, false } };
    }

    const std::vector<int> valuesList = fullStepValues(m_height, m_dbRange);
    std::vector<TrackRulerFullStep> steps { TrackRulerFullStep { m_dbRange, 0, 0, false, true, false },
                                            TrackRulerFullStep { 0.0, 0, 0, true, true, false },
                                            TrackRulerFullStep { 0.0, 0, 0, true, true, true }
    };

    for (const int stepValue : valuesList) {
        steps.push_back(TrackRulerFullStep { static_cast<double>(stepValue), 0, 0, false, false, false });
        steps.push_back(TrackRulerFullStep { static_cast<double>(stepValue), 0, 0, false, false, true });
    }

    return steps;
}

std::vector<TrackRulerSmallStep> DbLinearMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { TrackRulerSmallStep { 0.0, 0, false }, TrackRulerSmallStep { 0.0, 0, true } };
    }

    const int lowestFullStep = computeLowestFullStepValue(m_height, m_dbRange);
    const std::vector<int> valuesList = fullStepValues(m_height, m_dbRange);
    std::vector<TrackRulerSmallStep> steps;
    for (int i = lowestFullStep; i < 0; i++) {
        if (std::find(valuesList.begin(), valuesList.end(), i) != valuesList.end()) {
            continue;
        }
        steps.push_back(TrackRulerSmallStep { static_cast<double>(i), 0, false });
        steps.push_back(TrackRulerSmallStep { static_cast<double>(i), 0, true });
    }
    return steps;
}

void DbLinearMonoRuler::setVerticalZoom(float verticalZoom)
{
    // No-op for DbLinear ruler
}
