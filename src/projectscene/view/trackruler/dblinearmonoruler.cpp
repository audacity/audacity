/*
* Audacity: A Digital Audio Editor
*/
#include "dblinearmonoruler.h"

#include "framework/global/realfn.h"

#include "projectscene/view/trackruler/itrackruler.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_FULL_STEP_TO_INF_HEIGHT = 23;
constexpr int MIN_ADJACENT_STEPS_HEIGHT = 10;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 5;
constexpr int MIN_FULL_STEP_TO_ZERO_HEIGHT = 14;
constexpr std::array<int, 4> FULL_STEP_SIZES = { 1, 2, 3, 6 };
}

DbLinearMonoRuler::DbLinearMonoRuler()
    : DbLinearBaseRuler(DbLinearRulerUiSettings { MIN_ADJACENT_STEPS_HEIGHT,  MIN_FULL_STEP_TO_INF_HEIGHT,
                                                  MIN_FULL_STEP_TO_ZERO_HEIGHT,
                                                  std::vector<int>(FULL_STEP_SIZES.begin(), FULL_STEP_SIZES.end()) })
{
}

double DbLinearMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, bool isNegativeSample) const
{
    if (muse::RealIsEqual(step, m_dbRange)) {
        return m_height / 2.0;
    }

    return valueToPosition(step, m_height, isNegativeSample);
}

std::vector<TrackRulerFullStep> DbLinearMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { TrackRulerFullStep { m_dbRange, 0, 0, true, true, false } };
    }

    const std::vector<int> valuesList = fullStepValues(m_height);
    std::vector<TrackRulerFullStep> steps { TrackRulerFullStep { m_dbRange, 0, 0, false, true, false },
                                            TrackRulerFullStep { m_maxDisplayValueDB, 0, -1, true, true, false },
                                            TrackRulerFullStep { m_maxDisplayValueDB, 0, 1, true, true, true }
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
        return { TrackRulerSmallStep { m_maxDisplayValueDB, 0, false }, TrackRulerSmallStep { m_maxDisplayValueDB, 0, true } };
    }

    std::vector<int> valuesList = fullStepValues(m_height);
    valuesList.push_back(static_cast<int>(m_maxDisplayValueDB));

    std::vector<std::tuple<int, int> > fullStepRanges;
    fullStepRanges.reserve(valuesList.size() - 1);
    for (size_t i = 0; i < valuesList.size() - 1; ++i) {
        fullStepRanges.emplace_back(valuesList[i], valuesList[i + 1]);
    }

    // We ensure small steps will be shown onlu if there is enough room between full steps
    int lowestSmallStep = static_cast<int>(m_maxDisplayValueDB);
    for (const auto& [startValue, endValue] : fullStepRanges) {
        double firstPos = valueToPosition(startValue, m_height, false);
        double secondPos = valueToPosition(endValue, m_height, false);
        if (((firstPos - secondPos) / (endValue - startValue)) > MIN_ADJACENT_SMALL_STEPS_HEIGHT) {
            lowestSmallStep = startValue + 1;
            break;
        }
    }

    std::vector<TrackRulerSmallStep> steps = {};
    for (int i = lowestSmallStep; i < m_maxDisplayValueDB; i++) {
        if (std::find(valuesList.begin(), valuesList.end(), i) != valuesList.end()) {
            continue;
        }
        steps.push_back(TrackRulerSmallStep { static_cast<double>(i), 0, false });
        steps.push_back(TrackRulerSmallStep { static_cast<double>(i), 0, true });
    }
    return steps;
}
