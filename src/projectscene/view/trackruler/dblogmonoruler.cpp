/*
* Audacity: A Digital Audio Editor
*/
#include "framework/global/realfn.h"

#include "dblogmonoruler.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 20;
}

DbLogMonoRuler::DbLogMonoRuler()
    : DbLogBaseRuler(DbLogRulerUiSettings { MIN_ADJACENT_FULL_STEPS_HEIGHT })
{
}

int DbLogMonoRuler::getAlignment(double value, bool isNegativeSample) const
{
    if (muse::RealIsEqual(value, m_maxDisplayValueDB)) {
        return isNegativeSample ? 1 : -1;
    }
    return 0;
}

double DbLogMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, bool isNegativeSample) const
{
    return valueToPosition(step, m_height, isNegativeSample);
}

std::vector<TrackRulerFullStep> DbLogMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { TrackRulerFullStep { m_dbRange, 0, 0, true, true, false } };
    }

    std::vector<double> steps = fullStepsValues(m_height);

    std::vector<TrackRulerFullStep> result;
    result.reserve((2 * steps.size()) + 1);
    for (double value : steps) {
        result.push_back(TrackRulerFullStep { value, 0, getAlignment(value, false),
                                              isBold(value), false, false });
        result.push_back(TrackRulerFullStep { value, 0, getAlignment(value, true),
                                              isBold(value), false, true });
    }
    result.push_back(TrackRulerFullStep { m_dbRange, 0, 0, true, true, false });

    return result;
}

std::vector<TrackRulerSmallStep> DbLogMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { TrackRulerSmallStep { 0.0, 0, false }, TrackRulerSmallStep { 0.0, 0, true } };
    }

    std::vector<double> steps = smallStepsValues(m_height);
    std::vector<double> fullSteps = fullStepsValues(m_height);
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
