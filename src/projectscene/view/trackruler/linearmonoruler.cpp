/*
* Audacity: A Digital Audio Editor
*/
#include "framework/global/realfn.h"

#include "linearmonoruler.h"

using namespace au::projectscene;

namespace {
constexpr std::array<TrackRulerFullStep, 1> COLLAPSED_FULL_STEPS = { {
    TrackRulerFullStep{ 0.0, 0, 0, true, true, false },
} };
}

double LinearMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, [[maybe_unused]] bool isNegativeSample) const
{
    return valueToPosition(step, m_height);
}

std::vector<TrackRulerFullStep> LinearMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { COLLAPSED_FULL_STEPS.begin(), COLLAPSED_FULL_STEPS.end() };
    }

    std::vector<double> steps = fullStepsValues(m_height);
    std::vector<TrackRulerFullStep> result;
    result.reserve(steps.size());
    for (double v : steps) {
        result.push_back(TrackRulerFullStep { v, 0,
                                              getAlignment(v),
                                              isBold(v),
                                              muse::RealIsEqual(v, 0.0), v < 0.0 });
    }

    return result;
}

std::vector<TrackRulerSmallStep> LinearMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { TrackRulerSmallStep{ m_maxDisplayValue, 0, m_maxDisplayValue < 0.0 },
                 TrackRulerSmallStep{ m_minDisplayValue, 0, m_minDisplayValue < 0.0 } };
    }

    std::vector<double> fullSteps = fullStepsValues(m_height);
    std::vector<double> steps = smallStepsValues(m_height);

    std::vector<TrackRulerSmallStep> result;
    for (double v : steps) {
        if (std::find_if(fullSteps.begin(), fullSteps.end(), [v](double fs) { return muse::RealIsEqual(v, fs); }) != fullSteps.end()) {
            continue;
        }

        result.push_back(TrackRulerSmallStep { v, 0, v < 0.0 });
    }

    return result;
}
