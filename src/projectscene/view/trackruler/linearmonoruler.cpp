/*
* Audacity: A Digital Audio Editor
*/
#include "global/realfn.h"

#include "linearmonoruler.h"
#include "linearrulerutils.h"

using namespace au::projectscene;

namespace {
constexpr std::array<TrackRulerFullStep, 1> COLLAPSED_FULL_STEPS = { {
    TrackRulerFullStep{ 0.0, 0, 0, true, true, false },
} };

constexpr std::array<TrackRulerSmallStep, 2> COLLAPSED_SMALL_STEPS = { {
    TrackRulerSmallStep{ 1.0, 0, false },
    TrackRulerSmallStep{ -1.0, 0, true }
} };
}

double LinearMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, [[maybe_unused]] bool isNegativeSample) const
{
    return linearrulerutils::valueToPosition(step, m_height);
}

void LinearMonoRuler::setHeight(int height)
{
    m_height = height;
}

void LinearMonoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void LinearMonoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

std::string LinearMonoRuler::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(1) << std::abs(sample);
    return ss.str();
}

std::vector<TrackRulerFullStep> LinearMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { COLLAPSED_FULL_STEPS.begin(), COLLAPSED_FULL_STEPS.end() };
    }

    std::vector<double> steps = linearrulerutils::fullStepsValues(m_height);
    std::vector<TrackRulerFullStep> result;
    result.reserve(steps.size());
    for (double v : steps) {
        result.push_back(TrackRulerFullStep { v, 0,
                                              linearrulerutils::getAlignment(v),
                                              linearrulerutils::isBold(v),
                                              muse::RealIsEqual(v, 0.0), v < 0.0 });
    }

    return result;
}

std::vector<TrackRulerSmallStep> LinearMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { COLLAPSED_SMALL_STEPS.begin(), COLLAPSED_SMALL_STEPS.end() };
    }

    std::vector<double> fullSteps = linearrulerutils::fullStepsValues(m_height);
    std::vector<double> steps = linearrulerutils::smallStepsValues(m_height);

    std::vector<TrackRulerSmallStep> result;
    for (double v : steps) {
        if (std::find_if(fullSteps.begin(), fullSteps.end(), [v](double fs) { return muse::RealIsEqual(v, fs); }) != fullSteps.end()) {
            continue;
        }

        result.push_back(TrackRulerSmallStep { v, 0, v < 0.0 });
    }

    return result;
}
