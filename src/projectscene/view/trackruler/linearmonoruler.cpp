#include "projectscene/view/trackruler/linearmonoruler.h"
#include "global/realfn.h"

using namespace au::projectscene;

namespace {
constexpr int HIGH_RESOLUTION_MIN_HEIGHT = 340;

constexpr double MAX_VALUE = 1.0;
constexpr double MIN_VALUE = -1.0;

constexpr double LOW_RESOLUTION_FULL_STEPS_INCREMENT = 0.5;
constexpr double LOW_RESOLUTION_SMALL_STEPS_INCREMENT = 0.1;

constexpr double HIGH_RESOLUTION_FULL_STEPS_INCREMENT = 0.1;
constexpr double HIGH_RESOLUTION_SMALL_STEPS_INCREMENT = 0.05;

constexpr std::array<TrackRulerFullStep, 1> COLLAPSED_FULL_STEPS = { {
    TrackRulerFullStep{ 0.0, 0, 0, true, true, false },
} };

constexpr std::array<TrackRulerSmallStep, 2> COLLAPSED_SMALL_STEPS = { {
    TrackRulerSmallStep{ 1.0, 0, false },
    TrackRulerSmallStep{ -1.0, 0, true }
} };

bool isBold(double value)
{
    return muse::RealIsEqual(value, 1.0) || muse::RealIsEqual(value, 0.0) || muse::RealIsEqual(value, -1.0);
}

int getAlignment(double value)
{
    if (muse::RealIsEqual(value, 1.0)) {
        return -1;
    }

    if (muse::RealIsEqual(value, -1.0)) {
        return 1;
    }

    return 0;
}

std::vector<double> fullStepsValues(double height)
{
    double increment = height >= HIGH_RESOLUTION_MIN_HEIGHT ? HIGH_RESOLUTION_FULL_STEPS_INCREMENT : LOW_RESOLUTION_FULL_STEPS_INCREMENT;

    std::vector<double> steps;
    for (double v = MIN_VALUE; v <= MAX_VALUE; v += increment) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> smallStepsValues(double height)
{
    double increment = height >= HIGH_RESOLUTION_MIN_HEIGHT ? HIGH_RESOLUTION_SMALL_STEPS_INCREMENT : LOW_RESOLUTION_SMALL_STEPS_INCREMENT;

    std::vector<double> steps;
    for (double v = MIN_VALUE; v <= MAX_VALUE; v += increment) {
        steps.push_back(v);
    }
    return steps;
}
}

double LinearMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, [[maybe_unused]] bool isNegativeSample) const
{
    return (1.0 - (step / 2.0 + 0.5)) * m_height;
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
    ss << std::fixed << std::setprecision(2) << std::abs(sample);
    return ss.str();
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
        result.push_back(TrackRulerFullStep { v, 0, getAlignment(v), isBold(v), v == 0.0, v < 0.0 });
    }

    return result;
}

std::vector<TrackRulerSmallStep> LinearMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { COLLAPSED_SMALL_STEPS.begin(), COLLAPSED_SMALL_STEPS.end() };
    }

    std::vector<double> fullSteps = fullStepsValues(m_height);
    std::vector<double> steps = smallStepsValues(m_height);

    std::vector<TrackRulerSmallStep> result;
    for (double v : steps) {
        if (std::find(fullSteps.begin(), fullSteps.end(), v) != fullSteps.end()) {
            continue;
        }

        result.push_back(TrackRulerSmallStep { v, 0, v < 0.0 });
    }

    return result;
}
