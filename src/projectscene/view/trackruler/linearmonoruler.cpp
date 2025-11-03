/*
* Audacity: A Digital Audio Editor
*/
#include "projectscene/view/trackruler/linearmonoruler.h"
#include "global/realfn.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 20;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 10;
constexpr std::array<std::pair<double, double>, 4> STEP_INCREMENT = { { { 0.1, 0.05 }, { 0.5, 0.1 }, { 0.5, 0.25 }, { 1.0, 0.5 } } };
constexpr std::pair<double, double> DEFAULT_INCREMENT = { 1.0, 0.5 };

constexpr double MAX_VALUE = 1.0;
constexpr double MIN_VALUE = -1.0;

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

double valueToPosition(double value, double height)
{
    return (1.0 - (value / 2.0 + 0.5)) * height;
}

std::pair<double, double> stepsIncrement(double height)
{
    std::pair<double, double> increment = DEFAULT_INCREMENT;
    for (const auto& stepInc : STEP_INCREMENT) {
        const auto& [fs, ss] = stepInc;
        if ((valueToPosition(0.0, height) - valueToPosition(fs, height) >= MIN_ADJACENT_FULL_STEPS_HEIGHT)
            && (valueToPosition(0.0, height) - valueToPosition(ss, height) >= MIN_ADJACENT_SMALL_STEPS_HEIGHT)) {
            increment = stepInc;
            break;
        }
    }

    return increment;
}

std::vector<double> fullStepsValues(double height)
{
    const std::pair<double, double> increment = stepsIncrement(height);

    std::vector<double> steps;
    for (double v = MIN_VALUE; v <= MAX_VALUE; v += increment.first) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> smallStepsValues(double height)
{
    const std::pair<double, double> increment = stepsIncrement(height);

    std::vector<double> steps;
    for (double v = MIN_VALUE; v <= MAX_VALUE; v += increment.second) {
        steps.push_back(v);
    }
    return steps;
}
}

double LinearMonoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, [[maybe_unused]] bool isNegativeSample) const
{
    return valueToPosition(step, m_height);
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
        if (std::find_if(fullSteps.begin(), fullSteps.end(), [v](double fs) { return muse::RealIsEqual(v, fs); }) != fullSteps.end()) {
            continue;
        }

        result.push_back(TrackRulerSmallStep { v, 0, v < 0.0 });
    }

    return result;
}
