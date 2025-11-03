/*
* Audacity: A Digital Audio Editor
*/
#include "projectscene/view/trackruler/linearstereoruler.h"
#include "global/realfn.h"

using namespace au::projectscene;

namespace {
constexpr int MIN_ADJACENT_FULL_STEPS_HEIGHT = 20;
constexpr int MIN_ADJACENT_SMALL_STEPS_HEIGHT = 10;
constexpr std::array<std::pair<double, double>, 4> STEP_INCREMENT = { { { 0.1, 0.05 }, { 0.5, 0.1 }, { 0.5, 0.25 }, { 1.0, 0.5 } } };
constexpr std::pair<double, double> DEFAULT_INCREMENT = { 1.0, 0.5 };

constexpr double MAX_VALUE = 1.0;
constexpr double MIN_VALUE = -1.0;

constexpr std::array<TrackRulerFullStep, 2> COLLAPSED_FULL_STEPS = { {
    TrackRulerFullStep{ 0.0, 0, 0, true, true, false },
    TrackRulerFullStep{ 0.0, 1, 0, true, true, false },
} };

constexpr std::array<TrackRulerSmallStep, 2> COLLAPSED_SMALL_STEPS = { {
    TrackRulerSmallStep{ 1.0, 0, false },
    TrackRulerSmallStep{ -1.0, 1, false }
} };

constexpr double MIN_CHANNEL_HEIGHT = 40.0;

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

double LinearStereoRuler::stepToPosition(double step, [[maybe_unused]] size_t channel, [[maybe_unused]] bool isNegativeSample) const
{
    double position;
    if (channel == 0) {
        position = (1.0 - (step / 2.0 + 0.5)) * m_height * m_channelHeightRatio;
        if (step == -1.0) {
            position += 1;
        }
    } else {
        position = (1.0 - (step / 2.0 + 0.5)) * (1.0 - m_channelHeightRatio) * m_height + m_channelHeightRatio * m_height;
        if (step == 1.0) {
            position += 1;
        }
    }

    return position;
}

void LinearStereoRuler::setHeight(int height)
{
    m_height = height;
}

void LinearStereoRuler::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
}

void LinearStereoRuler::setCollapsed(bool isCollapsed)
{
    m_collapsed = isCollapsed;
}

std::string LinearStereoRuler::sampleToText(double sample) const
{
    std::stringstream ss;
    ss << std::fixed << std::setprecision(1) << std::abs(sample);
    return ss.str();
}

std::vector<TrackRulerFullStep> LinearStereoRuler::fullSteps() const
{
    std::vector<TrackRulerFullStep> steps;

    if (m_collapsed) {
        steps.insert(steps.end(), COLLAPSED_FULL_STEPS.begin(), COLLAPSED_FULL_STEPS.end());
        return steps;
    }

    std::vector<double> channelHeight = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t ch = 0; ch < channelHeight.size(); ++ch) {
        if (channelHeight[ch] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerFullStep { 0.0, ch, 0, true, true, false });
            continue;
        }

        const auto values = fullStepsValues(channelHeight[ch]);
        for (double value : values) {
            steps.push_back(TrackRulerFullStep { value, ch, getAlignment(value), isBold(value), value == 0.0, value < 0.0 });
        }
    }

    return steps;
}

std::vector<TrackRulerSmallStep> LinearStereoRuler::smallSteps() const
{
    std::vector<TrackRulerSmallStep> steps;
    if (m_collapsed) {
        steps.insert(steps.end(), COLLAPSED_SMALL_STEPS.begin(), COLLAPSED_SMALL_STEPS.end());
        return steps;
    }

    std::vector<double> channelHeight = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t ch = 0; ch < 2; ++ch) {
        if (channelHeight[ch] < MIN_CHANNEL_HEIGHT) {
            steps.push_back(TrackRulerSmallStep { 0.0, ch, false });
            continue;
        }

        const auto values = smallStepsValues(channelHeight[ch]);
        const auto fullSteps = fullStepsValues(channelHeight[ch]);
        for (double v : values) {
            if (std::find_if(fullSteps.begin(), fullSteps.end(), [v](double fs) { return muse::RealIsEqual(v, fs); }) != fullSteps.end()) {
                continue;
            }
            steps.push_back(TrackRulerSmallStep { v, ch,  v < 0.0 });
        }
    }

    return steps;
}
