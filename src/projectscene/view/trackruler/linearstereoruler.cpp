#include "projectscene/view/trackruler/linearstereoruler.h"
#include "global/realfn.h"

using namespace au::projectscene;

namespace {
constexpr int MEDIUM_RESOLUTION_MIN_HEIGHT = 105;
constexpr int HIGH_RESOLUTION_MIN_HEIGHT = 350;

constexpr double MAX_VALUE = 1.0;
constexpr double MIN_VALUE = -1.0;

constexpr double LOW_RESOLUTION_FULL_STEPS_INCREMENT = 1.0;
constexpr double LOW_RESOLUTION_SMALL_STEPS_INCREMENT = 0.5;

constexpr double MEDIUM_RESOLUTION_FULL_STEPS_INCREMENT = 0.5;
constexpr double MEDIUM_RESOLUTION_SMALL_STEPS_INCREMENT = 0.1;

constexpr double HIGH_RESOLUTION_FULL_STEPS_INCREMENT = 0.1;
constexpr double HIGH_RESOLUTION_SMALL_STEPS_INCREMENT = 0.05;

constexpr double MIN_CHANNEL_HEIGHT = 30.0;

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
    double increment;
    if (height >= HIGH_RESOLUTION_MIN_HEIGHT) {
        increment = HIGH_RESOLUTION_FULL_STEPS_INCREMENT;
    } else if (height >= MEDIUM_RESOLUTION_MIN_HEIGHT) {
        increment = MEDIUM_RESOLUTION_FULL_STEPS_INCREMENT;
    } else {
        increment = LOW_RESOLUTION_FULL_STEPS_INCREMENT;
    }

    std::vector<double> steps;
    for (double v = MIN_VALUE; v <= MAX_VALUE; v += increment) {
        steps.push_back(v);
    }
    return steps;
}

std::vector<double> smallStepsValues(double height)
{
    double increment;
    if (height >= HIGH_RESOLUTION_MIN_HEIGHT) {
        increment = HIGH_RESOLUTION_SMALL_STEPS_INCREMENT;
    } else if (height >= MEDIUM_RESOLUTION_MIN_HEIGHT) {
        increment = MEDIUM_RESOLUTION_SMALL_STEPS_INCREMENT;
    } else {
        increment = LOW_RESOLUTION_SMALL_STEPS_INCREMENT;
    }

    std::vector<double> steps;
    for (double v = MIN_VALUE; v <= MAX_VALUE; v += increment) {
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
        steps.push_back(TrackRulerFullStep { 0.0, 0, 0, true, true, false });
        steps.push_back(TrackRulerFullStep { 0.0, 1, 0, true, true, false });
        return steps;
    }

    std::vector<double> channelHeight = { m_height* m_channelHeightRatio, m_height* (1.0 - m_channelHeightRatio) };
    for (size_t ch = 0; ch < 2; ++ch) {
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
        steps.push_back(TrackRulerSmallStep { 1.0, 0, false });
        steps.push_back(TrackRulerSmallStep { -1.0, 1, false });
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
            if (std::find(fullSteps.begin(), fullSteps.end(), v) != fullSteps.end()) {
                continue;
            }
            steps.push_back(TrackRulerSmallStep { v, ch,  v < 0.0 });
        }
    }

    return steps;
}
