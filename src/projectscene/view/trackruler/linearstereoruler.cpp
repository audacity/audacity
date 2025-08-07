#include "projectscene/view/trackruler/linearstereoruler.h"

using namespace au::projectscene;

namespace {
constexpr std::array<TrackRulerFullStep, 3> FULL_STEPS_CH_0 = { {
    TrackRulerFullStep{ 1.0, 0, -1, true, false },
    TrackRulerFullStep{ 0.0, 0, 0, true, true },
    TrackRulerFullStep{ -1.0, 0, 1, true, true }
} };

constexpr std::array<TrackRulerFullStep, 3> FULL_STEPS_CH_1 = { {
    TrackRulerFullStep{ 1.0, 1, -1, true, true },
    TrackRulerFullStep{ 0.0, 1, 0, true, true },
    TrackRulerFullStep{ -1.0, 1, 1, true, false },
} };

constexpr std::array<TrackRulerSmallStep, 2> SMALL_STEPS_CH_0 = { {
    TrackRulerSmallStep{ 0, 0.5 },
    TrackRulerSmallStep{ 0, -0.5 }
} };

constexpr std::array<TrackRulerSmallStep, 2> SMALL_STEPS_CH_1 = { {
    TrackRulerSmallStep{ 1, 0.5 },
    TrackRulerSmallStep{ 1, -0.5 }
} };

constexpr std::array<TrackRulerFullStep, 2> COLLAPSED_FULL_STEPS_CH_0 = { {
    TrackRulerFullStep{ 0.0, 0, 0, true, true },
} };

constexpr std::array<TrackRulerFullStep, 2> COLLAPSED_FULL_STEPS_CH_1 = { {
    TrackRulerFullStep{ 0.0, 1, 0, true, true },
} };

constexpr std::array<TrackRulerSmallStep, 1> COLLAPSED_SMALL_STEPS_CH_0 = { {
    TrackRulerSmallStep{ 0, 1.0 }
} };

constexpr std::array<TrackRulerSmallStep, 1> COLLAPSED_SMALL_STEPS_CH_1 = { {
    TrackRulerSmallStep{ 1, -1.0 }
} };

constexpr double MIN_CHANNEL_HEIGHT = 30.0;
}

double LinearStereoRuler::stepToPosition(double step, int channel) const
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

std::vector<TrackRulerFullStep> LinearStereoRuler::fullSteps() const
{
    std::vector<TrackRulerFullStep> steps;

    if (m_collapsed) {
        steps.insert(steps.end(), COLLAPSED_FULL_STEPS_CH_0.begin(), COLLAPSED_FULL_STEPS_CH_0.end());
        steps.insert(steps.end(), COLLAPSED_FULL_STEPS_CH_1.begin(), COLLAPSED_FULL_STEPS_CH_1.end());
        return steps;
    }

    if (m_height * m_channelHeightRatio < MIN_CHANNEL_HEIGHT) {
        steps.insert(steps.end(), COLLAPSED_FULL_STEPS_CH_0.begin(), COLLAPSED_FULL_STEPS_CH_0.end());
        steps.insert(steps.end(), FULL_STEPS_CH_1.begin(), FULL_STEPS_CH_1.end());
        return steps;
    }

    if (m_height * (1.0 - m_channelHeightRatio) < MIN_CHANNEL_HEIGHT) {
        steps.insert(steps.end(), FULL_STEPS_CH_0.begin(), FULL_STEPS_CH_0.end());
        steps.insert(steps.end(), COLLAPSED_FULL_STEPS_CH_1.begin(), COLLAPSED_FULL_STEPS_CH_1.end());
        return steps;
    }

    steps.insert(steps.end(), FULL_STEPS_CH_0.begin(), FULL_STEPS_CH_0.end());
    steps.insert(steps.end(), FULL_STEPS_CH_1.begin(), FULL_STEPS_CH_1.end());
    return steps;
}

std::vector<TrackRulerSmallStep> LinearStereoRuler::smallSteps() const
{
    std::vector<TrackRulerSmallStep> steps;
    if (m_collapsed) {
        steps.insert(steps.end(), COLLAPSED_SMALL_STEPS_CH_0.begin(), COLLAPSED_SMALL_STEPS_CH_0.end());
        steps.insert(steps.end(), COLLAPSED_SMALL_STEPS_CH_1.begin(), COLLAPSED_SMALL_STEPS_CH_1.end());
        return steps;
    }

    if (m_height * m_channelHeightRatio < MIN_CHANNEL_HEIGHT) {
        steps.insert(steps.end(), COLLAPSED_SMALL_STEPS_CH_0.begin(), COLLAPSED_SMALL_STEPS_CH_0.end());
        steps.insert(steps.end(), SMALL_STEPS_CH_1.begin(), SMALL_STEPS_CH_1.end());
        return steps;
    }

    if (m_height * (1.0 - m_channelHeightRatio) < MIN_CHANNEL_HEIGHT) {
        steps.insert(steps.end(), SMALL_STEPS_CH_0.begin(), SMALL_STEPS_CH_0.end());
        steps.insert(steps.end(), COLLAPSED_SMALL_STEPS_CH_1.begin(), COLLAPSED_SMALL_STEPS_CH_1.end());
        return steps;
    }

    steps.insert(steps.end(), SMALL_STEPS_CH_0.begin(), SMALL_STEPS_CH_0.end());
    steps.insert(steps.end(), SMALL_STEPS_CH_1.begin(), SMALL_STEPS_CH_1.end());
    return steps;
}
