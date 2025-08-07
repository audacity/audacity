#include "projectscene/view/trackruler/linearmonoruler.h"

using namespace au::projectscene;

namespace {
constexpr std::array<TrackRulerFullStep, 5> FULL_STEPS = { {
    TrackRulerFullStep{ 1.0, 0, -1, true, false },
    TrackRulerFullStep{ 0.5, 0, 0, false, false },
    TrackRulerFullStep{ 0.0, 0, 0, true, true },
    TrackRulerFullStep{ -0.5, 0, 0, false, false },
    TrackRulerFullStep{ -1.0, 0, 1, true, false }
} };

constexpr std::array<TrackRulerSmallStep, 16> SMALL_STEPS = { {
    TrackRulerSmallStep{ 0, 0.9 },
    TrackRulerSmallStep{ 0, 0.8 },
    TrackRulerSmallStep{ 0, 0.7 },
    TrackRulerSmallStep{ 0, 0.6 },
    TrackRulerSmallStep{ 0, 0.4 },
    TrackRulerSmallStep{ 0, 0.3 },
    TrackRulerSmallStep{ 0, 0.2 },
    TrackRulerSmallStep{ 0, 0.1 },
    TrackRulerSmallStep{ 0, -0.1 },
    TrackRulerSmallStep{ 0, -0.2 },
    TrackRulerSmallStep{ 0, -0.3 },
    TrackRulerSmallStep{ 0, -0.4 },
    TrackRulerSmallStep{ 0, -0.6 },
    TrackRulerSmallStep{ 0, -0.7 },
    TrackRulerSmallStep{ 0, -0.8 },
    TrackRulerSmallStep{ 0, -0.9 }
} };

constexpr std::array<TrackRulerFullStep, 1> COLLAPSED_FULL_STEPS = { {
    TrackRulerFullStep{ 0.0, 0, 0, true, true },
} };

constexpr std::array<TrackRulerSmallStep, 2> COLLAPSED_SMALL_STEPS = { {
    TrackRulerSmallStep{ 0, 1.0 },
    TrackRulerSmallStep{ 0, -1.0 }
} };
}

double LinearMonoRuler::stepToPosition(double step, int) const
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

std::vector<TrackRulerFullStep> LinearMonoRuler::fullSteps() const
{
    if (m_collapsed) {
        return { COLLAPSED_FULL_STEPS.begin(), COLLAPSED_FULL_STEPS.end() };
    }

    return { FULL_STEPS.begin(), FULL_STEPS.end() };
}

std::vector<TrackRulerSmallStep> LinearMonoRuler::smallSteps() const
{
    if (m_collapsed) {
        return { COLLAPSED_SMALL_STEPS.begin(), COLLAPSED_SMALL_STEPS.end() };
    }

    return { SMALL_STEPS.begin(), SMALL_STEPS.end() };
}
