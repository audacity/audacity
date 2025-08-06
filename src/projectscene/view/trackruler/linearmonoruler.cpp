#include "projectscene/view/trackruler/linearmonoruler.h"

using namespace au::projectscene;

double LinearMonoRuler::stepToPosition(double step, int, double height) const
{
    return (1.0 - (step / 2.0 + 0.5)) * height;
}

std::vector<TrackRulerFullStep> LinearMonoRuler::fullSteps() const
{
    return {
        TrackRulerFullStep{ -1, 1.0, 0, true, false },
        TrackRulerFullStep{ 0, 0.5, 0, false, false },
        TrackRulerFullStep{ 0, 0.0, 0, true, true },
        TrackRulerFullStep{ 0, -0.5, 0, false, false },
        TrackRulerFullStep{ 1, -1.0, 0, true, false }
    };
}

std::vector<TrackRulerSmallStep> LinearMonoRuler::smallSteps() const
{
    return {
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
    };
}

std::vector<TrackRulerFullStep> LinearMonoRuler::collapsedFullSteps() const
{
    return {
        TrackRulerFullStep{ 0, 0.0, 0, true, true },
    };
}

std::vector<TrackRulerSmallStep> LinearMonoRuler::collapsedSmallSteps() const
{
    return {
        TrackRulerSmallStep{ 0, 1.0 },
        TrackRulerSmallStep{ 0, -1.0 }
    };
}
