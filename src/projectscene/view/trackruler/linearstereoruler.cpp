#include "projectscene/view/trackruler/linearstereoruler.h"

using namespace au::projectscene;

double LinearStereoRuler::stepToPosition(double step, int channel, double height) const
{
    double position;
    if (channel == 0) {
        position = (1.0 - (step / 2.0 + 0.5)) * height / 2;
        if (step == -1.0) {
            position += 1;
        }
    } else {
        position = (1.0 - (step / 2.0 + 0.5)) * height / 2 + height / 2;
        if (step == 1.0) {
            position += 1;
        }
    }

    return position;
}

std::vector<TrackRulerFullStep> LinearStereoRuler::fullSteps() const
{
    return {
        TrackRulerFullStep{ -1, 1.0, 0, true, false },
        TrackRulerFullStep{ 0, 0.0, 0, true, true },
        TrackRulerFullStep{ 1, -1.0, 0, true, true },
        TrackRulerFullStep{ -1, 1.0, 1, true, true },
        TrackRulerFullStep{ 0, 0.0, 1, true, true },
        TrackRulerFullStep{ 1, -1.0, 1, true, false },
    };
}

std::vector<TrackRulerSmallStep> LinearStereoRuler::smallSteps() const
{
    return {
        TrackRulerSmallStep{ 0, 0.5 },
        TrackRulerSmallStep{ 0, -0.5 },
        TrackRulerSmallStep{ 1, 0.5 },
        TrackRulerSmallStep{ 1, -0.5 },
    };
}

std::vector<TrackRulerFullStep> LinearStereoRuler::collapsedFullSteps() const
{
    return {
        TrackRulerFullStep{ 0, 0.0, 0, true, true },
        TrackRulerFullStep{ 0, 0.0, 1, true, true },
    };
}

std::vector<TrackRulerSmallStep> LinearStereoRuler::collapsedSmallSteps() const
{
    return {
        TrackRulerSmallStep{ 0, 1.0 },
        TrackRulerSmallStep{ 1, -1.0 }
    };
}
