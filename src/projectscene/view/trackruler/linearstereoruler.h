#pragma once

#include "projectscene/view/trackruler/itrackrulermodel.h"

namespace au::projectscene {
class LinearStereoRuler : public ITrackRulerModel
{
public:
    double stepToPosition(double step, int channel, double height) const override;
    std::vector<TrackRulerFullStep> fullSteps() const override;
    std::vector<TrackRulerSmallStep> smallSteps() const override;
    std::vector<TrackRulerFullStep> collapsedFullSteps() const override;
    std::vector<TrackRulerSmallStep> collapsedSmallSteps() const override;
};
}
