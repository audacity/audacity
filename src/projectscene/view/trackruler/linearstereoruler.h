/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/linearbaseruler.h"

namespace au::projectscene {
class LinearStereoRuler : public LinearBaseRuler
{
public:
    double stepToPosition(double step, size_t channel, bool isNegativeSample) const override;
    std::vector<TrackRulerFullStep> fullSteps() const override;
    std::vector<TrackRulerSmallStep> smallSteps() const override;

private:
    bool isFullTick(double value, size_t channel) const;
};
}
