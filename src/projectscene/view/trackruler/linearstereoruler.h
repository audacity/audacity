/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/linearbaseruler.h"

namespace au::projectscene {
class LinearStereoRuler : public LinearBaseRuler
{
public:
    [[nodiscard]] double stepToPosition(double step, size_t channel, bool isNegativeSample) const override;
    [[nodiscard]] std::vector<TrackRulerFullStep> fullSteps() const override;
    [[nodiscard]] std::vector<TrackRulerSmallStep> smallSteps() const override;
};
}
