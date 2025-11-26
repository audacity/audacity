/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/dblinearbaseruler.h"

namespace au::projectscene {
class DbLinearStereoRuler : public DbLinearBaseRuler
{
public:
    DbLinearStereoRuler();
    double stepToPosition(double step, size_t channel, bool isNegativeSample) const override;
    std::vector<TrackRulerFullStep> fullSteps() const override;
    std::vector<TrackRulerSmallStep> smallSteps() const override;
};
}
