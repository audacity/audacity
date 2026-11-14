/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "projectscene/view/trackruler/dblogbaseruler.h"
#include "view/trackruler/itrackruler.h"

namespace au::projectscene {
class DbLogStereoRuler : public DbLogBaseRuler
{
public:
    DbLogStereoRuler();
    double stepToPosition(double step, size_t channel, bool isNegativeSample) const override;
    std::vector<TrackRulerFullStep> fullSteps() const override;
    std::vector<TrackRulerSmallStep> smallSteps() const override;
private:
    std::vector<TrackRulerFullStep> fullStepsForHalfWave() const;
    std::vector<TrackRulerFullStep> fullStepsForFullWave() const;
    std::vector<TrackRulerSmallStep> smallStepsForHalfWave() const;
    std::vector<TrackRulerSmallStep> smallStepsForFullWave() const;
    int getAlignment(double value, size_t channel, bool isNegativeSample) const;
    bool isFullTick(double value, size_t channel, bool isNegativeSample) const;
};
}
