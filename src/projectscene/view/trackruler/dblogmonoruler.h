/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "dblogbaseruler.h"

namespace au::projectscene {
class DbLogMonoRuler : public DbLogBaseRuler
{
public:
    DbLogMonoRuler();
    double stepToPosition(double step, size_t channel, bool isNegativeSample) const override;
    std::vector<TrackRulerFullStep> fullSteps() const override;
    std::vector<TrackRulerSmallStep> smallSteps() const override;
private:
    int getAlignment(double value, bool isNegativeSample) const;
};
}
