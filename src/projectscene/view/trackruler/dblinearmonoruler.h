/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "dblinearbaseruler.h"

namespace au::projectscene {
class DbLinearMonoRuler : public DbLinearBaseRuler
{
public:
    DbLinearMonoRuler();
    double stepToPosition(double step, size_t channel, bool isNegativeSample) const override;
    std::vector<TrackRulerFullStep> fullSteps() const override;
    std::vector<TrackRulerSmallStep> smallSteps() const override;
};
}
