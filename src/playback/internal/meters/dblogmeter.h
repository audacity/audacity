/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "iplaybackmeter.h"

namespace au::playback {
class DbLogMeter : public IPlaybackMeter
{
public:
    DbLogMeter(double dbRange);

    double stepToPosition(double sample) const override;
    double sampleToPosition(double sample) const override;
    double positionToSample(double position) const override;
    std::string sampleToText(double sample) const override;

    void setDbRange(double dbRange) override;

    std::vector<double> fullSteps(int meterSize) const override;
    std::vector<double> smallSteps(int meterSize) const override;

private:
    double m_dbRange = 0.0;
};
}
