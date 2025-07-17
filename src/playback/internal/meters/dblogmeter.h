/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "iplaybackmeter.h"

namespace au::playback {
class DbLogMeter : public IPlaybackMeter
{
public:
    DbLogMeter(int meterSize, double dbRange);

    double stepToPosition(double sample) const override;
    double sampleToPosition(double sample) const override;
    std::string sampleToText(double sample) const override;

    void setMeterSize(int meterSize) override;
    void setDbRange(double dbRange) override;

    std::vector<double> fullSteps() const override;
    std::vector<double> smallSteps() const override;

private:
    int m_meterSize = 0;
    double m_dbRange = 0.0;
};
}
