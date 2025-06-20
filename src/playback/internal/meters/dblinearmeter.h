/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "iplaybackmeter.h"

namespace au::playback {
class DbLinearMeter : public IPlaybackMeter
{
public:
    DbLinearMeter() = default;

    double stepToPosition(double sample) const override;
    double sampleToPosition(double sample) const override;
    std::string sampleToText(double sample) const override;

    std::vector<double> fullSteps(int meterSize) const override;
    std::vector<double> smallSteps(int meterSize) const override;
};
}
