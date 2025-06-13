/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "iplaybackmeterstrategy.h"

namespace au::playback {
class LinearMeter : public IPlaybackMeterStrategy
{
public:
    LinearMeter() = default;

    double stepToPosition(double sample) const override;
    double sampleToPosition(double sample) const override;
    std::string sampleToText(double sample) const override;

    std::vector<double> fullSteps(int meterSize) const override;
    std::vector<double> smallSteps(int meterSize) const override;
};
}
