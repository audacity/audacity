/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

namespace au::playback {
class IPlaybackMeter
{
public:
    virtual ~IPlaybackMeter() = default;

    virtual double stepToPosition(double sample) const = 0;
    virtual double sampleToPosition(double sample) const = 0;
    virtual std::string sampleToText(double sample) const = 0;

    virtual std::vector<double> fullSteps(int meterSize) const = 0;
    virtual std::vector<double> smallSteps(int meterSize) const = 0;
};
}
