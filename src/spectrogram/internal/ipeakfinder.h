/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::spectrogram {
class IPeakFinder
{
public:
    virtual ~IPeakFinder() = default;
    virtual double findPeak(double nearFrequency) const = 0;
};
}
