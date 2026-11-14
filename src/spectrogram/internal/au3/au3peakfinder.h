/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../ipeakfinder.h"

#include "au3-wave-track/WaveTrack.h"
#include "au3-fft/SpectrumAnalyst.h"

namespace au::spectrogram {
class Au3PeakFinder : public IPeakFinder
{
public:
    Au3PeakFinder(const ::WaveTrack&, int channel, double startTime, double endTime);
    ~Au3PeakFinder() override = default;

    double findPeak(double nearFrequency) const override;

private:
    ::SpectrumAnalyst m_analyst;
};
}
