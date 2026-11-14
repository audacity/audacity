/*
 * Audacity: A Digital Audio Editor
 */
#include "au3peakfinder.h"

#include "au3spectrogramsettings.h"
#include "au3spectrogramutils.h"

#include "au3-math/SampleCount.h"

namespace au::spectrogram {
Au3PeakFinder::Au3PeakFinder(const ::WaveTrack& track, int channel, double startTime, double endTime)
{
    const auto& settings = Au3SpectrogramSettings::Get(track);

    std::vector<float> frequencySnappingData;
    const auto start = track.TimeToLongSamples(startTime);
    const auto end = track.TimeToLongSamples(endTime);
    // TODO why this number?
    const auto length = std::min(frequencySnappingData.max_size(), ::limitSampleBufferSize(10485760, end - start));
    constexpr auto minLength = 8;
    const auto effectiveLength = std::max<int>(minLength, length);
    frequencySnappingData.resize(effectiveLength, 0.0f);
    constexpr auto backwards = false;
    // Don't try to cope with exceptions, just read zeroes instead.
    constexpr auto mayThrow = false;
    float* dataPtr = frequencySnappingData.data();
    track.GetFloats(channel, 1, &dataPtr, start, effectiveLength, backwards, FillFormat::fillZero, mayThrow);
    m_analyst.Calculate(
        ::SpectrumAnalyst::Spectrum, toAu3WindowType(settings.WindowType()),
        settings.GetFFTLength(), track.GetRate(), frequencySnappingData.data(), length);
}

double Au3PeakFinder::findPeak(double nearFrequency) const
{
    return m_analyst.FindPeak(nearFrequency, nullptr);
}
}
