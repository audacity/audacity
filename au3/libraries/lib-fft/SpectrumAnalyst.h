/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumAnalyst.h

  Dominic Mazzoni
  Paul Licameli split from FreqWindow.h

**********************************************************************/

#pragma once

#include <vector>
#include <functional>

class FFT_API SpectrumAnalyst
{
public:

    enum Algorithm {
        Spectrum,
        Autocorrelation,
        CubeRootAutocorrelation,
        EnhancedAutocorrelation,
        Cepstrum,

        NumAlgorithms
    };

    using ProgressFn = std::function<void (long long num, long long den)>;

    SpectrumAnalyst();
    ~SpectrumAnalyst();

    // Return true iff successful
    bool Calculate(Algorithm alg, int windowFunc, // see FFT.h for values
                   size_t windowSize, double rate, const float* data, size_t dataLen, float* pYMin = NULL, float* pYMax = NULL, // outputs
                   ProgressFn progress = NULL);

    const float* GetProcessed() const;
    int GetProcessedSize() const;

    float GetProcessedValue(float freq0, float freq1) const;
    float FindPeak(float xPos, float* pY) const;

private:
    float CubicInterpolate(float y0, float y1, float y2, float y3, float x) const;
    float CubicMaximize(float y0, float y1, float y2, float y3, float* max) const;

private:
    Algorithm mAlg;
    double mRate;
    size_t mWindowSize;
    std::vector<float> mProcessed;
};
