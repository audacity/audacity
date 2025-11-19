/**********************************************************************

Audacity: A Digital Audio Editor

EBUR128.h

Max Maisel

***********************************************************************/

#ifndef __EBUR128_H__
#define __EBUR128_H__

#include "Biquad.h"
#include <memory>
#include "SampleFormat.h"

#include <cmath>

/// \brief Implements EBU-R128 loudness measurement.
class MATH_API EBUR128
{
public:
    EBUR128(double rate, size_t channels);
    EBUR128(const EBUR128&) = delete;
    EBUR128(EBUR128&&) = delete;
    ~EBUR128() = default;

    static ArrayOf<Biquad> CalcWeightingFilter(double fs);
    void ProcessSampleFromChannel(float x_in, size_t channel) const;
    void NextSample();
    double IntegrativeLoudness();
    inline double IntegrativeLoudnessToLUFS(double loudness)
    { return 10 * log10(loudness); }

private:
    void HistogramSums(size_t start_idx, double& sum_v, long int& sum_c) const;
    void AddBlockToHistogram(size_t validLen);

    static constexpr size_t HIST_BIN_COUNT = 65536;
    /// EBU R128 absolute threshold
    static constexpr double GAMMA_A = (-70.0 + 0.691) / 10.0;
    ArrayOf<long int> mLoudnessHist;
    Doubles mBlockRingBuffer;
    size_t mSampleCount{ 0 };
    size_t mBlockRingPos{ 0 };
    size_t mBlockRingSize{ 0 };
    const size_t mChannelCount;
    const double mRate;
    const size_t mBlockSize;
    const size_t mBlockOverlap;

    /// This is be an array of arrays of the type
    /// mWeightingFilter[CHANNEL][FILTER] with
    /// CHANNEL = LEFT/RIGHT (0/1) and
    /// FILTER  = HSF/HPF    (0/1)
    ArrayOf<ArrayOf<Biquad> > mWeightingFilter;
};

#endif
