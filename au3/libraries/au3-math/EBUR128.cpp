/**********************************************************************

Audacity: A Digital Audio Editor

EBUR128.cpp

Max Maisel

***********************************************************************/

#include "EBUR128.h"
#include <cstring>

EBUR128::EBUR128(double rate, size_t channels)
    : mChannelCount{channels}
    , mRate{rate}
    , mBlockSize(ceil(0.4 * mRate))  // 400 ms blocks
    , mBlockOverlap(ceil(0.1 * mRate))  // 100 ms overlap
{
    mLoudnessHist.reinit(HIST_BIN_COUNT, false);
    mBlockRingBuffer.reinit(mBlockSize);
    mWeightingFilter.reinit(mChannelCount, false);
    for (size_t channel = 0; channel < mChannelCount; ++channel) {
        mWeightingFilter[channel] = CalcWeightingFilter(mRate);
    }

    memset(mLoudnessHist.get(), 0, HIST_BIN_COUNT * sizeof(long int));
    for (size_t channel = 0; channel < mChannelCount; ++channel) {
        mWeightingFilter[channel][0].Reset();
        mWeightingFilter[channel][1].Reset();
    }
}

// fs: sample rate
// returns array of two Biquads
//
// EBU R128 parameter sampling rate adaption after
// Mansbridge, Stuart, Saoirse Finn, and Joshua D. Reiss.
// "Implementation and Evaluation of Autonomous Multi-track Fader Control."
// Paper presented at the 132nd Audio Engineering Society Convention,
// Budapest, Hungary, 2012."
ArrayOf<Biquad> EBUR128::CalcWeightingFilter(double fs)
{
    ArrayOf<Biquad> pBiquad(size_t(2), true);

    //
    // HSF pre filter
    //
    double db =    3.999843853973347;
    double f0 = 1681.974450955533;
    double Q  =    0.7071752369554196;
    double K  = tan(M_PI * f0 / fs);

    double Vh = pow(10.0, db / 20.0);
    double Vb = pow(Vh, 0.4996667741545416);

    double a0 = 1.0 + K / Q + K * K;

    pBiquad[0].fNumerCoeffs[Biquad::B0] = (Vh + Vb * K / Q + K * K) / a0;
    pBiquad[0].fNumerCoeffs[Biquad::B1] =       2.0 * (K * K - Vh) / a0;
    pBiquad[0].fNumerCoeffs[Biquad::B2] = (Vh - Vb * K / Q + K * K) / a0;

    pBiquad[0].fDenomCoeffs[Biquad::A1] =   2.0 * (K * K - 1.0) / a0;
    pBiquad[0].fDenomCoeffs[Biquad::A2] = (1.0 - K / Q + K * K) / a0;

    //
    // HPF weighting filter
    //
    f0 = 38.13547087602444;
    Q  =  0.5003270373238773;
    K  = tan(M_PI * f0 / fs);

    pBiquad[1].fNumerCoeffs[Biquad::B0] =  1.0;
    pBiquad[1].fNumerCoeffs[Biquad::B1] = -2.0;
    pBiquad[1].fNumerCoeffs[Biquad::B2] =  1.0;

    pBiquad[1].fDenomCoeffs[Biquad::A1] = 2.0 * (K * K - 1.0) / (1.0 + K / Q + K * K);
    pBiquad[1].fDenomCoeffs[Biquad::A2] = (1.0 - K / Q + K * K) / (1.0 + K / Q + K * K);

    return pBiquad;
}

void EBUR128::ProcessSampleFromChannel(float x_in, size_t channel) const
{
    double value;
    value = mWeightingFilter[channel][0].ProcessOne(x_in);
    value = mWeightingFilter[channel][1].ProcessOne(value);
    if (channel == 0) {
        mBlockRingBuffer[mBlockRingPos] = value * value;
    } else {
        // Add the power of additional channels to the power of first channel.
        // As a result, stereo tracks appear about 3 LUFS louder, as specified.
        mBlockRingBuffer[mBlockRingPos] += value * value;
    }
}

void EBUR128::NextSample()
{
    ++mBlockRingPos;
    ++mBlockRingSize;

    if (mBlockRingPos % mBlockOverlap == 0) {
        // A new full block of samples was submitted.
        if (mBlockRingSize >= mBlockSize) {
            AddBlockToHistogram(mBlockSize);
        }
    }
    // Close the ring.
    if (mBlockRingPos == mBlockSize) {
        mBlockRingPos = 0;
    }
    ++mSampleCount;
}

double EBUR128::IntegrativeLoudness()
{
    // EBU R128: z_i = mean square without root

    // Calculate Gamma_R from histogram.
    double sum_v;
    long int sum_c;
    HistogramSums(0, sum_v, sum_c);

    // Handle incomplete block if no non-zero block was found.
    if (sum_c == 0) {
        AddBlockToHistogram(mBlockRingSize);
        HistogramSums(0, sum_v, sum_c);
    }

    // Histogram values are simplified log(x^2) immediate values
    // without -0.691 + 10*(...) to safe computing power. This is
    // possible because they will cancel out anyway.
    // The -1 in the line below is the -10 LUFS from the EBU R128
    // specification without the scaling factor of 10.
    double Gamma_R = log10(sum_v / sum_c) - 1;
    size_t idx_R = round((Gamma_R - GAMMA_A) * double(HIST_BIN_COUNT) / -GAMMA_A - 1);

    // Apply Gamma_R threshold and calculate gated loudness (extent).
    HistogramSums(idx_R + 1, sum_v, sum_c);
    if (sum_c == 0) {
        // Silence was processed.
        return 0;
    }
    // LUFS is defined as -0.691 dB + 10*log10(sum(channels))
    return 0.8529037031 * sum_v / sum_c;
}

void
EBUR128::HistogramSums(size_t start_idx, double& sum_v, long int& sum_c) const
{
    double val;
    sum_v = 0;
    sum_c = 0;
    for (size_t i = start_idx; i < HIST_BIN_COUNT; ++i) {
        val = -GAMMA_A / double(HIST_BIN_COUNT) * (i + 1) + GAMMA_A;
        sum_v += pow(10, val) * mLoudnessHist[i];
        sum_c += mLoudnessHist[i];
    }
}

/// Process new full block. Incomplete blocks shall be discarded
/// according to the EBU R128 specification there is usually no need
/// to call this on the last block.
/// However, allow to override the block size if the audio to be
/// processed is shorter than one block.
void EBUR128::AddBlockToHistogram(size_t validLen)
{
    // Reset mBlockRingSize to full state to avoid overflow.
    // The actual value of mBlockRingSize does not matter
    // since this is only used to detect if blocks are complete (>= mBlockSize).
    mBlockRingSize = mBlockSize;

    size_t idx;
    double blockVal = 0;
    for (size_t i = 0; i < validLen; ++i) {
        blockVal += mBlockRingBuffer[i];
    }

    // Histogram values are simplified log10() immediate values
    // without -0.691 + 10*(...) to safe computing power. This is
    // possible because these constant cancel out anyway during the
    // following processing steps.
    blockVal = log10(blockVal / double(validLen));
    // log(blockVal) is within ]-inf, 1]
    idx = round((blockVal - GAMMA_A) * double(HIST_BIN_COUNT) / -GAMMA_A - 1);

    // idx is within ]-inf, HIST_BIN_COUNT-1], discard indices below 0
    // as they are below the EBU R128 absolute threshold anyway.
    if (idx < HIST_BIN_COUNT) {
        ++mLoudnessHist[idx];
    }
}
