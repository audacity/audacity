/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumAnalyst.cpp

  Dominic Mazzoni
  Paul Licameli split from FreqWindow.cpp

*******************************************************************//**

\class SpectrumAnalyst
\brief Used for finding the peaks, for snapping to peaks.

This class is used to do the 'find peaks' snapping both in FreqPlot
and in the spectrogram spectral selection.

*//*******************************************************************/

/*
  Salvo Ventura - November 2006
  Extended range check for additional FFT windows
*/

#include "SpectrumAnalyst.h"
#include "FFT.h"
#include "MemoryX.h"

SpectrumAnalyst::SpectrumAnalyst()
    : mAlg(Spectrum)
    , mRate(0.0)
    , mWindowSize(0)
{
}

SpectrumAnalyst::~SpectrumAnalyst()
{
}

bool SpectrumAnalyst::Calculate(Algorithm alg, int windowFunc,
                                size_t windowSize, double rate,
                                const float* data, size_t dataLen,
                                float* pYMin, float* pYMax,
                                ProgressFn progress)
{
    // Wipe old data
    mProcessed.resize(0);
    mRate = 0.0;
    mWindowSize = 0;

    // Validate inputs
    int f = NumWindowFuncs();

    if (!(windowSize >= 32 && windowSize <= 131072
          && alg >= SpectrumAnalyst::Spectrum
          && alg < SpectrumAnalyst::NumAlgorithms
          && windowFunc >= 0 && windowFunc < f)) {
        return false;
    }

    if (dataLen < windowSize) {
        return false;
    }

    // Now repopulate
    mRate = rate;
    mWindowSize = windowSize;
    mAlg = alg;

    auto half = mWindowSize / 2;
    mProcessed.resize(mWindowSize);

    ArrayOf<float> in{ mWindowSize };
    ArrayOf<float> out{ mWindowSize };
    ArrayOf<float> out2{ mWindowSize };
    ArrayOf<float> win{ mWindowSize };

    for (size_t i = 0; i < mWindowSize; i++) {
        mProcessed[i] = 0.0f;
        win[i] = 1.0f;
    }

    WindowFunc(windowFunc, mWindowSize, win.get());

    // Scale window such that an amplitude of 1.0 in the time domain
    // shows an amplitude of 0dB in the frequency domain
    double wss = 0;
    for (size_t i = 0; i < mWindowSize; i++) {
        wss += win[i];
    }
    if (wss > 0) {
        wss = 4.0 / (wss * wss);
    } else {
        wss = 1.0;
    }

    size_t start = 0;
    int windows = 0;
    while (start + mWindowSize <= dataLen) {
        for (size_t i = 0; i < mWindowSize; i++) {
            in[i] = win[i] * data[start + i];
        }

        switch (alg) {
        case Spectrum:
            PowerSpectrum(mWindowSize, in.get(), out.get());

            for (size_t i = 0; i < half; i++) {
                mProcessed[i] += out[i];
            }
            break;

        case Autocorrelation:
        case CubeRootAutocorrelation:
        case EnhancedAutocorrelation:

            // Take FFT
            RealFFT(mWindowSize, in.get(), out.get(), out2.get());
            // Compute power
            for (size_t i = 0; i < mWindowSize; i++) {
                in[i] = (out[i] * out[i]) + (out2[i] * out2[i]);
            }

            if (alg == Autocorrelation) {
                for (size_t i = 0; i < mWindowSize; i++) {
                    in[i] = sqrt(in[i]);
                }
            }
            if (alg == CubeRootAutocorrelation
                || alg == EnhancedAutocorrelation) {
                // Tolonen and Karjalainen recommend taking the cube root
                // of the power, instead of the square root

                for (size_t i = 0; i < mWindowSize; i++) {
                    in[i] = pow(in[i], 1.0f / 3.0f);
                }
            }
            // Take FFT
            RealFFT(mWindowSize, in.get(), out.get(), out2.get());

            // Take real part of result
            for (size_t i = 0; i < half; i++) {
                mProcessed[i] += out[i];
            }
            break;

        case Cepstrum:
            RealFFT(mWindowSize, in.get(), out.get(), out2.get());

            // Compute log power
            // Set a sane lower limit assuming maximum time amplitude of 1.0
            {
                float power;
                float minpower = 1e-20 * mWindowSize * mWindowSize;
                for (size_t i = 0; i < mWindowSize; i++) {
                    power = (out[i] * out[i]) + (out2[i] * out2[i]);
                    if (power < minpower) {
                        in[i] = log(minpower);
                    } else {
                        in[i] = log(power);
                    }
                }
                // Take IFFT
                InverseRealFFT(mWindowSize, in.get(), NULL, out.get());

                // Take real part of result
                for (size_t i = 0; i < half; i++) {
                    mProcessed[i] += out[i];
                }
            }

            break;

        default:
            wxASSERT(false);
            break;
        }                       //switch

        if (progress) {
            progress(start, dataLen);
        }

        start += half;
        windows++;
    }

    float mYMin = 1000000, mYMax = -1000000;
    double scale;
    switch (alg) {
    case Spectrum:
        // Convert to decibels
        mYMin = 1000000.;
        mYMax = -1000000.;
        scale = wss / (double)windows;
        for (size_t i = 0; i < half; i++) {
            mProcessed[i] = 10 * log10(mProcessed[i] * scale);
            if (mProcessed[i] > mYMax) {
                mYMax = mProcessed[i];
            } else if (mProcessed[i] < mYMin) {
                mYMin = mProcessed[i];
            }
        }
        break;

    case Autocorrelation:
    case CubeRootAutocorrelation:
        for (size_t i = 0; i < half; i++) {
            mProcessed[i] = mProcessed[i] / windows;
        }

        // Find min/max
        mYMin = mProcessed[0];
        mYMax = mProcessed[0];
        for (size_t i = 1; i < half; i++) {
            if (mProcessed[i] > mYMax) {
                mYMax = mProcessed[i];
            } else if (mProcessed[i] < mYMin) {
                mYMin = mProcessed[i];
            }
        }
        break;

    case EnhancedAutocorrelation:
        for (size_t i = 0; i < half; i++) {
            mProcessed[i] = mProcessed[i] / windows;
        }

        // Peak Pruning as described by Tolonen and Karjalainen, 2000

        // Clip at zero, copy to temp array
        for (size_t i = 0; i < half; i++) {
            if (mProcessed[i] < 0.0) {
                mProcessed[i] = float(0.0);
            }
            out[i] = mProcessed[i];
        }

        // Subtract a time-doubled signal (linearly interp.) from the original
        // (clipped) signal
        for (size_t i = 0; i < half; i++) {
            if ((i % 2) == 0) {
                mProcessed[i] -= out[i / 2];
            } else {
                mProcessed[i] -= ((out[i / 2] + out[i / 2 + 1]) / 2);
            }
        }

        // Clip at zero again
        for (size_t i = 0; i < half; i++) {
            if (mProcessed[i] < 0.0) {
                mProcessed[i] = float(0.0);
            }
        }

        // Find NEW min/max
        mYMin = mProcessed[0];
        mYMax = mProcessed[0];
        for (size_t i = 1; i < half; i++) {
            if (mProcessed[i] > mYMax) {
                mYMax = mProcessed[i];
            } else if (mProcessed[i] < mYMin) {
                mYMin = mProcessed[i];
            }
        }
        break;

    case Cepstrum:
        for (size_t i = 0; i < half; i++) {
            mProcessed[i] = mProcessed[i] / windows;
        }

        // Find min/max, ignoring first and last few values
        {
            size_t ignore = 4;
            mYMin = mProcessed[ignore];
            mYMax = mProcessed[ignore];
            for (size_t i = ignore + 1; i + ignore < half; i++) {
                if (mProcessed[i] > mYMax) {
                    mYMax = mProcessed[i];
                } else if (mProcessed[i] < mYMin) {
                    mYMin = mProcessed[i];
                }
            }
        }
        break;

    default:
        wxASSERT(false);
        break;
    }

    if (pYMin) {
        *pYMin = mYMin;
    }
    if (pYMax) {
        *pYMax = mYMax;
    }

    return true;
}

const float* SpectrumAnalyst::GetProcessed() const
{
    return &mProcessed[0];
}

int SpectrumAnalyst::GetProcessedSize() const
{
    return mProcessed.size() / 2;
}

float SpectrumAnalyst::GetProcessedValue(float freq0, float freq1) const
{
    float bin0, bin1, binwidth;

    if (mAlg == Spectrum) {
        bin0 = freq0 * mWindowSize / mRate;
        bin1 = freq1 * mWindowSize / mRate;
    } else {
        bin0 = freq0 * mRate;
        bin1 = freq1 * mRate;
    }
    binwidth = bin1 - bin0;

    float value = float(0.0);

    if (binwidth < 1.0) {
        float binmid = (bin0 + bin1) / 2.0;
        int ibin = (int)(binmid) - 1;
        if (ibin < 1) {
            ibin = 1;
        }
        if (ibin >= GetProcessedSize() - 3) {
            ibin = std::max(0, GetProcessedSize() - 4);
        }

        value = CubicInterpolate(mProcessed[ibin],
                                 mProcessed[ibin + 1],
                                 mProcessed[ibin + 2],
                                 mProcessed[ibin + 3], binmid - ibin);
    } else {
        if (bin0 < 0) {
            bin0 = 0;
        }
        if (bin1 >= GetProcessedSize()) {
            bin1 = GetProcessedSize() - 1;
        }

        if ((int)(bin1) > (int)(bin0)) {
            value += mProcessed[(int)(bin0)] * ((int)(bin0) + 1 - bin0);
        }
        bin0 = 1 + (int)(bin0);
        while (bin0 < (int)(bin1)) {
            value += mProcessed[(int)(bin0)];
            bin0 += 1.0;
        }
        value += mProcessed[(int)(bin1)] * (bin1 - (int)(bin1));

        value /= binwidth;
    }

    return value;
}

float SpectrumAnalyst::FindPeak(float xPos, float* pY) const
{
    float bestpeak = 0.0f;
    float bestValue = 0.0;
    if (GetProcessedSize() > 1) {
        bool up = (mProcessed[1] > mProcessed[0]);
        float bestdist = 1000000;
        for (int bin = 3; bin < GetProcessedSize() - 1; bin++) {
            bool nowUp = mProcessed[bin] > mProcessed[bin - 1];
            if (!nowUp && up) {
                // Local maximum.  Find actual value by cubic interpolation
                int leftbin = bin - 2;
                /*
                if (leftbin < 1)
                   leftbin = 1;
                   */
                float valueAtMax = 0.0;
                float max = leftbin + CubicMaximize(mProcessed[leftbin],
                                                    mProcessed[leftbin + 1],
                                                    mProcessed[leftbin + 2],
                                                    mProcessed[leftbin + 3],
                                                    &valueAtMax);

                float thispeak;
                if (mAlg == Spectrum) {
                    thispeak = max * mRate / mWindowSize;
                } else {
                    thispeak = max / mRate;
                }

                if (fabs(thispeak - xPos) < bestdist) {
                    bestpeak = thispeak;
                    bestdist = fabs(thispeak - xPos);
                    bestValue = valueAtMax;
                    // Should this test come after the enclosing if?
                    if (thispeak > xPos) {
                        break;
                    }
                }
            }
            up = nowUp;
        }
    }

    if (pY) {
        *pY = bestValue;
    }
    return bestpeak;
}

// If f(0)=y0, f(1)=y1, f(2)=y2, and f(3)=y3, this function finds
// the degree-three polynomial which best fits these points and
// returns the value of this polynomial at a value x.  Usually
// 0 < x < 3

float SpectrumAnalyst::CubicInterpolate(float y0, float y1, float y2, float y3, float x) const
{
    float a, b, c, d;

    a = y0 / -6.0 + y1 / 2.0 - y2 / 2.0 + y3 / 6.0;
    b = y0 - 5.0 * y1 / 2.0 + 2.0 * y2 - y3 / 2.0;
    c = -11.0 * y0 / 6.0 + 3.0 * y1 - 3.0 * y2 / 2.0 + y3 / 3.0;
    d = y0;

    float xx = x * x;
    float xxx = xx * x;

    return a * xxx + b * xx + c * x + d;
}

float SpectrumAnalyst::CubicMaximize(float y0, float y1, float y2, float y3, float* max) const
{
    // Find coefficients of cubic

    float a, b, c, d;

    a = y0 / -6.0 + y1 / 2.0 - y2 / 2.0 + y3 / 6.0;
    b = y0 - 5.0 * y1 / 2.0 + 2.0 * y2 - y3 / 2.0;
    c = -11.0 * y0 / 6.0 + 3.0 * y1 - 3.0 * y2 / 2.0 + y3 / 3.0;
    d = y0;

    // Take derivative

    float da, db, dc;

    da = 3 * a;
    db = 2 * b;
    dc = c;

    // Find zeroes of derivative using quadratic equation

    float discriminant = db * db - 4 * da * dc;
    if (discriminant < 0.0) {
        return float(-1.0);            // error
    }
    float x1 = (-db + sqrt(discriminant)) / (2 * da);
    float x2 = (-db - sqrt(discriminant)) / (2 * da);

    // The one which corresponds to a local _maximum_ in the
    // cubic is the one we want - the one with a negative
    // second derivative

    float dda = 2 * da;
    float ddb = db;

    if (dda * x1 + ddb < 0) {
        *max = a * x1 * x1 * x1 + b * x1 * x1 + c * x1 + d;
        return x1;
    } else {
        *max = a * x2 * x2 * x2 + b * x2 * x2 + c * x2 + d;
        return x2;
    }
}
