/**********************************************************************

  Audacity: A Digital Audio Editor

  Spectrum.cpp

  Dominic Mazzoni

*******************************************************************//*!

\file Spectrum.cpp
\brief Functions for computing Spectra.

*//*******************************************************************/

#include "Spectrum.h"

#include <math.h>
#include "MemoryX.h"
using Floats = ArrayOf<float>;

bool ComputeSpectrum(
    const float* data, size_t width, size_t windowSize, float* output,
    bool autocorrelation, int windowFunc)
{
    if (width < windowSize) {
        return false;
    }

    if (!data || !output) {
        return true;
    }

    Floats processed{ windowSize };

    for (size_t i = 0; i < windowSize; i++) {
        processed[i] = float(0.0);
    }
    auto half = windowSize / 2;

    Floats in{ windowSize };
    Floats out{ windowSize };
    Floats out2{ windowSize };

    size_t start = 0;
    unsigned windows = 0;
    while (start + windowSize <= width) {
        for (size_t i = 0; i < windowSize; i++) {
            in[i] = data[start + i];
        }

        WindowFunc(windowFunc, windowSize, in.get());

        if (autocorrelation) {
            // Take FFT
            RealFFT(windowSize, in.get(), out.get(), out2.get());
            // Compute power
            for (size_t i = 0; i < windowSize; i++) {
                in[i] = (out[i] * out[i]) + (out2[i] * out2[i]);
            }

            // Tolonen and Karjalainen recommend taking the cube root
            // of the power, instead of the square root

            for (size_t i = 0; i < windowSize; i++) {
                in[i] = powf(in[i], 1.0f / 3.0f);
            }

            // Take FFT
            RealFFT(windowSize, in.get(), out.get(), out2.get());
        } else {
            PowerSpectrum(windowSize, in.get(), out.get());
        }

        // Take real part of result
        for (size_t i = 0; i < half; i++) {
            processed[i] += out[i];
        }

        start += half;
        windows++;
    }

    if (autocorrelation) {
        // Peak Pruning as described by Tolonen and Karjalainen, 2000
        /*
         Combine most of the calculations in a single for loop.
         It should be safe, as indexes refer only to current and previous elements,
         that have already been clipped, etc...
        */
        for (size_t i = 0; i < half; i++) {
            // Clip at zero, copy to temp array
            if (processed[i] < 0.0) {
                processed[i] = float(0.0);
            }
            out[i] = processed[i];
            // Subtract a time-doubled signal (linearly interp.) from the original
            // (clipped) signal
            if ((i % 2) == 0) {
                processed[i] -= out[i / 2];
            } else {
                processed[i] -= ((out[i / 2] + out[i / 2 + 1]) / 2);
            }

            // Clip at zero again
            if (processed[i] < 0.0) {
                processed[i] = float(0.0);
            }
        }

        // Reverse and scale
        for (size_t i = 0; i < half; i++) {
            in[i] = processed[i] / (windowSize / 4);
        }
        for (size_t i = 0; i < half; i++) {
            processed[half - 1 - i] = in[i];
        }
    } else {
        // Convert to decibels
        // But do it safely; -Inf is nobody's friend
        for (size_t i = 0; i < half; i++) {
            float temp=(processed[i] / windowSize / windows);
            if (temp > 0.0) {
                processed[i] = 10 * log10(temp);
            } else {
                processed[i] = 0;
            }
        }
    }

    for (size_t i = 0; i < half; i++) {
        output[i] = processed[i];
    }

    return true;
}
