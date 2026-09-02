#ifndef AUDACITY_CVSD_H
#define AUDACITY_CVSD_H

#include "au3-math/SampleFormat.h"

struct CVSD_CONFIG {
    // 16 bit config for CVSD
    unsigned int num_bits = 1;

    // J (bit history window size for syllabic companding)
    static constexpr unsigned int bitref = 4;

    // b(k) = sgn{x(k) - y(k-1)}
    // Is the incoming signal higher than the current accumulator?
    bool b = false;

    // alpha = 1 if {bitref} bits in the last {bitref} output bits are equal, otherwise 0.
    bool alpha = false;

    // The predicted output so far for a sample
    // y(k)
    float accumulator = 0.0f;

    // Current step size (δ(k)) - start at minimum valid value
    // This ensures the step size is always within bounds from the first iteration
    float accumulatorStepSize = 10.0f;

    // Accumulator bounds (matches 16-bit PCM range)
    float minAccumulatorSize = -32768.0f;
    float maxAccumulatorSize = 32767.0f;

    // β (step size decay factor) - applied when bits are not homogenous
    double stepSizeDecay = 0.9990234375;

    // α (syllabic companding factor) - applied when last 4 bits are all 0s or all 1s
    double syllabicCompandingFactor = 1.0009765625;

    // Accumulator step size limits (δ_min and δ_max)
    int minAccumulatorStepSize = 10;
    int maxAccumulatorStepSize = 1280;

    // Bit history for syllabic companding (stores last 4 bits)
    u_int8_t bitHistory = 0;

};

#endif //AUDACITY_CVSD_H
