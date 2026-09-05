#ifndef AUDACITY_CVSD_H
#define AUDACITY_CVSD_H

#include "au3-math/SampleFormat.h"

#define DELTA_MIN           (int32_t)(0.0001 * 32768)//(0.0002 * 32768)
#define DELTA_MAX           (int32_t)(0.0078 * 32768)//(0.0078 * 32768)
#define SYLLABIC_CONST      (int32_t)(0.9845 * 32768)//(0.9845 * 32768)
#define PRM_INTEG_CONST     (int32_t)(0.9394 * 32768)//(0.9394 * 32768)
#define INTEG_B1            (int32_t)(1.2708 * 32768)//(1.2708 * 32768)
#define INTEG_B2            (int32_t)(0.3202 * 32768)//(0.3202 * 32768)
#define INTEG_G2D           (int32_t)(1.5092 * 32768)//(1.5092 * 32768)

typedef struct
{
    int16_t prev1;
    int16_t prev2;
    int32_t step;
    int32_t product;
    int16_t In_current;
    int16_t Out_current;
    int32_t bit_accum;
    int16_t dec_step;
    int16_t dec_prev1;
    int16_t dec_prev2;
} T_CVSD_MAIN_STRUCT;

// Preserve bit packing for the :: Process function
struct CVSD_BITPACKER {
    u_int8_t partialByte = 0;
    int partialBits = 0;
};

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

    float accumulatorDecay = 0.96875f;

    // Accumulator bounds (matches 16-bit PCM range)
    float minAccumulatorSize = -32768.0f;
    float maxAccumulatorSize = 32767.0f;

    // β (step size decay factor) - applied when bits are not homogenous
    double stepSizeDecay = 0.9990234375;

    // α (syllabic companding factor) - applied when last 4 bits are all 0s or all 1s
    double syllabicCompandingFactor = 1.0009765625;

    // Accumulator step size limits (δ_min and δ_max)
    float minAccumulatorStepSize = 10.0f;
    float maxAccumulatorStepSize = 1280.0f;

    // Bit history for syllabic companding (stores last 4 bits)
    u_int8_t bitHistory = 0;

};

#endif //AUDACITY_CVSD_H
