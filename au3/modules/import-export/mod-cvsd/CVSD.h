// #ifndef AUDACITY_CVSD_H
// #define AUDACITY_CVSD_H
#include "au3-math/SampleFormat.h"

struct CVSD_CONFIG {
    // 16 bit config for CVSD
    unsigned int num_bits = 1;

    // J
    // unsigned int bitref = 4;

    // This value can either be 1 or 0.
    // 0 = -1, 1 = 1
    // b(k) = sgn{x(k)−ˆx(k−1)}
    // Is the incoming signal higher than the current set to true. If so, increase
    bool b = false;

    // ⍺ -> = 1 if {bitref} bits in the last {bitref} output bits are equal then true, otherwise false.
    bool alpha = false;

    // The total output so far for a sample
    // y(k)
    float accumulator = 0.0f;

    // y_min & y_max
    float accumulatorStepSize = 1.0f;
    float minAccumulatorSize = INT16_MIN;
    float maxAccumulatorSize = INT16_MAX;


    // Taken from https://www.gnuradio.org/doc/doxygen/classgr_1_1vocoder_1_1cvsd__decode__bs.html?__cf_chl_f_tk=sgZxzWwlkhsQJcfRONKl5AqE6SjLQc8vOLtXNGa27PU-1782994519-1.0.1.1-nRVjQ1gW.dyVPxNcMPPKzjgkPqx6duVcZdelbd27rE8#a19f056bc2da0301fd36aeb31148d21f3
    // β
    double stepSizeDecay =  0.9990234375;
    // h
    double accumulatorDecay = 96875;

    // α
    float syllabicCompandingFactor = 0.0f;

    // Accumulator step size (δ(k))
    int minAccumulatorStepSize = 10;
    int maxAccumulatorStepSize = 1280;

    u_int8_t bitHistory = 0;

};

// #endif //AUDACITY_CVSD_H