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

#endif //AUDACITY_CVSD_H
