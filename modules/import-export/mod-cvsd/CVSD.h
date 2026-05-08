//
// Created by Sanskar Bajpai on 08/05/2026.
//

#ifndef AUDACITY_CVSD_H
#define AUDACITY_CVSD_H
#include <vector>
#include <cstdint>
#include <wxFileNameWrapper.h>

struct CVSD_CONFIG {
    // 5-seconds blocks of audio
    const size_t max_block_len = 44100 * 5;
    unsigned channels = 1;
    unsigned int num_bits = 1;
    unsigned char bitref = 4;
    unsigned char bitmask = 0; // Fixed garbage value

    float mAccumulator = 0.0f;
    float ref = 0.0f;
    float zeta = 0.0f;
    float delta = 0.0f;
    float delta_min = 0.0f;
    float delta_max = 0.0f;
    float step_size = 0.0f;

    float alpha = 0.0f;
    float beta = 0.0f;
};

// Extern means the actual 'config' variable lives in CVSD.cpp
extern CVSD_CONFIG config;

// Declarations of your functions
void CVSDEncode();
void CVSDDecode(std::unique_ptr<wxFFile> openedFile);

#endif //AUDACITY_CVSD_H
