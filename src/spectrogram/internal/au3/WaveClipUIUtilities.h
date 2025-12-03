/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstddef>
#include <vector>

#include "WaveTrack.h"

class sampleCount;

namespace WaveClipUIUtilities {
void findCorrection(
    const std::vector<sampleCount>& oldWhere, size_t oldLen, size_t newLen, double t0, double sampleRate, double stretchRatio,
    double samplesPerPixel, int& oldX0, double& correction);

void fillWhere(
    std::vector<sampleCount>& where, size_t len, bool addBias, double correction, double t0, double sampleRate, double stretchRatio,
    double samplesPerPixel);
} // namespace WaveClipUIUtilities
