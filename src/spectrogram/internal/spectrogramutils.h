/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramconfiguration.h"
#include "spectrogramtypes.h"

#include <vector>

namespace au::spectrogram {
int viewportWidth(const ViewInfo&);

double positionToTime(const ViewInfo&, int position);

long long timeToPosition(const ViewInfo&, double projectTime);

void findCorrection(
    const std::vector<long long>& oldWhere, size_t oldLen, size_t newLen, double t0, double sampleRate, double stretchRatio,
    double samplesPerPixel, int& oldX0, double& correction);

void fillWhere(
    std::vector<long long>& where, size_t len, bool addBias, double correction, double t0, double sampleRate, double stretchRatio,
    double samplesPerPixel);

int fftLength(const ISpectrogramConfiguration& config);

std::pair<float, float> spectrogramBounds(const ISpectrogramConfiguration&, double sampleRate);
}
