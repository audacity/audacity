/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include <vector>

namespace au::spectrogram {
int viewportWidth(const ViewInfo&);

double positionToTime(const ViewInfo&, int position);

int timeToPosition(const ViewInfo&, double projectTime);

void findCorrection(
    const std::vector<long long>& oldWhere, size_t oldLen, size_t newLen, double t0, double sampleRate, double stretchRatio,
    double samplesPerPixel, int& oldX0, double& correction);

void fillWhere(
    std::vector<long long>& where, size_t len, bool addBias, double correction, double t0, double sampleRate, double stretchRatio,
    double samplesPerPixel);
}
