/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include <cmath>

namespace au::spectrogram {
int viewportWidth(const ViewInfo&);
double positionToTime(const ViewInfo&, int position);
int timeToPosition(const ViewInfo&, double projectTime);
}
