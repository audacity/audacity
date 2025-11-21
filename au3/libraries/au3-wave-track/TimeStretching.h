/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeStretching.h

  Paul Licameli split from WaveTrackUtilities.h

  @brief Various operations on WaveTrack, related to time stretching

**********************************************************************/
#ifndef __AUDACITY_TIME_STRETCHING__
#define __AUDACITY_TIME_STRETCHING__

#include "Internat.h"
#include "IteratorX.h"
#include "TranslatableString.h"
#include "WaveTrack.h"
#include <unordered_set>

class WaveTrack;
using ProgressReporter = std::function<void (double)>;

namespace TimeStretching {
//! Whether any clips, whose play regions intersect the interval, have non-unit
//! stretch ratio
WAVE_TRACK_API
bool HasPitchOrSpeed(const WaveTrack& track, double t0, double t1);

extern WAVE_TRACK_API const TranslatableString defaultStretchRenderingTitle;

// Calls UserException::WithCancellableProgress supplying title and caption
WAVE_TRACK_API void WithClipRenderingProgress(
    std::function<void(const ProgressReporter&)> action, TranslatableString title = defaultStretchRenderingTitle);

WAVE_TRACK_API bool SetClipStretchRatio(
    const WaveTrack& track, WaveTrack::Interval& interval, double stretchRatio);
}

#endif
