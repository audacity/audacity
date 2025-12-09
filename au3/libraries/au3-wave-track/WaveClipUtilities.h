/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveClipUtilities.h

  Paul Licameli

  @brief Various operations on WaveClip, needing only its public interface

**********************************************************************/
#ifndef __AUDACITY_WAVE_CLIP_UTILITIES__
#define __AUDACITY_WAVE_CLIP_UTILITIES__

enum class sampleFormat : unsigned;

#include "WaveTrack.h"
#include <cstddef>

namespace WaveClipUtilities {
/*!
 @param t absolute time
 */
WAVE_TRACK_API bool GetFloatAtTime(const WaveClip& clip, double t, size_t iChannel, float& value, bool mayThrow);

//! Succeed with out-of-bounds requests, only changing what is in bounds.
//! @{
// clang-format off
/*!
 @brief Considers `buffer` as audio starting at `TimeToSamples(t)`
 (relative to clip play start time) and with equal stretch ratio. Samples
 at intersecting indices are then copied, leaving non-intersecting clip
 samples untouched. E.g.,
     buffer:      [a b c d e]
     clip  :            [x y z]
     result:            [d e z]
 */
// clang-format on
WAVE_TRACK_API void SetFloatsFromTime(WaveClip& clip, double t, size_t iChannel, const float* buffer, size_t numSamples,
                                      sampleFormat effectiveFormat);

//! used by commands which interact with clips using the keyboard
/*!
 When two clips are immediately next to each other, the GetPlayEndTime()
 of the first clip and the GetPlayStartTime() of the second clip may not
 be exactly equal due to rounding errors.
 */
WAVE_TRACK_API bool SharesBoundaryWithNextClip(
    const WaveTrack::Interval& prev, const WaveTrack::Interval& next);
}

#endif
