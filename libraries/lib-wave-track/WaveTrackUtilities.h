/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.h

  Paul Licameli

  @brief Various operations on WaveTrack, needing only its public interface

**********************************************************************/

#include "Internat.h"
#include "TranslatableString.h"

class sampleCount;
class WaveTrack;
using ProgressReporter = std::function<void(double)>;

namespace WaveTrackUtilities {

//! Whether any clips, whose play regions intersect the interval, have non-unit
//! stretch ratio
WAVE_TRACK_API
bool HasStretch(const WaveTrack &track, double t0, double t1);

extern WAVE_TRACK_API const TranslatableString defaultStretchRenderingTitle;

WAVE_TRACK_API void WithStretchRenderingProgress(
   std::function<void(const ProgressReporter&)> action,
   TranslatableString title = defaultStretchRenderingTitle,
   TranslatableString message = XO("Rendering Time-Stretched Audio"));

//! Argument is in (0, 1)
//! @return true if processing should continue
using ProgressReport = std::function<bool(double)>;

WAVE_TRACK_API bool Reverse(WaveTrack &track,
   sampleCount start, sampleCount len, const ProgressReport &report = {});

/*!
 @return the total number of samples in all underlying sequences
of all clips, across all channels (including hidden audio but not
counting the cutlines)

 @pre `track.IsLeader()`
 */
WAVE_TRACK_API sampleCount GetSequenceSamplesCount(const WaveTrack &track);

/*!
 @return the total number of blocks in all underlying sequences of all clips,
across all channels (including hidden audio but not counting the cutlines)

 @pre `track.IsLeader()`
 */
WAVE_TRACK_API size_t CountBlocks(const WaveTrack &track);

//! Should be called upon project close.  Not balanced by unlocking calls.
/*!
 @pre `track.IsLeader()`
 @excsafety{No-fail}
 */
WAVE_TRACK_API void CloseLock(WaveTrack &track) noexcept;

//! Remove cut line, without expanding the audio in it
/*
 @pre `track.IsLeader()`
 @return whether any cutline existed at the position and was removed
 */
WAVE_TRACK_API bool RemoveCutLine(WaveTrack &track, double cutLinePosition);
}
