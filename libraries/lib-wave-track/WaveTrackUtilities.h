/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.h

  Paul Licameli

  @brief Various operations on WaveTrack, needing only its public interface

**********************************************************************/
#ifndef __AUDACITY_WAVE_TRACK_UTILITIES__
#define __AUDACITY_WAVE_TRACK_UTILITIES__

#include "Internat.h"
#include "TranslatableString.h"
#include <unordered_set>

class sampleCount;
class WaveTrack;
using ProgressReporter = std::function<void(double)>;

class SampleBlock;
class TrackList;

namespace WaveTrackUtilities {

using SampleBlockID = long long;
using SampleBlockIDSet = std::unordered_set<SampleBlockID>;
using BlockVisitor = std::function<void(SampleBlock&)>;
using BlockInspector = std::function<void(const SampleBlock&)>;

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

//! Expand cut line (that is, re-insert audio, then delete audio saved in
//! cut line)
/*
 @pre `track.IsLeader()`
 @param[out] cutlineStart start time of the insertion
 @param[out] cutlineEnd end time of the insertion
 */
WAVE_TRACK_API void ExpandCutLine(WaveTrack &track, double cutLinePosition,
   double* cutlineStart = nullptr, double* cutlineEnd = nullptr);

//! Whether any clips have hidden audio
/*!
 @pre `track.IsLeader()`
 */
WAVE_TRACK_API bool HasHiddenData(const WaveTrack &track);

//! Remove hidden audio from all clips
/*!
 @pre `track.IsLeader()`
 */
WAVE_TRACK_API void DiscardTrimmed(WaveTrack &track);

// Function to visit all sample blocks from a list of tracks.
// If a set is supplied, then only visit once each unique block ID not already
// in that set, and accumulate those into the set as a side-effect.
// The visitor function may be null.
WAVE_TRACK_API void VisitBlocks(TrackList &tracks, BlockVisitor visitor,
   SampleBlockIDSet *pIDs = nullptr);

// Non-mutating version of the above
WAVE_TRACK_API void InspectBlocks(const TrackList &tracks,
   BlockInspector inspector, SampleBlockIDSet *pIDs = nullptr);
}

#endif
