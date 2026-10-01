/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.h

  Paul Licameli

  @brief Various operations on WaveTrack, needing only its public interface

**********************************************************************/
#ifndef __AUDACITY_WAVE_TRACK_UTILITIES__
#define __AUDACITY_WAVE_TRACK_UTILITIES__

#include "au3-utility/IteratorX.h"
#include "WaveTrack.h"
#include <unordered_set>

class SampleBlock;
class sampleCount;
class TrackList;
class WaveTrack;

#include <functional>
using ProgressReporter = std::function<void (double)>;

class SampleBlock;
class TrackList;

namespace WaveTrackUtilities {
using SampleBlockID = long long;
using SampleBlockIDSet = std::unordered_set<SampleBlockID>;
using BlockVisitor
    =std::function<void (const std::shared_ptr<SampleBlock>&)>;
using BlockInspector
    =std::function<void (std::shared_ptr<const SampleBlock>)>;

using IntervalHolder = std::shared_ptr<WaveTrack::Interval>;
using IntervalHolders = std::vector<IntervalHolder>;
using IntervalConstHolder = std::shared_ptr<const WaveTrack::Interval>;

//! Argument is in (0, 1)
//! @return true if processing should continue
using ProgressReport = std::function<bool (double)>;

WAVE_TRACK_API bool Reverse(WaveTrack& track, sampleCount start, sampleCount len, const ProgressReport& report = {});

/*!
 @return the total number of samples in all underlying sequences
of all clips, across all channels (including hidden audio)
 */
WAVE_TRACK_API sampleCount GetSequenceSamplesCount(const WaveTrack& track);

/*!
 @return the total number of blocks in all underlying sequences of all clips,
across all channels (including hidden audio)
 */
WAVE_TRACK_API size_t CountBlocks(const WaveTrack& track);

//! Should be called upon project close.  Not balanced by unlocking calls.
/*!
 @excsafety{No-fail}
 */
WAVE_TRACK_API void CloseLock(WaveTrack& track) noexcept;

//! Whether any clips have hidden audio
WAVE_TRACK_API bool HasHiddenData(const WaveTrack& track);

//! Remove hidden audio from all clips
WAVE_TRACK_API void DiscardTrimmed(WaveTrack& track);

// Function to visit all sample blocks from a list of tracks.
// If a set is supplied, then only visit once each unique block ID not already
// in that set, and accumulate those into the set as a side-effect.
// The visitor function may be null.
WAVE_TRACK_API void VisitBlocks(TrackList& tracks, BlockVisitor visitor, SampleBlockIDSet* pIDs = nullptr);

// Non-mutating version of the above
WAVE_TRACK_API void InspectBlocks(const TrackList& tracks, BlockInspector inspector, SampleBlockIDSet* pIDs = nullptr);

WAVE_TRACK_API void
ExpandClipTillNextOne(const WaveTrack& track, WaveTrack::Interval& interval);

//! Resolve overlapping clip play regions so that no two clips overlap.
/*!
 Clips are visited in play-start order; whenever a clip's play region overlaps
 its predecessor, the earlier clip yields its overlapping tail - its right edge
 is trimmed back to the later clip's start, or it is removed if fully shadowed.
 Sequence/hidden (trimmed-away) extents are not considered. Tolerant of
 sub-sample rounding.
 @post `track.NoPlayRegionsOverlap()`
 */
WAVE_TRACK_API void RemoveOverlaps(WaveTrack& track);

//! Ref: the copy keeps using the realtime effect states of the original,
//! for a copy that replaces the original or is temporary.
//! Deep: the copy gets its own states, for a copy that becomes a new track.
enum class RealtimeEffectsCopy {
    Ref, Deep
};

//! Make another track copying format, rate, etc. but containing no
//! clips; with the specified number of channels.
/*!
 It is important to pass the correct factory (that for the project
 which will own the copy) in the unusual case that a track is copied from
 another project or the clipboard.  For copies within one project, the
 default will do.
 */
WAVE_TRACK_API WaveTrack::Holder EmptyCopy(const WaveTrack& track, size_t nChannels, RealtimeEffectsCopy effects,
                                           const SampleBlockFactoryPtr& pFactory = {});

//! As above, with as many channels as in `track`
WAVE_TRACK_API WaveTrack::Holder EmptyCopy(const WaveTrack& track, RealtimeEffectsCopy effects,
                                           const SampleBlockFactoryPtr& pFactory = {});
} // namespace WaveTrackUtilities

#endif
