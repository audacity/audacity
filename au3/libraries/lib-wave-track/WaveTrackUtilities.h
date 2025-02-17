/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.h

  Paul Licameli

  @brief Various operations on WaveTrack, needing only its public interface

**********************************************************************/
#ifndef __AUDACITY_WAVE_TRACK_UTILITIES__
#define __AUDACITY_WAVE_TRACK_UTILITIES__

#include "IteratorX.h"
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

//! Get mutative access to all clips (in some unspecified sequence),
//! including those hidden in cutlines.
/*!
 If clips are added to the track during the visit, not all may be visited.
 If a clip is removed from the track during the visit, there will not be
 dangling pointers, but a clip not in the track may be visited.
 */
class WAVE_TRACK_API AllClipsIterator : public ValueIterator<IntervalHolder>
{
public:
    // Constructs an "end" iterator
    AllClipsIterator() {}

    // Construct a "begin" iterator
    explicit AllClipsIterator(WaveTrack& track);

    value_type operator *() const;

    AllClipsIterator& operator ++();

    //! Define == well enough to serve for loop termination test
    friend bool operator ==(
        const AllClipsIterator& a, const AllClipsIterator& b)
    { return a.mStack.empty() == b.mStack.empty(); }

    friend bool operator !=(
        const AllClipsIterator& a, const AllClipsIterator& b)
    { return !(a == b); }

private:
    using Stack = std::vector<std::pair<IntervalHolders, size_t> >;
    void Push(IntervalHolders clips);

    WaveTrack* mpTrack{};
    Stack mStack;
};

//! Get const access to all clips (in some unspecified sequence),
//! including those hidden in cutlines.
/*!
 @copydoc AllClipsIterator
 */
class WAVE_TRACK_API AllClipsConstIterator : public ValueIterator<IntervalConstHolder>
{
public:
    // Constructs an "end" iterator
    AllClipsConstIterator() {}

    // Construct a "begin" iterator
    explicit AllClipsConstIterator(const WaveTrack& track)
        : mIter{const_cast<WaveTrack&>(track)}
    {}

    value_type operator *() const { return *mIter; }

    AllClipsConstIterator& operator ++()
    { ++mIter; return *this; }

    //! Define == well enough to serve for loop termination test
    friend bool operator ==(
        const AllClipsConstIterator& a, const AllClipsConstIterator& b)
    { return a.mIter == b.mIter; }

    friend bool operator !=(
        const AllClipsConstIterator& a, const AllClipsConstIterator& b)
    { return !(a == b); }

private:
    AllClipsIterator mIter;
};

inline IteratorRange<AllClipsIterator> GetAllClips(WaveTrack& track)
{
    return { AllClipsIterator{ track }, AllClipsIterator{} };
}

inline IteratorRange<AllClipsConstIterator> GetAllClips(const WaveTrack& track)
{
    return { AllClipsConstIterator{ track }, AllClipsConstIterator{} };
}

//! Argument is in (0, 1)
//! @return true if processing should continue
using ProgressReport = std::function<bool (double)>;

WAVE_TRACK_API bool Reverse(WaveTrack& track, sampleCount start, sampleCount len, const ProgressReport& report = {});

/*!
 @return the total number of samples in all underlying sequences
of all clips, across all channels (including hidden audio but not
counting the cutlines)
 */
WAVE_TRACK_API sampleCount GetSequenceSamplesCount(const WaveTrack& track);

/*!
 @return the total number of blocks in all underlying sequences of all clips,
across all channels (including hidden audio but not counting the cutlines)
 */
WAVE_TRACK_API size_t CountBlocks(const WaveTrack& track);

//! Should be called upon project close.  Not balanced by unlocking calls.
/*!
 @excsafety{No-fail}
 */
WAVE_TRACK_API void CloseLock(WaveTrack& track) noexcept;

//! Remove cut line, without expanding the audio in it
/*
 @return whether any cutline existed at the position and was removed
 */
WAVE_TRACK_API bool RemoveCutLine(WaveTrack& track, double cutLinePosition);

//! Expand cut line (that is, re-insert audio, then delete audio saved in
//! cut line)
/*
 @param[out] cutlineStart start time of the insertion
 @param[out] cutlineEnd end time of the insertion
 */
WAVE_TRACK_API void ExpandCutLine(WaveTrack& track, double cutLinePosition, double* cutlineStart = nullptr, double* cutlineEnd = nullptr);

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
} // namespace WaveTrackUtilities

#endif
