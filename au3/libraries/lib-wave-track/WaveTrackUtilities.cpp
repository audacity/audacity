/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.cpp

  Paul Licameli

**********************************************************************/
#include "WaveTrackUtilities.h"
#include "SampleBlock.h"
#include "Sequence.h"
#include "WaveClip.h"
#include <algorithm>

WaveTrackUtilities::AllClipsIterator::AllClipsIterator(WaveTrack& track)
    : mpTrack(&track)
{
    if (mpTrack) {
        auto&& clips = mpTrack->Intervals();
        Push({ clips.begin(), clips.end() });
    }
}

auto WaveTrackUtilities::AllClipsIterator::operator *() const -> value_type
{
    if (mStack.empty()) {
        return nullptr;
    } else {
        auto&[intervals, ii] = mStack.back();
        return intervals[ii];
    }
}

auto WaveTrackUtilities::AllClipsIterator::operator ++() -> AllClipsIterator
&
{
    // The unspecified sequence is a post-order, but there is no
    // promise whether sister nodes are ordered in time.
    if (mpTrack && !mStack.empty()) {
        auto&[intervals, ii] = mStack.back();
        if (++ii == intervals.size()) {
            mStack.pop_back();
        } else {
            Push(intervals[ii]->GetCutLines());
        }
    }

    return *this;
}

void WaveTrackUtilities::AllClipsIterator::Push(IntervalHolders clips)
{
    if (!mpTrack) {
        return;
    }

    // Go depth first while there are cutlines
    while (!clips.empty()) {
        auto nextClips = clips[0]->GetCutLines();
        mStack.push_back({ move(clips), 0 });
        clips = move(nextClips);
    }
}

namespace {
bool ReverseOneClip(WaveTrack& track,
                    sampleCount start, sampleCount len,
                    sampleCount originalStart, sampleCount originalEnd,
                    const WaveTrackUtilities::ProgressReport& report)
{
    bool rc = true;
    // keep track of two blocks whose data we will swap
    auto first = start;

    auto blockSize = track.GetMaxBlockSize();
    const auto width = track.NChannels();
    Floats buffers0[2]{
        Floats(blockSize), width > 1 ? Floats(blockSize) : Floats{} };
    float* pointers0[2]{ buffers0[0].get(),
                         width > 1 ? buffers0[1].get() : nullptr };
    Floats buffers1[2]{
        Floats(blockSize), width > 1 ? Floats(blockSize) : Floats{} };
    float* pointers1[2]{ buffers1[0].get(),
                         width > 1 ? buffers1[1].get() : nullptr };
    constexpr auto reverseBuffers
        =[](float* const (&pointers)[2], size_t size){
        for (const auto pointer : pointers) {
            if (pointer) {
                std::reverse(pointer, pointer + size);
            }
        }
    };

    auto originalLen = originalEnd - originalStart;

    while (len > 1) {
        auto block
            =limitSampleBufferSize(track.GetBestBlockSize(first), len / 2);
        auto second = first + (len - block);

        track.GetFloats(0, width, pointers0, first, block);
        reverseBuffers(pointers0, block);
        track.GetFloats(0, width, pointers1, second, block);
        reverseBuffers(pointers1, block);
        // Don't dither on later rendering if only reversing samples
        const bool success
            =track.SetFloats(pointers1, first, block, narrowestSampleFormat)
              &&
              track.SetFloats(pointers0, second, block, narrowestSampleFormat);
        if (!success) {
            return false;
        }

        len -= 2 * block;
        first += block;

        if (!report(
                2 * (first - originalStart).as_double() / originalLen.as_double()
                )) {
            rc = false;
            break;
        }
    }

    return rc;
}
}

bool WaveTrackUtilities::Reverse(WaveTrack& track,
                                 sampleCount start, sampleCount len, const ProgressReport& progress)
{
    bool rValue = true; // return value

    // start, end, len refer to the selected reverse region
    auto end = start + len;

    auto clipArray = track.SortedIntervalArray();
    const auto invariant = [&]{
        return std::is_sorted(clipArray.begin(), clipArray.end(),
                              [](const auto& pA, const auto& pB){
            return pA->GetPlayStartTime() < pB->GetPlayEndTime();
        });
    };
    assert(invariant());

    // STEP 1:
    // If a reverse selection begins and/or ends at the inside of a clip
    // perform a split at the start and/or end of the reverse selection
    // Beware, the array grows as we loop over it, so don't use range-for
    for (size_t ii = 0; ii < clipArray.size(); ++ii) {
        const auto& clip = *clipArray[ii];
        auto clipStart = clip.GetPlayStartSample();
        auto clipEnd = clip.GetPlayEndSample();
        const auto splitAt = [&](double splitTime){
            auto [_, second] = track.SplitAt(splitTime);
            if (second) {
                clipArray.insert(clipArray.begin() + ii + 1, second);
            }
        };
        if (clipStart < start && clipEnd > start && clipEnd <= end) {
            // the reverse selection begins at the inside of a clip
            double splitTime = track.LongSamplesToTime(start);
            splitAt(splitTime);
        } else if (clipStart >= start && clipStart < end && clipEnd > end) {
            // the reverse selection ends at the inside of a clip
            double splitTime = track.LongSamplesToTime(end);
            splitAt(splitTime);
        } else if (clipStart < start && clipEnd > end) {
            // the selection begins AND ends at the inside of a clip
            double splitTime = track.LongSamplesToTime(end);
            splitAt(splitTime);
            splitTime = track.LongSamplesToTime(start);
            splitAt(splitTime);
        }
        assert(invariant());
    }

    //STEP 2:
    // Individually reverse each clip inside the selected region
    // and apply the appropriate offset after detaching them from the track

    bool checkedFirstClip = false;

    // used in calculating the offset of clips to rearrange
    // holds the new end position of the current clip
    auto currentEnd = end;

    // holds the reversed clips
    using IntervalHolders = WaveTrack::IntervalHolders;
    IntervalHolders revClips;
    // holds the clips that appear after the reverse selection region
    IntervalHolders otherClips;
    // Unlike in the previous iteration, clipArray is not inserted or
    // erased
    size_t i = 0;
    for (const auto& clip : clipArray) {
        Finally Do([&]{ ++i; });
        auto clipStart = clip->GetPlayStartSample();
        auto clipEnd = clip->GetPlayEndSample();

        if (clipStart >= start && clipEnd <= end) {
            // if the clip is inside the selected region
            // this is used to check if the selected region begins with a
            // whitespace.  If yes then clipStart (of the first clip) and start are
            // not the same.  Adjust currentEnd accordingly and set endMerge to
            // false
            if (!checkedFirstClip && clipStart > start) {
                checkedFirstClip = true;
                if (i > 0) {
                    if (clipArray[i - 1]->GetPlayEndSample() <= start) {
                        currentEnd -= (clipStart - start);
                    }
                } else {
                    currentEnd -= (clipStart - start);
                }
            }

            auto revStart = std::max(clipStart, start);
            auto revEnd = std::min(end, clipEnd);
            auto revLen = revEnd - revStart;
            if (revEnd >= revStart) {
                // reverse the clip
                if (!ReverseOneClip(track, revStart, revLen, start, end, progress)) {
                    rValue = false;
                    break;
                }

                // calculate the offset required
                auto clipOffsetStart = currentEnd - (clipEnd - clipStart);
                double offsetStartTime = track.LongSamplesToTime(clipOffsetStart);
                if (i + 1 < clipArray.size()) {
                    // update currentEnd if there is a clip to process next
                    auto nextClipStart = clipArray[i + 1]->GetPlayStartSample();
                    currentEnd = currentEnd
                                 - (clipEnd - clipStart) - (nextClipStart - clipEnd);
                }

                // detach the clip from track
                revClips.push_back(clip);
                track.RemoveInterval(clip);
                // align time to a sample and set offset
                revClips.back()->SetPlayStartTime(
                    track.SnapToSample(offsetStartTime));
            }
        } else if (clipStart >= end) {
            // clip is after the selection region
            // simply remove and append to otherClips
            otherClips.push_back(clip);
            track.RemoveInterval(clip);
        }
    }

    // STEP 3: Append the clips from
    // revClips and otherClips back to the track
    // the last clip of revClips is appended to the track first
    // PRL:  I don't think that matters, the sequence of storage of clips in the
    // track is not elsewhere assumed to be by time
    for (auto it = revClips.rbegin(), revEnd = revClips.rend();
         it != revEnd; ++it) {
        track.InsertInterval(*it, false);
    }

    if (!rValue) {
        return false;
    }

    for (auto& clip : otherClips) {
        track.InsertInterval(clip, false);
    }

    return rValue;
}

sampleCount WaveTrackUtilities::GetSequenceSamplesCount(const WaveTrack& track)
{
    sampleCount result{ 0 };
    for (const auto& pInterval : track.Intervals()) {
        result += pInterval->GetSequenceSamplesCount();
    }
    return result;
}

size_t WaveTrackUtilities::CountBlocks(const WaveTrack& track)
{
    size_t result{};
    for (const auto& pInterval : track.Intervals()) {
        result += pInterval->CountBlocks();
    }
    return result;
}

void WaveTrackUtilities::CloseLock(WaveTrack& track) noexcept
{
    for (const auto& pClip : track.Intervals()) {
        pClip->CloseLock();
    }
}

bool WaveTrackUtilities::RemoveCutLine(WaveTrack& track, double cutLinePosition)
{
    bool removed = false;
    for (const auto& pClip : track.Intervals()) {
        if (pClip->RemoveCutLine(cutLinePosition)) {
            removed = true;
            break;
        }
    }
    return removed;
}

// Expand cut line (that is, re-insert audio, then DELETE audio saved in cut line)
// Can't yet promise strong exception safety for a pair of channels together
void WaveTrackUtilities::ExpandCutLine(WaveTrack& track,
                                       double cutLinePosition, double* cutlineStart,
                                       double* cutlineEnd)
{
    const bool editClipCanMove = GetEditClipsCanMove();

    // Find clip which contains this cut line
    double start = 0, end = 0;
    const auto& clips = track.Intervals();
    const auto pEnd = clips.end();
    const auto pClip = std::find_if(clips.begin(), pEnd,
                                    [&](const auto& clip) {
        return clip->FindCutLine(cutLinePosition, &start, &end);
    });
    if (pClip != pEnd) {
        auto&& clip = *pClip;
        if (!editClipCanMove) {
            // We are not allowed to move the other clips, so see if there
            // is enough room to expand the cut line
            for (const auto& clip2: clips) {
                if (clip2->GetPlayStartTime() > clip->GetPlayStartTime()
                    && clip->GetPlayEndTime() + end - start > clip2->GetPlayStartTime()) {
                    // Strong-guarantee in case of this path
                    throw SimpleMessageBoxException{
                              ExceptionType::BadUserAction,
                              XO("There is not enough room available to expand the cut line"),
                              XO("Warning"),
                              "Error:_Insufficient_space_in_track"
                    };
                }
            }
        }

        clip->ExpandCutLine(cutLinePosition);

        // Strong-guarantee provided that the following gives No-fail-guarantee

        if (cutlineStart) {
            *cutlineStart = start;
        }
        if (cutlineEnd) {
            *cutlineEnd = end;
        }

        // Move clips which are to the right of the cut line
        if (editClipCanMove) {
            for (const auto& clip2 : clips) {
                if (clip2->GetPlayStartTime() > clip->GetPlayStartTime()) {
                    clip2->ShiftBy(end - start);
                }
            }
        }
    }
}

bool WaveTrackUtilities::HasHiddenData(const WaveTrack& track)
{
    const auto& clips = track.Intervals();
    return std::any_of(clips.begin(), clips.end(), [](const auto& pClip){
        return pClip->GetTrimLeft() != 0 || pClip->GetTrimRight() != 0;
    });
}

void WaveTrackUtilities::DiscardTrimmed(WaveTrack& track)
{
    for (const auto& pClip : track.Intervals()) {
        if (pClip->GetTrimLeft() != 0) {
            auto t0 = pClip->GetPlayStartTime();
            pClip->SetTrimLeft(0);
            pClip->ClearLeft(t0);
        }
        if (pClip->GetTrimRight() != 0) {
            auto t1 = pClip->GetPlayEndTime();
            pClip->SetTrimRight(0);
            pClip->ClearRight(t1);
        }
    }
}

void WaveTrackUtilities::VisitBlocks(TrackList& tracks, BlockVisitor visitor,
                                     SampleBlockIDSet* pIDs)
{
    for (auto wt : tracks.Any<WaveTrack>()) {
        // Scan all clips within current track
        for (const auto& pClip : GetAllClips(*wt)) {
            // Scan all sample blocks within current clip
            for (const auto& pChannel : pClip->Channels()) {
                auto blocks = pChannel->GetSequenceBlockArray();
                for (const auto& block : *blocks) {
                    auto& pBlock = block.sb;
                    if (pBlock) {
                        if (pIDs && !pIDs->insert(pBlock->GetBlockID()).second) {
                            continue;
                        }
                        if (visitor) {
                            visitor(pBlock);
                        }
                    }
                }
            }
        }
    }
}

void WaveTrackUtilities::InspectBlocks(const TrackList& tracks,
                                       BlockInspector inspector, SampleBlockIDSet* pIDs)
{
    VisitBlocks(const_cast<TrackList&>(tracks), move(inspector), pIDs);
}

void WaveTrackUtilities::ExpandClipTillNextOne(
    const WaveTrack& track, WaveTrack::Interval& interval)
{
    if (
        const auto nextClip
            =track.GetNextInterval(interval, PlaybackDirection::forward)) {
        interval.StretchRightTo(nextClip->GetPlayStartTime());
    }
}
