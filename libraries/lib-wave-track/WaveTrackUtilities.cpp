/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrackUtilities.cpp

  Paul Licameli

**********************************************************************/
#include "WaveTrackUtilities.h"
#include "BasicUI.h"
#include "Sequence.h"
#include "UserException.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include <algorithm>

const TranslatableString WaveTrackUtilities::defaultStretchRenderingTitle =
   XO("Pre-processing");

bool WaveTrackUtilities::HasStretch(
   const WaveTrack &track, double t0, double t1)
{
   auto &clips = track.GetClips();
   return any_of(clips.begin(), clips.end(),
      [&](auto &pClip){
         return pClip->IntersectsPlayRegion(t0, t1) &&
            pClip->GetStretchRatio() != 1.0;
      });
}

void WaveTrackUtilities::WithStretchRenderingProgress(
   std::function<void(const ProgressReporter&)> action,
   TranslatableString title, TranslatableString message)
{
   using namespace BasicUI;
   auto progress =
      MakeProgress(std::move(title), std::move(message), ProgressShowCancel);
   const auto reportProgress = [&](double progressFraction) {
      const auto result = progress->Poll(progressFraction * 1000, 1000);
      if (result != ProgressResult::Success)
         throw UserException {};
   };
   action(reportProgress);
}

namespace {
bool ReverseOneClip(WaveTrack &track,
   sampleCount start, sampleCount len,
   sampleCount originalStart, sampleCount originalEnd,
      const WaveTrackUtilities::ProgressReport &report)
{
   bool rc = true;
   // keep track of two blocks whose data we will swap
   auto first = start;

   auto blockSize = track.GetMaxBlockSize();
   const auto width = track.NChannels();
   Floats buffers0[2]{
      Floats(blockSize), width > 1 ? Floats(blockSize) : Floats{} };
   float *pointers0[2]{ buffers0[0].get(),
      width > 1 ? buffers0[1].get() : nullptr };
   Floats buffers1[2]{
      Floats(blockSize), width > 1 ? Floats(blockSize) : Floats{} };
   float *pointers1[2]{ buffers1[0].get(),
      width > 1 ? buffers1[1].get() : nullptr };
   constexpr auto reverseBuffers =
   [](float *const (&pointers)[2], size_t size){
      for (const auto pointer : pointers)
         if (pointer)
            std::reverse(pointer, pointer + size);
   };

   auto originalLen = originalEnd - originalStart;

   while (len > 1) {
      auto block =
         limitSampleBufferSize(track.GetBestBlockSize(first), len / 2);
      auto second = first + (len - block);

      track.GetFloats(0, width, pointers0, first, block);
      reverseBuffers(pointers0, block);
      track.GetFloats(0, width, pointers1, second, block);
      reverseBuffers(pointers1, block);
      // Don't dither on later rendering if only reversing samples
      const bool success =
         track.SetFloats(pointers1, first, block, narrowestSampleFormat)
         &&
         track.SetFloats(pointers0, second, block, narrowestSampleFormat);
      if (!success)
         return false;

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

bool WaveTrackUtilities::Reverse(WaveTrack &track,
   sampleCount start, sampleCount len, const ProgressReport &progress)
{
   bool rValue = true; // return value

   // start, end, len refer to the selected reverse region
   auto end = start + len;

   auto clipArray = track.SortedIntervalArray();
   const auto invariant = [&]{
      return std::is_sorted(clipArray.begin(), clipArray.end(),
         [](const auto &pA, const auto &pB){
            return pA->GetPlayStartTime() < pB->GetPlayEndTime();
         });
   };
   assert(invariant());

   // STEP 1:
   // If a reverse selection begins and/or ends at the inside of a clip
   // perform a split at the start and/or end of the reverse selection
   // Beware, the array grows as we loop over it, so don't use range-for
   for (size_t ii = 0; ii < clipArray.size(); ++ii) {
      const auto &clip = *clipArray[ii];
      auto clipStart = clip.GetPlayStartSample();
      auto clipEnd = clip.GetPlayEndSample();
      const auto splitAt = [&](double splitTime){
         auto [_, second] = track.SplitAt(splitTime);
         if (second)
            clipArray.insert(clipArray.begin() + ii + 1, second);
      };
      if (clipStart < start && clipEnd > start && clipEnd <= end) {
         // the reverse selection begins at the inside of a clip
         double splitTime = track.LongSamplesToTime(start);
         splitAt(splitTime);
      }
      else if (clipStart >= start && clipStart < end && clipEnd > end) {
         // the reverse selection ends at the inside of a clip
         double splitTime = track.LongSamplesToTime(end);
         splitAt(splitTime);
      }
      else if (clipStart < start && clipEnd > end) {
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
   for (const auto &clip : clipArray) {
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
               if (clipArray[i - 1]->GetPlayEndSample() <= start)
                  currentEnd -= (clipStart - start);
            }
            else
               currentEnd -= (clipStart - start);
         }

         auto revStart = std::max(clipStart, start);
         auto revEnd = std::min(end, clipEnd);
         auto revLen = revEnd - revStart;
         if (revEnd >= revStart) {
            // reverse the clip
            if (!ReverseOneClip(track, revStart, revLen, start, end, progress))
            {
               rValue = false;
               break;
            }

            // calculate the offset required
            auto clipOffsetStart = currentEnd - (clipEnd - clipStart);
            double offsetStartTime = track.LongSamplesToTime(clipOffsetStart);
            if (i + 1 < clipArray.size()) {
               // update currentEnd if there is a clip to process next
               auto nextClipStart = clipArray[i + 1]->GetPlayStartSample();
               currentEnd = currentEnd -
                  (clipEnd - clipStart) - (nextClipStart - clipEnd);
            }

            // detach the clip from track
            revClips.push_back(clip);
            track.RemoveInterval(clip);
            // align time to a sample and set offset
            revClips.back()->SetPlayStartTime(
               track.SnapToSample(offsetStartTime));
         }
      }
      else if (clipStart >= end) {
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
         it != revEnd; ++it)
      track.InsertInterval(*it);

   if (!rValue)
      return false;

   for (auto &clip : otherClips)
      track.InsertInterval(clip);

   return rValue;
}

sampleCount WaveTrackUtilities::GetSequenceSamplesCount(const WaveTrack &track)
{
   assert(track.IsLeader());
   sampleCount result{ 0 };
   for (const auto &&pInterval : track.Intervals())
      result += pInterval->GetSequenceSamplesCount();
   return result;
}

size_t WaveTrackUtilities::CountBlocks(const WaveTrack &track)
{
   assert(track.IsLeader());
   size_t result{};
   for (const auto &&pInterval : track.Intervals())
      result += pInterval->CountBlocks();
   return result;
}
