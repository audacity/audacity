/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.cpp

  Mark Phillips

*******************************************************************//**

\class EffectReverse
\brief An Effect that reverses the selected audio.

*//********************************************************************/



#include "Reverse.h"
#include "EffectOutputTracks.h"
#include "LoadEffects.h"

#include <algorithm>
#include <math.h>

#include "../LabelTrack.h"
#include "SyncLock.h"
#include "WaveClip.h"
#include "WaveTrack.h"

//
// EffectReverse
//

const ComponentInterfaceSymbol EffectReverse::Symbol
{ XO("Reverse") };

namespace{ BuiltinEffectsModule::Registration< EffectReverse > reg; }

EffectReverse::EffectReverse()
{
}

EffectReverse::~EffectReverse()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectReverse::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectReverse::GetDescription() const
{
   return XO("Reverses the selected audio");
}

// EffectDefinitionInterface implementation

EffectType EffectReverse::GetType() const
{
   return EffectTypeProcess;
}

bool EffectReverse::IsInteractive() const
{
   return false;
}

// Effect implementation

bool EffectReverse::Process(EffectInstance &, EffectSettings &)
{
   //all needed because Reverse should move the labels too
   EffectOutputTracks outputs{ *mTracks, true };
   bool bGoodResult = true;
   int count = 0;

   auto trackRange =
      outputs.Get().Leaders() + &SyncLock::IsSelectedOrSyncLockSelected;
   trackRange.VisitWhile(bGoodResult,
      [&](WaveTrack &track) {
         const auto progress =
            [&](double fraction){ return !TrackProgress(count, fraction); };
         if (mT1 > mT0) {
            auto start = track.TimeToLongSamples(mT0);
            auto end = track.TimeToLongSamples(mT1);
            auto len = end - start;

            if (!track.Reverse(start, len, progress))
               bGoodResult = false;
         }
         count += track.NChannels();
      },
      [&](LabelTrack &track) {
         track.ChangeLabelsOnReverse(mT0, mT1);
         count++;
      }
   );

   if (bGoodResult)
      outputs.Commit();

   return bGoodResult;
}

bool WaveTrack::Reverse(sampleCount start, sampleCount len,
   const ProgressReport &progress)
{
   size_t count = 0;
   const auto range = TrackList::Channels(this);
   const auto myProgress = [&](double fraction){
      return progress((count + fraction) / range.size());
   };
   for (const auto pChannel : range) {
      if (!ReverseOne(*pChannel, start, len, myProgress))
         return false;
      ++count;
   }
   return true;
}

bool WaveTrack::ReverseOne(WaveTrack &track,
   sampleCount start, sampleCount len,
   const ProgressReport &progress)
{
   bool rValue = true; // return value

   // start, end, len refer to the selected reverse region
   auto end = start + len;

   // STEP 1:
   // If a reverse selection begins and/or ends at the inside of a clip
   // perform a split at the start and/or end of the reverse selection
   const auto &clips = track.GetClips();
   // Beware, the array grows as we loop over it.  Use integer subscripts, not
   // iterators.
   for (size_t ii = 0; ii < clips.size(); ++ii) {
      const auto &clip = clips[ii].get();
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();
      if (clipStart < start && clipEnd > start && clipEnd <= end) {
         // the reverse selection begins at the inside of a clip
         double splitTime = track.LongSamplesToTime(start);
         track.SplitAt(splitTime);
      }
      else if (clipStart >= start && clipStart < end && clipEnd > end) {
         // the reverse selection ends at the inside of a clip
         double splitTime = track.LongSamplesToTime(end);
         track.SplitAt(splitTime);
      }
      else if (clipStart < start && clipEnd > end) {
         // the selection begins AND ends at the inside of a clip
         double splitTime = track.LongSamplesToTime(start);
         track.SplitAt(splitTime);
         splitTime = track.LongSamplesToTime(end);
         track.SplitAt(splitTime);
      }
   }

   //STEP 2:
   // Individually reverse each clip inside the selected region
   // and apply the appropriate offset after detaching them from the track

   bool checkedFirstClip = false;

   // used in calculating the offset of clips to rearrange
   // holds the new end position of the current clip
   auto currentEnd = end;

   // holds the reversed clips
   WaveClipHolders revClips;
   // holds the clips that appear after the reverse selection region
   WaveClipHolders otherClips;
   auto clipArray = track.SortedClipArray();
   for (size_t i = 0; i < clipArray.size(); ++i) {
      WaveClip *clip = clipArray[i];
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
            revClips.push_back(track.RemoveAndReturnClip(clip));
            // align time to a sample and set offset
            revClips.back()->SetPlayStartTime(
               track.LongSamplesToTime(
                  track.TimeToLongSamples(offsetStartTime)));
         }
      }
      else if (clipStart >= end) {
         // clip is after the selection region
         // simply remove and append to otherClips
         otherClips.push_back(track.RemoveAndReturnClip(clip));
      }
   }

   // STEP 3: Append the clips from
   // revClips and otherClips back to the track
   // the last clip of revClips is appended to the track first
   // PRL:  I don't think that matters, the sequence of storage of clips in the
   // track is not elsewhere assumed to be by time
   for (auto it = revClips.rbegin(), revEnd = revClips.rend();
        rValue && it != revEnd; ++it)
      rValue = track.AddClip(*it);

   if (!rValue)
      return false;

   for (auto &clip : otherClips)
      if (!(rValue = track.AddClip(clip)))
          break;

   return rValue;
}

bool WaveTrack::ReverseOneClip(WaveTrack &track,
   sampleCount start, sampleCount len,
   sampleCount originalStart, sampleCount originalEnd,
   const ProgressReport &report)
{
   bool rc = true;
   // keep track of two blocks whose data we will swap
   auto first = start;

   auto blockSize = track.GetMaxBlockSize();
   Floats buffer1{ blockSize };
   const auto pBuffer1 = buffer1.get();
   Floats buffer2{ blockSize };
   const auto pBuffer2 = buffer2.get();

   auto originalLen = originalEnd - originalStart;

   while (len > 1) {
      auto block =
         limitSampleBufferSize(track.GetBestBlockSize(first), len / 2);
      auto second = first + (len - block);

      track.GetFloats(buffer1.get(), first, block);
      std::reverse(pBuffer1, pBuffer1 + block);
      track.GetFloats(buffer2.get(), second, block);
      std::reverse(pBuffer2, pBuffer2 + block);
      // Don't dither on later rendering if only reversing samples
      track.Set((samplePtr)buffer2.get(), floatSample, first, block,
         narrowestSampleFormat);
      track.Set((samplePtr)buffer1.get(), floatSample, second, block,
         narrowestSampleFormat);

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
