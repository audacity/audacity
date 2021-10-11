/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.cpp

  Mark Phillips

*******************************************************************//**

\class EffectReverse
\brief An Effect that reverses the selected audio.

*//********************************************************************/



#include "Reverse.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/intl.h>

#include "../LabelTrack.h"
#include "../WaveClip.h"
#include "../WaveTrack.h"

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

ComponentInterfaceSymbol EffectReverse::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectReverse::GetDescription()
{
   return XO("Reverses the selected audio");
}

// EffectDefinitionInterface implementation

EffectType EffectReverse::GetType()
{
   return EffectTypeProcess;
}

bool EffectReverse::IsInteractive()
{
   return false;
}

// Effect implementation

bool EffectReverse::Process()
{
   //all needed because Reverse should move the labels too
   this->CopyInputTracks(true); // Set up mOutputTracks.
   bool bGoodResult = true;
   int count = 0;

   auto trackRange =
      mOutputTracks->Any() + &Track::IsSelectedOrSyncLockSelected;
   trackRange.VisitWhile( bGoodResult,
      [&](WaveTrack * track) {
         if (mT1 > mT0) {
            auto start = track->TimeToLongSamples(mT0);
            auto end = track->TimeToLongSamples(mT1);
            auto len = end - start;

            if (!ProcessOneWave(count, track, start, len))
               bGoodResult = false;
         }
         count++;
      },
      [&](LabelTrack * track) {
         track->ChangeLabelsOnReverse(mT0, mT1);
         count++;
      }
   );

   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

bool EffectReverse::ProcessOneWave(int count, WaveTrack * track, sampleCount start, sampleCount len)
{
   bool rValue = true; // return value

   auto end = start + len; // start, end, len refer to the selected reverse region

   // STEP 1:
   // If a reverse selection begins and/or ends at the inside of a clip
   // perform a split at the start and/or end of the reverse selection
   const auto &clips = track->GetClips();
   // Beware, the array grows as we loop over it.  Use integer subscripts, not iterators.
   for (size_t ii = 0; ii < clips.size(); ++ii) {
      const auto &clip = clips[ii].get();
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();
      if (clipStart < start && clipEnd > start && clipEnd <= end) { // the reverse selection begins at the inside of a clip
         double splitTime = track->LongSamplesToTime(start);
         track->SplitAt(splitTime);
      }
      else if (clipStart >= start && clipStart < end && clipEnd > end) { // the reverse selection ends at the inside of a clip
         double splitTime = track->LongSamplesToTime(end);
         track->SplitAt(splitTime);
      }
      else if (clipStart < start && clipEnd > end) { // the selection begins AND ends at the inside of a clip
         double splitTime = track->LongSamplesToTime(start);
         track->SplitAt(splitTime);
         splitTime = track->LongSamplesToTime(end);
         track->SplitAt(splitTime);
      }
   }

   //STEP 2:
   // Individually reverse each clip inside the selected region
   // and apply the appropriate offset after detaching them from the track

   bool checkedFirstClip = false;

   // used in calculating the offset of clips to rearrange
   // holds the NEW end position of the current clip
   auto currentEnd = end;

   WaveClipHolders revClips; // holds the reversed clips
   WaveClipHolders otherClips; // holds the clips that appear after the reverse selection region
   auto clipArray = track->SortedClipArray();
   size_t i;
   for (i=0; i < clipArray.size(); i++) {

      WaveClip *clip = clipArray[i];
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();

      if (clipStart >= start && clipEnd <= end) { // if the clip is inside the selected region

         // this is used to check if the selected region begins with a whitespace.
         // if yes then clipStart (of the first clip) and start are not the same.
         // adjust currentEnd accordingly and set endMerge to false
         if(checkedFirstClip == false && clipStart > start) {
            checkedFirstClip = true;
            if(i > 0) {
               if (clipArray[i-1]->GetPlayEndSample() <= start) {
                  currentEnd -= (clipStart - start);
               }
            }
            else {
               currentEnd -= (clipStart - start);
            }
         }

         auto revStart = (clipStart >= start)? clipStart: start;
         auto revEnd = (clipEnd >= end)? end: clipEnd;
         auto revLen = revEnd - revStart;
         if (revEnd >= revStart) {
            if(!ProcessOneClip(count, track, revStart, revLen, start, end)) // reverse the clip
            {
               rValue = false;
               break;
            }

            auto clipOffsetStart = currentEnd - (clipEnd - clipStart); // calculate the offset required
            double offsetStartTime = track->LongSamplesToTime(clipOffsetStart);
            if(i+1 < clipArray.size()) // update currentEnd if there is a clip to process next
            {
               auto nextClipStart = clipArray[i+1]->GetPlayStartSample();
               currentEnd = currentEnd - (clipEnd - clipStart) - (nextClipStart - clipEnd);
            }

            revClips.push_back(track->RemoveAndReturnClip(clip)); // detach the clip from track
            revClips.back()->SetPlayStartTime(track->LongSamplesToTime(track->TimeToLongSamples(offsetStartTime))); // align time to a sample and set offset
         }
      }
      else if (clipStart >= end) { // clip is after the selection region
         otherClips.push_back(track->RemoveAndReturnClip(clip)); // simply remove and append to otherClips
      }
   }

   // STEP 3: Append the clips from
   // revClips and otherClips back to the track
   // the last clip of revClips is appended to the track first
   // PRL:  I don't think that matters, the sequence of storage of clips in the track
   // is not elsewhere assumed to be by time
   {
      for (auto it = revClips.rbegin(), revEnd = revClips.rend(); rValue && it != revEnd; ++it)
         rValue = track->AddClip(*it);
   }

   for (auto &clip : otherClips)
      if (!(rValue = track->AddClip(clip)))
          break;

   return rValue;
}

bool EffectReverse::ProcessOneClip(int count, WaveTrack *track,
                               sampleCount start, sampleCount len,
                               sampleCount originalStart, sampleCount originalEnd)
{
   bool rc = true;
   // keep track of two blocks whose data we will swap
   auto first = start;

   auto blockSize = track->GetMaxBlockSize();
   float tmp;
   Floats buffer1{ blockSize };
   Floats buffer2{ blockSize };

   auto originalLen = originalEnd - originalStart;

   while (len > 1) {
      auto block =
         limitSampleBufferSize( track->GetBestBlockSize(first), len / 2 );
      auto second = first + (len - block);

      track->GetFloats(buffer1.get(), first, block);
      track->GetFloats(buffer2.get(), second, block);
      for (decltype(block) i = 0; i < block; i++) {
         tmp = buffer1[i];
         buffer1[i] = buffer2[block-i-1];
         buffer2[block-i-1] = tmp;
      }
      track->Set((samplePtr)buffer1.get(), floatSample, first, block);
      track->Set((samplePtr)buffer2.get(), floatSample, second, block);

      len -= 2 * block;
      first += block;

      if( TrackProgress(count, 2 * ( first - originalStart ).as_double() /
                        originalLen.as_double() ) ) {
         rc = false;
         break;
      }
   }

   return rc;
}
