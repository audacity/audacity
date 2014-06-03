/**********************************************************************

  Audacity: A Digital Audio Editor

  Reverse.cpp

  Mark Phillips

*******************************************************************//**

\class EffectReverse
\brief An Effect that reverses the selected audio.

*//********************************************************************/


#include "../Audacity.h"

#include <math.h>

#include "Reverse.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "../LabelTrack.h"

//
// EffectReverse
//

EffectReverse::EffectReverse()
{
}

bool EffectReverse::Process()
{
   //Track::All is needed because Reverse should move the labels too
   this->CopyInputTracks(Track::All); // Set up mOutputTracks.
   bool bGoodResult = true;

   TrackListIterator iter(mOutputTracks);
   Track *t = iter.First();
   int count = 0;
   while (t) {
      if (t->GetKind() == Track::Wave &&
            (t->GetSelected() || t->IsSyncLockSelected()))
      {
         WaveTrack *track = (WaveTrack*)t;

         if (mT1 > mT0) {
            sampleCount start = track->TimeToLongSamples(mT0);
            sampleCount end = track->TimeToLongSamples(mT1);
            sampleCount len = (sampleCount)(end - start);

            if (!ProcessOneWave(count, track, start, len))
            {
               bGoodResult = false;
               break;
            }
         }
      }
      else if (t->GetKind() == Track::Label &&
            (t->GetSelected() || t->IsSyncLockSelected()))
      {
         LabelTrack *track = (LabelTrack*)t;
         track->ChangeLabelsOnReverse(mT0, mT1);
      }
      t = iter.Next();
      count++;
   }

   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

bool EffectReverse::ProcessOneWave(int count, WaveTrack * track, sampleCount start, sampleCount len)
{
   bool rValue = true; // return value

   sampleCount end = (sampleCount) start + len; // start, end, len refer to the selected reverse region

   // STEP 1:
   // If a reverse selection begins and/or ends at the inside of a clip
   // perform a split at the start and/or end of the reverse selection
   WaveClipList::compatibility_iterator node = track->GetClipIterator();
   while (node) {
      WaveClip *clip = node->GetData();
      sampleCount clipStart = clip->GetStartSample();
      sampleCount clipEnd = clip->GetEndSample();
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
      node = node->GetNext();
   }

   //STEP 2:
   // Individually reverse each clip inside the selected region
   // and apply the appropriate offset after detaching them from the track

   bool checkedFirstClip = false;

   // used in calculating the offset of clips to rearrange
   // holds the new end position of the current clip
   sampleCount currentEnd = (sampleCount)end;

   WaveClipList revClips; // holds the reversed clips
   WaveClipList otherClips; // holds the clips that appear after the reverse selection region
   WaveClipArray clipArray;
   track->FillSortedClipArray(clipArray);
   size_t i;
   for (i=0; i < clipArray.Count(); i++) {

      WaveClip *clip = clipArray.Item(i);
      sampleCount clipStart = clip->GetStartSample();
      sampleCount clipEnd = clip->GetEndSample();

      if (clipStart >= start && clipEnd <= end) { // if the clip is inside the selected region

         // this is used to check if the selected region begins with a whitespace.
         // if yes then clipStart (of the first clip) and start are not the same.
         // adjust currentEnd accordingly and set endMerge to false
         if(checkedFirstClip == false && clipStart > start) {
            checkedFirstClip = true;
            if(i > 0) {
               if (clipArray.Item(i-1)->GetEndSample() <= start) {
                  currentEnd -= (clipStart - start);
               }
            }
            else {
               currentEnd -= (clipStart - start);
            }
         }

         sampleCount revStart = (clipStart >= start)? clipStart: start;
         sampleCount revEnd = (clipEnd >= end)? end: clipEnd;
         sampleCount revLen = (sampleCount)revEnd-revStart;
         if (revEnd >= revStart) {
            if(!ProcessOneClip(count, track, revStart, revLen, start, end)) // reverse the clip
            {
               rValue = false;
               break;
            }

            sampleCount clipOffsetStart = (sampleCount)(currentEnd - (clipEnd-clipStart)); // calculate the offset required
            double offsetStartTime = track->LongSamplesToTime(clipOffsetStart);
            if(i+1 < clipArray.Count()) // update currentEnd if there is a clip to process next
            {
               sampleCount nextClipStart = clipArray.Item(i+1)->GetStartSample();
               currentEnd = (sampleCount)(currentEnd - (clipEnd - clipStart) - (nextClipStart - clipEnd));
            }

            clip = track->RemoveAndReturnClip(clip); // detach the clip from track
            clip->SetOffset(track->LongSamplesToTime(track->TimeToLongSamples(offsetStartTime))); // align time to a sample and set offset
            revClips.Append(clip);

         }
      }
      else if (clipStart >= end) { // clip is after the selection region
         clip = track->RemoveAndReturnClip(clip); // simply remove and append to otherClips
         otherClips.Append(clip);
      }
   }

   // STEP 3: Append the clips from
   // revClips and otherClips back to the track
   size_t revClipsCount = revClips.GetCount();
   for (i = 0; i < revClipsCount; i++) {
      WaveClipList::compatibility_iterator node = revClips.Item(revClipsCount - 1 - i); // the last clip of revClips is appended to the track first
      WaveClip *clip = node->GetData();
      track->AddClip(clip);
   }

   for (i = 0; i < otherClips.GetCount(); i++) {
      WaveClipList::compatibility_iterator node = otherClips.Item(i);
      track->AddClip(node->GetData());
   }

   return rValue;
}

bool EffectReverse::ProcessOneClip(int count, WaveTrack *track,
                               sampleCount start, sampleCount len,
                               sampleCount originalStart, sampleCount originalEnd)
{
   bool rc = true;
   // keep track of two blocks whose data we will swap
   sampleCount first = start;
   sampleCount second;

   sampleCount blockSize = track->GetMaxBlockSize();
   float tmp;
   float *buffer1 = new float[blockSize];
   float *buffer2 = new float[blockSize];

   sampleCount originalLen = (sampleCount)originalEnd-originalStart;

   while (len > 1) {
      sampleCount block = track->GetBestBlockSize(first);
      if (block > len / 2)
         block = len / 2;
      second = first + (len - block);

      track->Get((samplePtr)buffer1, floatSample, first, block);
      track->Get((samplePtr)buffer2, floatSample, second, block);
      for (int i = 0; i < block; i++) {
         tmp = buffer1[i];
         buffer1[i] = buffer2[block-i-1];
         buffer2[block-i-1] = tmp;
      }
      track->Set((samplePtr)buffer1, floatSample, first, block);
      track->Set((samplePtr)buffer2, floatSample, second, block);

      len -= 2 * block;
      first += block;

      if( TrackProgress(count, 2*(first-originalStart) / (double) originalLen) ) {
         rc = false;
         break;
      }
   }

   delete[] buffer1;
   delete[] buffer2;

   return rc;
}

