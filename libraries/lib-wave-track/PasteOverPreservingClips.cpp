/**********************************************************************

   Audacity: A Digital Audio Editor

   PasteOverPreservingClips.cpp

   Mitch Golden
   Vaughan Johnson
   Martyn Shaw

   Paul Licameli split from Equalization.cpp

**********************************************************************/
#include "PasteOverPreservingClips.h"
#include "WaveClip.h"
#include "WaveTrack.h"

ClipData CollectClipData(
   const WaveTrack &oldTrack, sampleCount start, sampleCount len)
{
   assert(oldTrack.IsLeader());
   ClipData results;
   auto &[clipStartEndTimes, clipRealStartEndTimes, clipNames] = results;

   double lenT = oldTrack.LongSamplesToTime(len);
   // 'start' is the sample offset in 't', the passed in track
   // 'startT' is the equivalent time value
   double startT = oldTrack.LongSamplesToTime(start);

   for (const auto &clip : oldTrack.GetClips()) {
      auto clipStartT = clip->GetPlayStartTime();
      auto clipEndT = clip->GetPlayEndTime();
      if (clipEndT <= startT)
         continue;   // clip is not within selection
      if (clipStartT >= startT + lenT)
         continue;   // clip is not within selection

      //save the actual clip start/end so that we can rejoin them after we paste.
      clipRealStartEndTimes.emplace_back(clipStartT, clipEndT);

      if (clipStartT < startT)  // does selection cover the whole clip?
         clipStartT = startT; // don't copy all the NEW clip
      if(clipEndT > startT + lenT)  // does selection cover the whole clip?
         clipEndT = startT + lenT; // don't copy all the NEW clip

      //save them
      clipStartEndTimes.emplace_back(clipStartT, clipEndT);
      clipNames.push_back(clip->GetName());
   }
   return results;
}

void PasteOverPreservingClips(const ClipData &data,
   WaveTrack &oldTrack, sampleCount start, sampleCount len,
   const WaveTrack &newContents)
{
   assert(oldTrack.IsLeader());
   assert(newContents.IsLeader());
   assert(oldTrack.NChannels() == newContents.NChannels());
   const auto &[clipStartEndTimes, clipRealStartEndTimes, clipNames] = data;

   //newContents has one waveclip for the total length, even though
   //oldTrack might have whitespace separating multiple clips
   //we want to maintain the original clip structure, so
   //only paste the intersections of the new clip.

   // now move the appropriate bit of the output back to the track
   // (this could be enhanced in the future to use the tails)
   double lenT = oldTrack.LongSamplesToTime(len);
   // 'start' is the sample offset in 't', the passed in track
   // 'startT' is the equivalent time value
   double startT = oldTrack.LongSamplesToTime(start);

   //now go through and replace the old clips with NEW
   for (unsigned int i = 0; i < clipStartEndTimes.size(); ++i) {
      //remove the old audio and get the NEW
      auto [start, end] = clipStartEndTimes[i];
      oldTrack.Clear(start, end);
   
      auto toClipOutput = newContents.Copy(start - startT, end - startT);
      oldTrack.Paste(start, *toClipOutput);

      //Restore original clip's name
      auto newClip = oldTrack.GetClipAtTime(start + 0.5 / oldTrack.GetRate());
      newClip->SetName(clipNames[i]);

      //if the clip was only partially selected, the Paste will have created a
      // split line.  Join is needed to take care of this
      //This is not true when the selection is fully contained within one clip
      // (second half of conditional)
      auto [realStart, realEnd] = clipRealStartEndTimes[i];
      if ((realStart  != start || realEnd != end) &&
         !(realStart <= startT && realEnd >= startT + lenT))
         oldTrack.Join(realStart, realEnd);
   }
}
