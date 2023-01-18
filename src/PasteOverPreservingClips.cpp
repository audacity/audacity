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

void PasteOverPreservingClips(
   WaveTrack &oldTrack, sampleCount start, sampleCount len,
   WaveTrack &newContents)
{
   // now move the appropriate bit of the output back to the track
   // (this could be enhanced in the future to use the tails)
   double lenT = oldTrack.LongSamplesToTime(len);
   // 'start' is the sample offset in 't', the passed in track
   // 'startT' is the equivalent time value
   // 'newContents' starts at zero
   double startT = oldTrack.LongSamplesToTime(start);

   //newContents has one waveclip for the total length, even though
   //oldTrack might have whitespace separating multiple clips
   //we want to maintain the original clip structure, so
   //only paste the intersections of the NEW clip.

   //Find the bits of clips that need replacing
   std::vector<std::pair<double, double> > clipStartEndTimes;
   //may be truncated due to a clip being partially selected
   std::vector<std::pair<double, double> > clipRealStartEndTimes;
   //Used to restore clip names after pasting
   std::vector<wxString> clipNames;
   for (const auto &clip : oldTrack.GetClips()) {
      auto clipStartT = clip->GetPlayStartTime();
      auto clipEndT = clip->GetPlayEndTime();
      if ( clipEndT <= startT )
         continue;   // clip is not within selection
      if ( clipStartT >= startT + lenT )
         continue;   // clip is not within selection

      //save the actual clip start/end so that we can rejoin them after we paste.
      clipRealStartEndTimes.emplace_back(clipStartT, clipEndT);

      if ( clipStartT < startT )  // does selection cover the whole clip?
         clipStartT = startT; // don't copy all the NEW clip
      if( clipEndT > startT + lenT )  // does selection cover the whole clip?
         clipEndT = startT + lenT; // don't copy all the NEW clip

      //save them
      clipStartEndTimes.emplace_back(clipStartT, clipEndT);
      clipNames.push_back(clip->GetName());
   }
   //now go through and replace the old clips with NEW
   for (unsigned int i = 0; i < clipStartEndTimes.size(); ++i) {
      //remove the old audio and get the NEW
      auto [start, end] = clipStartEndTimes[i];
      oldTrack.Clear(start, end);
      auto toClipOutput = newContents.Copy(start - startT, end - startT);
      //put the processed audio in
      oldTrack.Paste(start, toClipOutput.get());

      //Restore original clip's name
      auto newClip = oldTrack.GetClipAtTime(start + 0.5 / oldTrack.GetRate());
      newClip->SetName(clipNames[i]);

      //if the clip was only partially selected, the Paste will have created a
      // split line.  Join is needed to take care of this
      //This is not true when the selection is fully contained within one clip
      // (second half of conditional)
      auto [realStart, realEnd] = clipRealStartEndTimes[i];
      if ((realStart  != start || realEnd != end) &&
         !(realStart <= startT && realEnd >= startT + lenT) )
         oldTrack.Join(realStart, realEnd);
   }
}
