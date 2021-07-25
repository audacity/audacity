/**********************************************************************

  Audacity: A Digital Audio Editor

  Generator.h

  Two Abstract classes, Generator, and BlockGenerator, that effects which
  generate audio should derive from.

  Block Generator breaks the synthesis task up into smaller parts.

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#include "Generator.h"

#include "../Project.h"
#include "Prefs.h"
#include "../ViewInfo.h"
#include "../WaveTrack.h"
#include "../prefs/TracksBehaviorsPrefs.h"

#include "TimeWarper.h"

#include "../widgets/AudacityMessageBox.h"

bool Generator::Process()
{
   if (GetDuration() < 0.0)
      return false;


   // Set up mOutputTracks.
   // This effect needs all for sync-lock grouping.
   this->CopyInputTracks(true);

   // Iterate over the tracks
   bool bGoodResult = true;
   int ntrack = 0;

   mOutputTracks->Any().VisitWhile( bGoodResult,
      [&](WaveTrack *track, const Track::Fallthrough &fallthrough) {
         if (!track->GetSelected())
            return fallthrough();
         bool editClipCanMove = GetEditClipsCanMove();

         //if we can't move clips, and we're generating into an empty space,
         //make sure there's room.
         if (!editClipCanMove &&
             track->IsEmpty(mT0, mT1+1.0/track->GetRate()) &&
             !track->IsEmpty(mT0, mT0+GetDuration()-(mT1-mT0)-1.0/track->GetRate()))
         {
            Effect::MessageBox(
               XO("There is not enough room available to generate the audio"),
               wxICON_STOP,
               XO("Error") );
            Failure();
            bGoodResult = false;
            return;
         }

         if (GetDuration() > 0.0)
         {
            auto pProject = FindProject();
            // Create a temporary track
            auto tmp = track->EmptyCopy();
            BeforeTrack(*track);
            BeforeGenerate();

            // Fill it with data
            if (!GenerateTrack(&*tmp, *track, ntrack))
               bGoodResult = false;
            else {
               // Transfer the data from the temporary track to the actual one
               tmp->Flush();
               PasteTimeWarper warper{ mT1, mT0+GetDuration() };
               const auto &selectedRegion =
                  ViewInfo::Get( *pProject ).selectedRegion;
               track->ClearAndPaste(
                  selectedRegion.t0(), selectedRegion.t1(),
                  &*tmp, true, false, &warper);
            }

            if (!bGoodResult) {
               Failure();
               return;
            }
         }
         else
         {
            // If the duration is zero, there's no need to actually
            // generate anything
            track->Clear(mT0, mT1);
         }

         ntrack++;
      },
      [&](Track *t) {
         if (t->IsSyncLockSelected()) {
            t->SyncLockAdjust(mT1, mT0 + GetDuration());
         }
      }
   );

   if (bGoodResult) {
      Success();

      this->ReplaceProcessedTracks(bGoodResult);

      mT1 = mT0 + GetDuration(); // Update selection.
   }

   return bGoodResult;
}

bool BlockGenerator::GenerateTrack(WaveTrack *tmp,
                                   const WaveTrack &track,
                                   int ntrack)
{
   bool bGoodResult = true;
   numSamples = track.TimeToLongSamples(GetDuration());
   decltype(numSamples) i = 0;
   Floats data{ tmp->GetMaxBlockSize() };

   while ((i < numSamples) && bGoodResult) {
      const auto block =
         limitSampleBufferSize( tmp->GetBestBlockSize(i), numSamples - i );

      GenerateBlock(data.get(), track, block);

      // Add the generated data to the temporary track
      tmp->Append((samplePtr)data.get(), floatSample, block);
      i += block;

      // Update the progress meter
      if (TrackProgress(ntrack,
                        i.as_double() /
                        numSamples.as_double()))
         bGoodResult = false;
   }
   return bGoodResult;
}
