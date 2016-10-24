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
#include "../Prefs.h"
#include "../WaveTrack.h"

#include "TimeWarper.h"

#include "../MemoryX.h"

bool Generator::Process()
{
   if (GetDuration() < 0.0)
      return false;


   // Set up mOutputTracks.
   // This effect needs Track::All for sync-lock grouping.
   this->CopyInputTracks(Track::All);

   // Iterate over the tracks
   bool bGoodResult = true;
   int ntrack = 0;
   TrackListIterator iter(mOutputTracks.get());
   Track* t = iter.First();

   while (t != NULL)
   {
      if (t->GetKind() == Track::Wave && t->GetSelected()) {
         WaveTrack* track = (WaveTrack*)t;

         bool editClipCanMove;
         gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove, true);

         //if we can't move clips, and we're generating into an empty space,
         //make sure there's room.
         if (!editClipCanMove &&
             track->IsEmpty(mT0, mT1+1.0/track->GetRate()) &&
             !track->IsEmpty(mT0, mT0+GetDuration()-(mT1-mT0)-1.0/track->GetRate()))
         {
            wxMessageBox(
                  _("There is not enough room available to generate the audio"),
                  _("Error"), wxICON_STOP);
            Failure();
            return false;
         }

         if (GetDuration() > 0.0)
         {
            AudacityProject *p = GetActiveProject();
            // Create a temporary track
            WaveTrack::Holder tmp(
               mFactory->NewWaveTrack(track->GetSampleFormat(),
               track->GetRate())
            );
            BeforeTrack(*track);
            BeforeGenerate();

            // Fill it with data
            if (!GenerateTrack(&*tmp, *track, ntrack))
               bGoodResult = false;
            else {
               // Transfer the data from the temporary track to the actual one
               tmp->Flush();
               SetTimeWarper(std::make_unique<StepTimeWarper>(mT0+GetDuration(), GetDuration()-(mT1-mT0)));
               bGoodResult = track->ClearAndPaste(p->GetSel0(), p->GetSel1(), &*tmp, true,
                     false, GetTimeWarper());
            }

            if (!bGoodResult) {
               Failure();
               return false;
            }
         }
         else
         {
            // If the duration is zero, there's no need to actually
            // generate anything
            track->Clear(mT0, mT1);
         }

         ntrack++;
      }
      else if (t->IsSyncLockSelected()) {
         t->SyncLockAdjust(mT1, mT0 + GetDuration());
      }
      // Move on to the next track
      t = iter.Next();
   }

   Success();

   this->ReplaceProcessedTracks(bGoodResult);

   mT1 = mT0 + GetDuration(); // Update selection.

   return true;
}

bool BlockGenerator::GenerateTrack(WaveTrack *tmp,
                                   const WaveTrack &track,
                                   int ntrack)
{
   bool bGoodResult = true;
   numSamples = track.TimeToLongSamples(GetDuration());
   decltype(numSamples) i = 0;
   float *data = new float[tmp->GetMaxBlockSize()];

   while ((i < numSamples) && bGoodResult) {
      const auto block =
         limitSampleBufferSize( tmp->GetBestBlockSize(i), numSamples - i );

      GenerateBlock(data, track, block);

      // Add the generated data to the temporary track
      tmp->Append((samplePtr)data, floatSample, block);
      i += block;

      // Update the progress meter
      if (TrackProgress(ntrack,
                        i.as_double() /
                        numSamples.as_double()))
         bGoodResult = false;
   }
   delete[] data;
   return bGoodResult;
}
