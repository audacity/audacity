/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.cpp

  Lynn Allan (from DM's Normalize)
  Philip Van Baren (more options and boundary fixes)

*******************************************************************//**

\class EffectTruncSilence
\brief Truncate Silence automatically reduces the length of passages 
       where the volume is below a set threshold level.

  \todo mBlendFrameCount only retrieved from prefs ... not using dialog
        Only way to change (for windows) is thru registry
        The values should be figured dynamically ... too many frames could be invalid

*//****************************************************************//**

\class TruncSilenceDialog
\brief Dialog used with EffectTruncSilence

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/wx.h>
#include <wx/list.h>
#include <wx/listimpl.cpp>
#include <limits>
#include <math.h>

#include "../Experimental.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "TruncSilence.h"

WX_DEFINE_LIST(RegionList);

EffectTruncSilence::EffectTruncSilence()
{
   Init();
}

bool EffectTruncSilence::Init()
{
   mTruncInitialAllowedSilentMs = gPrefs->Read(wxT("/Effects/TruncateSilence/InitialAllowedSilentMs"), 200L);
   if ((mTruncInitialAllowedSilentMs < 0) || (mTruncInitialAllowedSilentMs >= 9999999)) {  // corrupted Prefs?
      mTruncInitialAllowedSilentMs = 200L;
      gPrefs->Write(wxT("/Effects/TruncateSilence/InitialAllowedSilentMs"), mTruncInitialAllowedSilentMs);
   }
   mTruncLongestAllowedSilentMs = gPrefs->Read(wxT("/Effects/TruncateSilence/LongestAllowedSilentMs"), 1000L);
   if ((mTruncLongestAllowedSilentMs < 0) || (mTruncLongestAllowedSilentMs >= 9999999)) {  // corrupted Prefs?
      mTruncLongestAllowedSilentMs = 1000L;
      gPrefs->Write(wxT("/Effects/TruncateSilence/LongestAllowedSilentMs"), mTruncLongestAllowedSilentMs);
   }
   
   if( mTruncLongestAllowedSilentMs < mTruncInitialAllowedSilentMs )
      mTruncInitialAllowedSilentMs = mTruncLongestAllowedSilentMs;

   mTruncDbChoiceIndex = gPrefs->Read(wxT("/Effects/TruncateSilence/DbChoiceIndex"), 4L);
   if ((mTruncDbChoiceIndex < 0) || (mTruncDbChoiceIndex >= Enums::NumDbChoices)) {  // corrupted Prefs?
      mTruncDbChoiceIndex = Enums::NumDbChoices - 1;  // Off-Skip
      gPrefs->Write(wxT("/Effects/TruncateSilence/DbChoiceIndex"), mTruncDbChoiceIndex);
      mTruncLongestAllowedSilentMs = SKIP_EFFECT_MILLISECOND;
      gPrefs->Write(wxT("/Effects/TruncateSilence/LongestAllowedSilentMs"), mTruncLongestAllowedSilentMs);
   }
   mBlendFrameCount = gPrefs->Read(wxT("/Effects/TruncateSilence/BlendFrameCount"), 100L);
   if ((mBlendFrameCount < 0) || (mBlendFrameCount >= 5000)) {  // corrupted Prefs?
      mBlendFrameCount = 100;
      gPrefs->Write(wxT("/Effects/TruncateSilence/BlendFrameCount"), 100);
   }
   mSilenceCompressRatio = 0.1*gPrefs->Read(wxT("/Effects/TruncateSilence/CompressRatio"), 40L);
   if ((mSilenceCompressRatio < 1.0) || (mSilenceCompressRatio > 20.0)) {  // corrupted Prefs?
      mSilenceCompressRatio = 4.0;
      gPrefs->Write(wxT("/Effects/TruncateSilence/CompressRatio"), 40L);
   }
   return gPrefs->Flush();
}

bool EffectTruncSilence::CheckWhetherSkipEffect()
{
   return ((mTruncDbChoiceIndex >= (Enums::NumDbChoices - 1))
          ||  (mTruncLongestAllowedSilentMs >= SKIP_EFFECT_MILLISECOND));
}

void EffectTruncSilence::End()
{
}

bool EffectTruncSilence::PromptUser()
{
   TruncSilenceDialog dlog(this, mParent);

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   gPrefs->Write(wxT("/Effects/TruncateSilence/InitialAllowedSilentMs"), mTruncInitialAllowedSilentMs);
   gPrefs->Write(wxT("/Effects/TruncateSilence/LongestAllowedSilentMs"), mTruncLongestAllowedSilentMs);
   gPrefs->Write(wxT("/Effects/TruncateSilence/DbChoiceIndex"), mTruncDbChoiceIndex);
   gPrefs->Write(wxT("/Effects/TruncateSilence/CompressRatio"), (int)floor(10.0*mSilenceCompressRatio+0.5));
   gPrefs->Flush();

   return true;
}

bool EffectTruncSilence::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferEnum(wxT("Db"), mTruncDbChoiceIndex, Enums::NumDbChoices, Enums::GetDbChoices());
   shuttle.TransferInt(wxT("Minimum"), mTruncInitialAllowedSilentMs, 200);
   shuttle.TransferInt(wxT("Duration"), mTruncLongestAllowedSilentMs, 1000);
   shuttle.TransferDouble(wxT("Compress"), mSilenceCompressRatio, 4.0f);
   return true;
}

#ifndef EXPERIMENTAL_TRUNC_SILENCE
#define QUARTER_SECOND_MS 250
bool EffectTruncSilence::Process()
{
   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *t;
   double t0 = mT0;
   double t1 = mT1;
   int tndx; 
   int tcount = 0;
   int fr;

   // Init using first track
   t = (WaveTrack *) iter.First();
   double rate = t->GetRate();
   sampleCount blockLen = t->GetMaxBlockSize();

   // Get the left and right bounds for all tracks
   while (t) {
      // Make sure all tracks have the same sample rate
      if (rate != t->GetRate()) {
         wxMessageBox(_("All tracks must have the same sample rate"), _("Truncate Silence"));
         return false;
      }

      // Count the tracks
      tcount++;

      // Set the current bounds to whichever left marker is
      // greater and whichever right marker is less
      t0 = wxMax(mT0, t->GetStartTime());
      t1 = wxMin(mT1, t->GetEndTime());

      // Use the smallest block size of all the tracks
      blockLen = wxMin(blockLen, t->GetMaxBlockSize());

      // Iterate to the next track
      t = (WaveTrack*) iter.Next();
   }

   // Just a sanity check, really it should be much higher
   if(blockLen < 4*mBlendFrameCount)
      blockLen = 4*mBlendFrameCount;

   // Transform the marker timepoints to samples
   t = (WaveTrack *) iter.First();
   sampleCount start = t->TimeToLongSamples(t0);
   sampleCount end = t->TimeToLongSamples(t1);

   // Bigger buffers reduce 'reset'
   //blockLen *= 8;
   // Stress-test the logic for cutting samples through block endpoints
   //blockLen /= 8;

   // Set thresholds
   // We have a lower bound on the amount of silence we chop out at a time
   // to avoid chopping up low frequency sounds.  We're good down to 10Hz
   // if we use 100ms.
   const float minTruncMs = 1.0f;
   double truncDbSilenceThreshold = Enums::Db2Signal[mTruncDbChoiceIndex];
   int truncInitialAllowedSilentSamples = 
      int((wxMax( mTruncInitialAllowedSilentMs, minTruncMs) * rate) / 1000.0);
   int truncLongestAllowedSilentSamples = 
      int((wxMax( mTruncLongestAllowedSilentMs, minTruncMs) * rate) / 1000.0);

   // Require at least 4 samples for lengths
   if(truncInitialAllowedSilentSamples < 4)
      truncInitialAllowedSilentSamples = 4;
   if(truncLongestAllowedSilentSamples < 4)
      truncLongestAllowedSilentSamples = 4;

   // If the cross-fade is longer than the minimum length,
   // then limit the cross-fade length to the minimum length
   // This allows us to have reasonable cross-fade by default
   // and still allow for 1ms minimum lengths
   if(truncInitialAllowedSilentSamples < mBlendFrameCount)
      mBlendFrameCount = truncInitialAllowedSilentSamples;
   if(truncLongestAllowedSilentSamples < mBlendFrameCount)
      mBlendFrameCount = truncLongestAllowedSilentSamples;

   // For sake of efficiency, don't let blockLen be less than double the longest silent samples
   // up until a sane limit of 1Meg samples
   while((blockLen > 0) && (blockLen < truncLongestAllowedSilentSamples*2) && (blockLen < 1048576)) {
      blockLen *= 2;
   }
    // Don't allow either value to be more than half of the block length
   if(truncLongestAllowedSilentSamples > blockLen/2)
      truncLongestAllowedSilentSamples = blockLen/2;
   if(truncInitialAllowedSilentSamples > truncLongestAllowedSilentSamples)
      truncInitialAllowedSilentSamples = truncLongestAllowedSilentSamples;

   // We use the 'longest' variable as additive to the 'initial' variable
   truncLongestAllowedSilentSamples -= truncInitialAllowedSilentSamples;

   // Perform the crossfade half-way through the minimum removed silence duration
   int rampInFrames = (truncInitialAllowedSilentSamples + mBlendFrameCount) / 2;
   if(rampInFrames > truncInitialAllowedSilentSamples)
      rampInFrames = truncInitialAllowedSilentSamples;

   // Allocate buffers
   float **buffer = new float*[tcount];
   for (tndx = 0; tndx < tcount; tndx++) {
      buffer[tndx] = new float[blockLen];
   }

   // Start processing
   //Track::All is needed because this effect has clear functionality
   this->CopyInputTracks(Track::All); // Set up mOutputTracks.
   SelectedTrackListOfKindIterator iterOut(Track::Wave, mOutputTracks);

   sampleCount index = start;
   sampleCount outTrackOffset = start;
   bool cancelled = false;
   // Reset
   bool ignoringFrames = false;
   bool truncToMinimum = true;  // Ignore the initial samples until we get above the noise floor
   sampleCount consecutiveSilentFrames = 0;
   sampleCount truncIndex = 0;
   sampleCount i = 0;
   sampleCount keep;

   while (index < end) {

      // Limit size of current block if we've reached the end
      sampleCount count = blockLen-i;
      if ((index + count) > end) {
         count = end - index; 
      }

      // Fill the buffers
      tndx = 0;
      t = (WaveTrack *) iter.First();
      while (t) {
         t->Get((samplePtr)(buffer[tndx++]+i), floatSample, index, count);
         t = (WaveTrack *) iter.Next();
      }

      // Shift over to account for samples remaining from prior block
      sampleCount limit = count+i;

      // Look for silences in current block
      for ( ; i < limit; i++) {

         // Is current frame in all tracks below threshold
         bool below = true;
         for (tndx = 0; tndx < tcount; tndx++) {
            if (fabs(buffer[tndx][i]) >= truncDbSilenceThreshold) {
               below = false;
               break;
            }
         }
         // Make sure we cross-fade and output the last silence
         // so we get a smooth transition into whatever follows the selected region
         // Also set the 'truncToMinimum' flag so that the last silence is truncated to the minimum amount
         if(below && ((index+i+1) == end)) {
            below = false;
            truncToMinimum = true;
         }

         // Count frame if it's below threshold
         if (below) {
            consecutiveSilentFrames++;

            // Ignore this frame (equivalent to cutting it)
            // otherwise, keep sample to be part of allowed silence
            if (consecutiveSilentFrames > truncInitialAllowedSilentSamples) {
               ignoringFrames = true;
               continue;
            }
         }
         else {
            if (ignoringFrames == true) {
               // Scale the consectiveSilentFrames so we keep a silence duration
               // which is proportional to the original silence up to the limit
               keep = consecutiveSilentFrames - truncInitialAllowedSilentSamples;
               keep /= mSilenceCompressRatio;

               // The first and last samples always get truncated to the minimum amount
               if(truncToMinimum == true)
                  keep = 0;
               if(keep > truncLongestAllowedSilentSamples)
                  keep = truncLongestAllowedSilentSamples;
               if(keep < 0)
                  keep = 0;

               // Compute the location of the cross-fade to be halfway through the silence
               // with restriction to the samples we still have available to use
               rampInFrames = (truncInitialAllowedSilentSamples - keep + mBlendFrameCount) / 2;
               if(rampInFrames > truncInitialAllowedSilentSamples)
                  rampInFrames = truncInitialAllowedSilentSamples;
               if(rampInFrames < mBlendFrameCount)
                  rampInFrames = mBlendFrameCount;

               // Include the cross-fade samples in the count to make the loop logic easier
               keep += rampInFrames;
               truncIndex -= rampInFrames;
               if(truncIndex < 0) {
                  // This happens when we have silence overlapping a block boundary
                  keep += truncIndex;
                  if(keep < 0)
                     keep = 0;
                  truncIndex = 0;
               }
               // back up for cross-fade
               sampleCount curOffset = i - keep;

               if(curOffset < 0) {
                  // This should never happen, but just in case...
                  keep += curOffset - rampInFrames;
                  if(keep < mBlendFrameCount)
                     keep = mBlendFrameCount;
                  curOffset = 0;
               }

               for (tndx = 0; tndx < tcount; tndx++) {
                  // Cross fade the cut point
                  for (fr = 0; fr < mBlendFrameCount; fr++) {
                     buffer[tndx][truncIndex+fr] = ((mBlendFrameCount-fr)*buffer[tndx][truncIndex+fr] + fr*buffer[tndx][curOffset + fr]) / mBlendFrameCount;
                  }
                  // Append the 'keep' samples, if any
                  for ( ; fr < keep; fr++) {
                     buffer[tndx][truncIndex+fr] = buffer[tndx][curOffset + fr];
                  }
               }
               truncIndex += keep;
            }
            consecutiveSilentFrames = 0;
            ignoringFrames = false;
            truncToMinimum = false;
         }

         // Can get here either because > dbThreshold
         // or silence duration isn't longer than allowed
         for (tndx = 0; tndx < tcount; tndx++) {
            buffer[tndx][truncIndex] = buffer[tndx][i];
         }
         truncIndex++;
      }

      // Update tracks if any samples were removed, now or before
      if (outTrackOffset + truncIndex != index + limit) {
         // Put updated sample back into output tracks.
         tndx = 0;
         t = (WaveTrack *) iterOut.First();
         while (t) {
            t->Set((samplePtr)buffer[tndx++], floatSample, outTrackOffset, truncIndex);
            t = (WaveTrack *) iterOut.Next();
         }
      }

      // If currently in a silent section, retain samples for the next pass
      if(consecutiveSilentFrames > mBlendFrameCount) {
         if (ignoringFrames == true) {
            // Retain only what we need for truncating the silence
            keep = consecutiveSilentFrames-truncInitialAllowedSilentSamples;
            if(keep > (truncLongestAllowedSilentSamples+mBlendFrameCount))
               keep = truncLongestAllowedSilentSamples+mBlendFrameCount;
            for (tndx = 0; tndx < tcount; tndx++) {
               // Cross fade the cut point
               for(fr = 0; fr < mBlendFrameCount; fr++) {
                  buffer[tndx][fr] = ((mBlendFrameCount-fr)*buffer[tndx][truncIndex-mBlendFrameCount+fr]
                                   + fr*buffer[tndx][i-keep+fr]) / mBlendFrameCount;
               }
               for( ; fr < keep; fr++) {
                  buffer[tndx][fr] = buffer[tndx][i-keep+fr];
               }
            }
            // Update the output index, less what we are retaining for next time
            outTrackOffset += truncIndex - mBlendFrameCount;
            // Append the following buffer to the existing data
            i = keep;
            truncIndex = mBlendFrameCount;
         } else {
            // Retain the silent samples for the next buffer
            keep = consecutiveSilentFrames;
            for (tndx = 0; tndx < tcount; tndx++) {
               for(fr=0 ; fr < keep; fr++) {
                  buffer[tndx][fr] = buffer[tndx][i-keep+fr];
               }
            }
            // Update the output index, less what we are retaining for next time
            outTrackOffset += truncIndex - keep;
            // Append the following buffer to the existing data
            i = keep;
            truncIndex = keep;
         }
      } else {
         // Maintain output index
         outTrackOffset += truncIndex;
         // Reset the buffer pointers to the beginning
         i = 0;
         truncIndex = 0;
         consecutiveSilentFrames = 0;
      }

      // Update progress and bail if user cancelled
      cancelled = TrackProgress(0, ((double)index / (double)end));
      if (cancelled) {
         break;
      }

      // Bump to next block
      index += count;
   }

   AudacityProject *p = GetActiveProject();
   if (!p)
      return false;

   // Remove stale data at end of output tracks.
   if (!cancelled && (outTrackOffset < end)) {
      t = (WaveTrack *) iterOut.First();
      while(t) {
         t->Clear(outTrackOffset / rate, t1);
         t = (WaveTrack *) iterOut.Next();
      }         

      t1 = outTrackOffset / rate;
   }

   // Free buffers
   for (tndx = 0; tndx < tcount; tndx++) {
      delete [] buffer[tndx];
   }
   delete [] buffer;

   mT0 = t0;
   mT1 = t1;

   this->ReplaceProcessedTracks(!cancelled); 
   return !cancelled;
}

#else // defined(EXPERIMENTAL_TRUNC_SILENCE)

// AWD: this is the new version!
bool EffectTruncSilence::Process()
{
   // Typical fraction of total time taken by detection (better to guess low)
   const double detectFrac = .4;

   // Copy tracks
   this->CopyInputTracks(Track::All);

   // Lower bound on the amount of silence to find at a time -- this avoids
   // detecting silence repeatedly in low-frequency sounds.
   const float minTruncMs = 1.0f;
   double truncDbSilenceThreshold = Enums::Db2Signal[mTruncDbChoiceIndex];

   // Master list of silent regions; it is responsible for deleting them.
   // This list should always be kept in order.
   RegionList silences;
   silences.DeleteContents(true);

   // Start with the whole selection silent
   Region *sel = new Region;
   sel->start = mT0;
   sel->end = mT1;
   silences.push_back(sel);

   // Remove non-silent regions in each track
   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   int whichTrack = 0;
   for (Track *t = iter.First(); t; t = iter.Next())
   {
      WaveTrack *wt = (WaveTrack *)t;

      // Smallest silent region to detect in frames
      sampleCount minSilenceFrames = 
         sampleCount((wxMax( mTruncInitialAllowedSilentMs, minTruncMs) *
                  wt->GetRate()) / 1000.0);

      //
      // Scan the track for silences
      //
      RegionList trackSilences;
      trackSilences.DeleteContents(true);
      sampleCount blockLen = wt->GetMaxBlockSize();
      sampleCount start = wt->TimeToLongSamples(mT0);
      sampleCount end = wt->TimeToLongSamples(mT1);

      // Allocate buffer
      float *buffer = new float[blockLen];

      sampleCount index = start;
      sampleCount silentFrames = 0;
      bool cancelled = false;

      // Keep position in overall silences list for optimization
      RegionList::iterator rit(silences.begin());

      while (index < end) {
         // Show progress dialog, test for cancellation
         cancelled = TotalProgress(
               detectFrac * (whichTrack + index / (double)end) / 
               (double)GetNumWaveTracks());
         if (cancelled)
            break;

         //
         // Optimization: if not in a silent region skip ahead to the next one
         //
         double curTime = wt->LongSamplesToTime(index);
         for ( ; rit != silences.end(); ++rit)
         {
            // Find the first silent region ending after current time
            if ((*rit)->end >= curTime)
               break;
         }

         if (rit == silences.end()) {
            // No more regions -- no need to process the rest of the track
            break;
         }
         else if ((*rit)->start > curTime) {
            // End current silent region, skip ahead
            if (silentFrames >= minSilenceFrames) {
               Region *r = new Region;
               r->start = wt->LongSamplesToTime(index - silentFrames);
               r->end = wt->LongSamplesToTime(index);
               trackSilences.push_back(r);
            }
            silentFrames = 0;

            index = wt->TimeToLongSamples((*rit)->start);
         }
         //
         // End of optimization
         //

         // Limit size of current block if we've reached the end
         sampleCount count = blockLen;
         if ((index + count) > end) {
            count = end - index;
         }

         // Fill buffer
         wt->Get((samplePtr)(buffer), floatSample, index, count);

         // Look for silences in current block
         for (sampleCount i = 0; i < count; ++i) {
            if (fabs(buffer[i]) < truncDbSilenceThreshold) {
               ++silentFrames;
            }
            else {
               if (silentFrames >= minSilenceFrames)
               {
                  // Record the silent region
                  Region *r = new Region;
                  r->start = wt->LongSamplesToTime(index + i - silentFrames);
                  r->end = wt->LongSamplesToTime(index + i);
                  trackSilences.push_back(r);
               }
               silentFrames = 0;
            }
         }

         // Next block
         index += count;
      }

      delete [] buffer;

      // Buffer has been freed, so we're OK to return if cancelled
      if (cancelled) 
      {
         ReplaceProcessedTracks(false);
         return false;
      }

      if (silentFrames >= minSilenceFrames)
      {
         // Track ended in silence -- record region
         Region *r = new Region;
         r->start = wt->LongSamplesToTime(index - silentFrames);
         r->end = wt->LongSamplesToTime(index);
         trackSilences.push_back(r);
      }

      // Intersect with the overall silent region list
      Intersect(silences, trackSilences);
      whichTrack++;
   }

   //
   // Now remove the silent regions from all selected / sync-lock selected tracks.
   //

   // Loop over detected regions in reverse (so cuts don't change time values
   // down the line)
   int whichReg = 0;
   RegionList::reverse_iterator rit;
   double totalCutLen = 0.0;  // For cutting selection at the end
   for (rit = silences.rbegin(); rit != silences.rend(); ++rit) {
      Region *r = *rit;

      // Progress dialog and cancellation. Do additional cleanup before return.
      if (TotalProgress(detectFrac + (1 - detectFrac) * whichReg / (double)silences.size()))
      {
         ReplaceProcessedTracks(false);
         return false;
      }

      // Intersection may create regions smaller than allowed; ignore them
      if (r->end - r->start < mTruncInitialAllowedSilentMs / 1000.0)
         continue;

      // Find new silence length as requested
      double inLength = r->end - r->start;
      double outLength = wxMin(
            mTruncInitialAllowedSilentMs / 1000.0 + (inLength - mTruncInitialAllowedSilentMs / 1000.0) / mSilenceCompressRatio,
            mTruncLongestAllowedSilentMs / 1000.0);
      double cutLen = inLength - outLength;
      totalCutLen += cutLen;

      TrackListIterator iterOut(mOutputTracks);
      for (Track *t = iterOut.First(); t; t = iterOut.Next())
      {
         // Don't waste time past the end of a track
         if (t->GetEndTime() < r->start)
            continue;

         if (t->GetKind() == Track::Wave && (
                  t->GetSelected() || t->IsSyncLockSelected()))
         {
            // In WaveTracks, clear with a cross-fade
            WaveTrack *wt = (WaveTrack *)t;
            sampleCount blendFrames = mBlendFrameCount;
            double cutStart = (r->start + r->end - cutLen) / 2;
            double cutEnd = cutStart + cutLen;
            // Round start/end times to frame boundaries
            cutStart = wt->LongSamplesToTime(wt->TimeToLongSamples(cutStart));
            cutEnd = wt->LongSamplesToTime(wt->TimeToLongSamples(cutEnd));

            // Make sure the cross-fade does not affect non-silent frames
            if (wt->LongSamplesToTime(blendFrames) > inLength) {
               blendFrames = wt->TimeToLongSamples(inLength);
            }

            // Perform cross-fade in memory
            float *buf1 = new float[blendFrames];
            float *buf2 = new float[blendFrames];
            sampleCount t1 = wt->TimeToLongSamples(cutStart) - blendFrames / 2;
            sampleCount t2 = wt->TimeToLongSamples(cutEnd) - blendFrames / 2;

            wt->Get((samplePtr)buf1, floatSample, t1, blendFrames);
            wt->Get((samplePtr)buf2, floatSample, t2, blendFrames);

            for (sampleCount i = 0; i < blendFrames; ++i) {
               buf1[i] = ((blendFrames-i) * buf1[i] + i * buf2[i]) /
                         (double)blendFrames;
            }

            // Perform the cut
            wt->Clear(cutStart, cutEnd);

            // Write cross-faded data
            wt->Set((samplePtr)buf1, floatSample, t1, blendFrames);

            delete [] buf1;
            delete [] buf2;
         }
         else if (t->GetSelected() || t->IsSyncLockSelected())
         {
            // Non-wave tracks: just do a sync-lock adjust
            double cutStart = (r->start + r->end - cutLen) / 2;
            double cutEnd = cutStart + cutLen;
            t->SyncLockAdjust(cutEnd, cutStart);
         }
      }
      ++whichReg;
   }

   mT1 -= totalCutLen;

   ReplaceProcessedTracks(true);

   return true;
}

// Finds the intersection of the ordered region lists, stores in dest
void EffectTruncSilence::Intersect(RegionList &dest, const RegionList &src)
{
   RegionList::iterator destIter;
   destIter = dest.begin();
   // Any time we reach the end of the dest list we're finished
   if (destIter == dest.end())
      return;
   Region *curDest = *destIter;

   // Operation: find non-silent regions in src, remove them from dest.
   double nsStart = curDest->start;
   double nsEnd;
   bool lastRun = false; // must run the loop one extra time

   RegionList::const_iterator srcIter = src.begin();
   
   // This logic, causing the loop to run once after end of src, must occur
   // each time srcIter is updated
   if (srcIter == src.end()) {
      lastRun = true;
   }

   while (srcIter != src.end() || lastRun)
   {
      // Don't use curSrc unless lastRun is false!
      Region *curSrc;

      if (lastRun)
      {
         // The last non-silent region extends as far as possible
         curSrc = NULL;
         nsEnd = std::numeric_limits<double>::max();
      }
      else
      {
         curSrc = *srcIter;
         nsEnd = curSrc->start;
      }

      if (nsEnd > nsStart)
      {
         // Increment through dest until we have a region that could be affected
         while (curDest->end <= nsStart) {
            ++destIter;
            if (destIter == dest.end())
               return;
            curDest = *destIter;
         }

         // Check for splitting dest region in two
         if (nsStart > curDest->start && nsEnd < curDest->end) {
            // The second region
            Region *r = new Region;
            r->start = nsEnd;
            r->end = curDest->end;

            // The first region
            curDest->end = nsStart;

            // Insert second region after first
            RegionList::iterator nextIt(destIter);
            ++nextIt;
            
            // This should just read: destIter = dest.insert(nextIt, r); but we
            // work around two two wxList::insert() bugs. First, in some
            // versions it returns the wrong value. Second, in some versions,
            // it crashes when you insert at list end.
            if (nextIt == dest.end()) {
               dest.Append(r);
            }
            else {
               dest.insert(nextIt, r);
            }
            ++destIter;          // (now points at the newly-inserted region)

            curDest = *destIter;
         }

         // Check for truncating the end of dest region
         if (nsStart > curDest->start && nsStart < curDest->end &&
               nsEnd >= curDest->end)
         {
            curDest->end = nsStart;

            ++destIter;
            if (destIter == dest.end())
               return;
            curDest = *destIter;
         }

         // Check for all dest regions that need to be removed completely
         while (nsStart <= curDest->start && nsEnd >= curDest->end) {
            destIter = dest.erase(destIter);
            if (destIter == dest.end())
               return;
            curDest = *destIter;
         }

         // Check for truncating the beginning of dest region
         if (nsStart <= curDest->start &&
               nsEnd > curDest->start && nsEnd < curDest->end)
         {
            curDest->start = nsEnd;
         }
      }

      if (lastRun) {
         // done
         lastRun = false;
      }
      else {
         // Next non-silent region starts at the end of this silent region
         nsStart = curSrc->end;
         ++srcIter;
         if (srcIter == src.end()) {
            lastRun = true;
         }
      }
   }
}

#endif // EXPERIMENTAL_TRUNC_SILENCE

void EffectTruncSilence::BlendFrames(float* buffer, int blendFrameCount, int leftIndex, int rightIndex)
{
   float* bufOutput = &buffer[leftIndex];
   float* bufBefore = &buffer[leftIndex];
   float* bufAfter  = &buffer[rightIndex];
   double beforeFactor = 1.0;
   double afterFactor  = 0.0;
   double adjFactor = 1.0 / (double)blendFrameCount;
   for (int j = 0; j < blendFrameCount; ++j) {
      bufOutput[j] = (float)((bufBefore[j] * beforeFactor) + (bufAfter[j] * afterFactor));
      beforeFactor -= adjFactor;
      afterFactor  += adjFactor;
   }
}

//----------------------------------------------------------------------------
// TruncSilenceDialog
//----------------------------------------------------------------------------

#define ID_SHORTEST_SILENCE_TEXT   7000
#define ID_LONGEST_SILENCE_TEXT   7001
#define ID_COMPRESS_FACTOR   7002
#define ID_DB_SILENCE_THRESHOLD_CHOICE 7003

BEGIN_EVENT_TABLE(TruncSilenceDialog, EffectDialog)
    EVT_BUTTON(ID_EFFECT_PREVIEW, TruncSilenceDialog::OnPreview)
    EVT_TEXT( ID_SHORTEST_SILENCE_TEXT, TruncSilenceDialog::OnDurationChange )
    EVT_TEXT( ID_LONGEST_SILENCE_TEXT, TruncSilenceDialog::OnDurationChange )
    EVT_TEXT( ID_COMPRESS_FACTOR, TruncSilenceDialog::OnDurationChange )
END_EVENT_TABLE()

TruncSilenceDialog::TruncSilenceDialog(EffectTruncSilence * effect,
                                       wxWindow * parent)
:  EffectDialog(parent, _("Truncate Silence"), PROCESS_EFFECT),
   mEffect(effect)
{
   Init();
}

void TruncSilenceDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("by Lynn Allan && Philip Van Baren"));
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxCENTER, false);
   {
      // Add a little space
   }
   S.EndHorizontalLay();

   S.StartThreeColumn();
   {
      wxArrayString choices(Enums::NumDbChoices, Enums::GetDbChoices());

      S.Id( ID_SHORTEST_SILENCE_TEXT ).TieNumericTextBox(_("Min silence duration:"),
                   mEffect->mTruncInitialAllowedSilentMs,
                   10);
      S.AddUnits( _("milliseconds") );
      S.Id( ID_LONGEST_SILENCE_TEXT ).TieNumericTextBox(_("Max silence duration:"),
                   mEffect->mTruncLongestAllowedSilentMs,
                   10);
      S.AddUnits( _("milliseconds") );
      S.Id( ID_COMPRESS_FACTOR ).TieNumericTextBox(_("Silence compression:"),
                   mEffect->mSilenceCompressRatio,
                   10);
      /* i18n-hint: Leave as is unless your language has a different way to show ratios like 5:1*/
      S.AddUnits( _(":1") );
      S.TieChoice(_("Threshold for silence:"),
                  mEffect->mTruncDbChoiceIndex,
                  &choices);
   }
   S.EndTwoColumn();
   pWarning = S.AddVariableText( wxT("") );
}

void TruncSilenceDialog::OnPreview(wxCommandEvent & event)
{
   TransferDataFromWindow();
   mEffect->Preview();
}

void TruncSilenceDialog::OnDurationChange(wxCommandEvent & event)
{
   // We may even get called during the constructor.
   // This test saves us from calling unsafe functions.
   if( !IsShown() )
      return;
   TransferDataFromWindow();
   bool bOk =  (mEffect->mTruncInitialAllowedSilentMs > 0.9f) 
            && (mEffect->mTruncLongestAllowedSilentMs > 0.9f)
            && (mEffect->mSilenceCompressRatio >= 1.0f);
   pWarning->SetLabel( bOk ? 
      wxT("") : 
   _("   Duration must be at least 1 millisecond\n   Compress ratio must be at least 1:1")
         );
   wxWindow *pWnd;
   pWnd = FindWindowById( wxID_OK, this );
   pWnd->Enable( bOk );
   pWnd = FindWindowById( ID_EFFECT_PREVIEW, this );
   pWnd->Enable( bOk );

}
