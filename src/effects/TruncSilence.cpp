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
   // FIXME: This misses the final (-80 dB) option.
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

      // Intersection may create regions smaller than allowed; ignore them.
      // Allow one nanosecond extra for consistent results with exact milliseconds of allowed silence.
      if ((r->end - r->start) < ((mTruncInitialAllowedSilentMs / 1000.0) - 0.000000001))
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
   S.AddSpace(0, 5);

   S.StartStatic(_("Detection"));
   {
      S.StartTwoColumn();
      {
         wxArrayString choices(Enums::NumDbChoices, Enums::GetDbChoices());
         S.TieChoice(_("Threshold for silence:"),
                     mEffect->mTruncDbChoiceIndex,
                     &choices);
      }
      S.EndTwoColumn();
      S.StartThreeColumn();
      {
         S.Id( ID_SHORTEST_SILENCE_TEXT ).TieNumericTextBox(_("Ignore silence less than:"),
                     mEffect->mTruncInitialAllowedSilentMs,
                     10);
         S.AddUnits( _("milliseconds") );
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Truncation"));
   {
      mTruncationMessage = S.AddVariableText(wxString::Format(_("For silences longer than %d milliseconds:"),
                                                 (gPrefs->Read(wxT("/Effects/TruncateSilence/InitialAllowedSilentMs"), 200L))));

      S.StartThreeColumn();
      {         
         S.Id( ID_COMPRESS_FACTOR ).TieNumericTextBox(_("Compress silence by:"),
                     mEffect->mSilenceCompressRatio,
                     10);
         /* i18n-hint: Leave as is unless your language has a different way to show ratios like 5:1*/
         S.AddUnits( _(":1") );

         // Truncation.
         S.Id( ID_LONGEST_SILENCE_TEXT ).TieNumericTextBox(_("and then truncate to:"),
                     mEffect->mTruncLongestAllowedSilentMs,
                     10);
         S.AddUnits( _("milliseconds.") );

      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   pWarning = S.AddVariableText( wxT("") );
}

void TruncSilenceDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();
   mEffect->Preview();
}

void TruncSilenceDialog::OnDurationChange(wxCommandEvent & WXUNUSED(event))
{
   // We may even get called during the constructor.
   // This test saves us from calling unsafe functions.
   if( !IsShown() )
      return;
   TransferDataFromWindow();

   mTruncationMessage->SetLabel(wxString::Format(_("For silence longer than %d milliseconds:"),
                                                 (mEffect->mTruncInitialAllowedSilentMs)));

   bool bOk =  true;

   wxString warningText;
   if (mEffect->mTruncInitialAllowedSilentMs < 1.0f) {
      bOk = false;
      warningText = _("Ignored silence must be at least 1 millisecond");
   }
   if (mEffect->mTruncLongestAllowedSilentMs < 1.0f) {
      bOk = false;
      warningText = _("Cannot truncate to less than 1 millisecond");
   }
   if (mEffect->mSilenceCompressRatio < 1.0f) {
      bOk = false;
      warningText = _("Compression ratio must be at least 1:1");
   }

   pWarning->SetLabel( bOk ? wxT("") : warningText);

   wxWindow *pWnd;
   pWnd = FindWindowById( wxID_OK, this );
   pWnd->Enable( bOk );
   pWnd = FindWindowById( ID_EFFECT_PREVIEW, this );
   pWnd->Enable( bOk );

}
