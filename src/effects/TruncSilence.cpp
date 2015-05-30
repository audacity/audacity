/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.cpp

  Lynn Allan (from DM's Normalize)
  Philip Van Baren (more options and boundary fixes)

*******************************************************************//**

\class EffectTruncSilence
\brief Truncate Silence automatically reduces the length of passages
       where the volume is below a set threshold level.

*//*******************************************************************/

#include "../Audacity.h"

#include <limits>
#include <math.h>

#include <wx/valgen.h>

#include "../Prefs.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"

#include "TruncSilence.h"

enum kActions
{
   kTruncate,
   kCompress,
   kNumActions
};

static const wxChar *kActionStrings[kNumActions] =
{
   XO("Truncate Detected Silence"),
   XO("Compress Excess Silence")
};

// Define defaults, minimums, and maximums for each parameter
#define DefaultAndLimits(name, def, min, max) \
   static const double DEF_ ## name = (def); \
   static const double MIN_ ## name = (min); \
   static const double MAX_ ## name = (max);

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key               Def         Min      Max                        Scale
Param( DbIndex,   int,     XO("Db"),         0,          0,       Enums::NumDbChoices - 1,   1  );
Param( ActIndex,  int,     XO("Action"),     kTruncate,  0,       kNumActions - 1,           1  );
Param( Minimum,   double,  XO("Minimum"),    0.5,        0.001,   10000.0,                   1  );
Param( Truncate,  double,  XO("Truncate"),   0.5,        0.0,     10000.0,                   1  );
Param( Compress,  double,  XO("Compress"),   50.0,       0.0,     99.9,                      1  );

static const sampleCount DEF_BlendFrameCount = 100;

#include <wx/listimpl.cpp>
WX_DEFINE_LIST(RegionList);

BEGIN_EVENT_TABLE(EffectTruncSilence, wxEvtHandler)
   EVT_CHOICE(wxID_ANY, EffectTruncSilence::OnControlChange)
   EVT_TEXT(wxID_ANY, EffectTruncSilence::OnControlChange)
END_EVENT_TABLE()

EffectTruncSilence::EffectTruncSilence()
{
   mDbChoices = wxArrayString(Enums::NumDbChoices, Enums::GetDbChoices());

   mInitialAllowedSilence = DEF_Minimum;
   mTruncLongestAllowedSilence = DEF_Truncate;
   mSilenceCompressPercent = DEF_Compress;
   mTruncDbChoiceIndex = DEF_DbIndex;
   mActionIndex = DEF_ActIndex;

   SetLinearEffectFlag(false);

   // This used to be changeable via the audacity.cfg/registery.  Doubtful that was
   // ever done.
   //
   // Original comment:
   //
   //   mBlendFrameCount only retrieved from prefs ... not using dialog
   //   Only way to change (for windows) is thru registry
   //   The values should be figured dynamically ... too many frames could be invalid
   mBlendFrameCount = DEF_BlendFrameCount;
}

EffectTruncSilence::~EffectTruncSilence()
{
}

// IdentInterface implementation

wxString EffectTruncSilence::GetSymbol()
{
   return TRUNCATESILENCE_PLUGIN_SYMBOL;
}

wxString EffectTruncSilence::GetDescription()
{
   return XO("Automatically reduces the length of passages where the volume is below a specified level");
}

// EffectIdentInterface implementation

EffectType EffectTruncSilence::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

bool EffectTruncSilence::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_DbIndex, Enums::DbChoices[mTruncDbChoiceIndex]);
   parms.Write(KEY_ActIndex, kActionStrings[mActionIndex]);
   parms.Write(KEY_Minimum, mInitialAllowedSilence);
   parms.Write(KEY_Truncate, mTruncLongestAllowedSilence);
   parms.Write(KEY_Compress, mSilenceCompressPercent);
   
   return true;
}

bool EffectTruncSilence::SetAutomationParameters(EffectAutomationParameters & parms)
{
   wxArrayString actions(kNumActions, kActionStrings);
   actions.Insert(wxT("0"), 0); // Compatible with 2.1.0 and before
   actions.Insert(wxT("1"), 1); // Compatible with 2.1.0 and before

   ReadAndVerifyDouble(Minimum);
   ReadAndVerifyDouble(Truncate);
   ReadAndVerifyDouble(Compress);
   ReadAndVerifyEnum(DbIndex, mDbChoices);
   ReadAndVerifyEnum(ActIndex, actions);

   mInitialAllowedSilence = Minimum;
   mTruncLongestAllowedSilence = Truncate;
   mSilenceCompressPercent = Compress;
   mTruncDbChoiceIndex = DbIndex;
   mActionIndex = ActIndex;

   // Readjust for 2.1.0 or before
   if (mActionIndex >= kNumActions)
   {
      mActionIndex -= kNumActions;
   }

   return true;
}

// Effect implementation

bool EffectTruncSilence::Startup()
{
   wxString base = wxT("/Effects/TruncateSilence/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      mTruncDbChoiceIndex = gPrefs->Read(base + wxT("DbChoiceIndex"), 4L);
      if ((mTruncDbChoiceIndex < 0) || (mTruncDbChoiceIndex >= Enums::NumDbChoices))
      {  // corrupted Prefs?
         mTruncDbChoiceIndex = 4L;
      }
      mActionIndex = gPrefs->Read(base + wxT("ProcessChoice"), 0L);
      if ((mActionIndex < 0) || (mActionIndex > 1))
      {  // corrupted Prefs?
         mActionIndex = 0L;
      }
      gPrefs->Read(base + wxT("InitialAllowedSilence"), &mInitialAllowedSilence, 0.5);
      if ((mInitialAllowedSilence < 0.001) || (mInitialAllowedSilence > 10000.0))
      {  // corrupted Prefs?
         mInitialAllowedSilence = 0.5;
      }
      gPrefs->Read(base + wxT("LongestAllowedSilence"), &mTruncLongestAllowedSilence, 0.5);
      if ((mTruncLongestAllowedSilence < 0.0) || (mTruncLongestAllowedSilence > 10000.0))
      {  // corrupted Prefs?
         mTruncLongestAllowedSilence = 0.5;
      }
      gPrefs->Read(base + wxT("CompressPercent"), &mSilenceCompressPercent, 50.0);
      if ((mSilenceCompressPercent < 0.0) || (mSilenceCompressPercent > 100.0))
      {  // corrupted Prefs?
         mSilenceCompressPercent = 50.0;
      }

      SaveUserPreset(GetCurrentSettingsGroup());
   }

   // Do not migrate again
   gPrefs->Write(base + wxT("Migrated"), true);

   return true;
}

bool EffectTruncSilence::Process()
{
   // Typical fraction of total time taken by detection (better to guess low)
   const double detectFrac = .4;

   // Copy tracks
   CopyInputTracks(Track::All);

   // Lower bound on the amount of silence to find at a time -- this avoids
   // detecting silence repeatedly in low-frequency sounds.
   const double minTruncMs = 0.001;
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
            sampleCount(wxMax( mInitialAllowedSilence, minTruncMs) *
                  wt->GetRate());

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

      while (index < end)
      {
         // Show progress dialog, test for cancellation
         cancelled = TotalProgress(
               detectFrac * (whichTrack + index / (double)end) /
               (double)GetNumWaveTracks());
         if (cancelled)
         {
            break;
         }

         //
         // Optimization: if not in a silent region skip ahead to the next one
         //
         double curTime = wt->LongSamplesToTime(index);
         for ( ; rit != silences.end(); ++rit)
         {
            // Find the first silent region ending after current time
            if ((*rit)->end >= curTime)
            {
               break;
            }
         }

         if (rit == silences.end())
         {
            // No more regions -- no need to process the rest of the track
            break;
         }
         else if ((*rit)->start > curTime)
         {
            // End current silent region, skip ahead
            if (silentFrames >= minSilenceFrames) 
            {
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
         if ((index + count) > end) 
         {
            count = end - index;
         }

         // Fill buffer
         wt->Get((samplePtr)(buffer), floatSample, index, count);

         // Look for silences in current block
         for (sampleCount i = 0; i < count; ++i) 
         {
            if (fabs(buffer[i]) < truncDbSilenceThreshold) 
            {
               ++silentFrames;
            }
            else 
            {
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
   for (rit = silences.rbegin(); rit != silences.rend(); ++rit)
   {
      Region *r = *rit;

      // Progress dialog and cancellation. Do additional cleanup before return.
      if (TotalProgress(detectFrac + (1 - detectFrac) * whichReg / (double)silences.size()))
      {
         ReplaceProcessedTracks(false);
         return false;
      }

      // Intersection may create regions smaller than allowed; ignore them.
      // Allow one nanosecond extra for consistent results with exact milliseconds of allowed silence.
      if ((r->end - r->start) < (mInitialAllowedSilence - 0.000000001))
         continue;

      // Find new silence length as requested
      double inLength = r->end - r->start;
      double outLength;

      switch (mActionIndex)
      {
      case kTruncate:
         outLength = wxMin(mTruncLongestAllowedSilence, inLength);
         break;
      case kCompress:
         outLength = mInitialAllowedSilence +
                        (inLength - mInitialAllowedSilence) * mSilenceCompressPercent / 100.0;
         break;
      default: // Not currently used.
         outLength = wxMin(mInitialAllowedSilence +
                              (inLength - mInitialAllowedSilence) * mSilenceCompressPercent / 100.0,
                           mTruncLongestAllowedSilence);
      }

      double cutLen = inLength - outLength;
      totalCutLen += cutLen;

      TrackListIterator iterOut(mOutputTracks);
      for (Track *t = iterOut.First(); t; t = iterOut.Next())
      {
         // Don't waste time past the end of a track
         if (t->GetEndTime() < r->start)
         {
            continue;
         }

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
            if (wt->LongSamplesToTime(blendFrames) > inLength)
            {
               blendFrames = wt->TimeToLongSamples(inLength);
            }

            // Perform cross-fade in memory
            float *buf1 = new float[blendFrames];
            float *buf2 = new float[blendFrames];
            sampleCount t1 = wt->TimeToLongSamples(cutStart) - blendFrames / 2;
            sampleCount t2 = wt->TimeToLongSamples(cutEnd) - blendFrames / 2;

            wt->Get((samplePtr)buf1, floatSample, t1, blendFrames);
            wt->Get((samplePtr)buf2, floatSample, t2, blendFrames);

            for (sampleCount i = 0; i < blendFrames; ++i)
            {
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

void EffectTruncSilence::PopulateOrExchange(ShuttleGui & S)
{
   wxASSERT(kNumActions == WXSIZEOF(kActionStrings));

   wxArrayString actionChoices;
   for (int i = 0; i < kNumActions; i++)
   {
      actionChoices.Add(wxGetTranslation(kActionStrings[i]));
   }

   S.AddSpace(0, 5);

   S.StartStatic(_("Detect Silence"));
   {
      S.StartMultiColumn(3, wxALIGN_CENTER_HORIZONTAL);
      {
         // Threshold
         mTruncDbChoice = S.AddChoice(_("Level:"), wxT(""), &mDbChoices);
         mTruncDbChoice->SetValidator(wxGenericValidator(&mTruncDbChoiceIndex));
         S.SetSizeHints(-1, -1);
         S.AddSpace(0); // 'choices' already includes units.

         // Ignored silence
         FloatingPointValidator<double> vldDur(3, &mInitialAllowedSilence, NUM_VAL_NO_TRAILING_ZEROES);
         vldDur.SetRange(MIN_Minimum, MAX_Minimum);
         mInitialAllowedSilenceT = S.AddTextBox(_("Duration:"), wxT(""), 12);
         mInitialAllowedSilenceT->SetValidator(vldDur);
         S.AddUnits(wxT("seconds"));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Action"));
   {
      S.StartHorizontalLay();
      {
         // Action choices
         mActionChoice = S.AddChoice(wxT(""), wxT(""), &actionChoices);
         mActionChoice->SetValidator(wxGenericValidator(&mActionIndex));
         S.SetSizeHints(-1, -1);
      }
      S.EndHorizontalLay();
      S.StartMultiColumn(3, wxALIGN_CENTER_HORIZONTAL);
      {
         // Truncation / Compression factor

         FloatingPointValidator<double> vldTrunc(3, &mTruncLongestAllowedSilence, NUM_VAL_NO_TRAILING_ZEROES);
         vldTrunc.SetRange(MIN_Truncate, MAX_Truncate);
         mTruncLongestAllowedSilenceT = S.AddTextBox(_("Truncate to:"), wxT(""), 12);
         mTruncLongestAllowedSilenceT->SetValidator(vldTrunc);
         S.AddUnits(wxT("seconds"));

         FloatingPointValidator<double> vldComp(3, &mSilenceCompressPercent, NUM_VAL_NO_TRAILING_ZEROES);
         vldComp.SetRange(MIN_Compress, MAX_Compress);
         mSilenceCompressPercentT = S.AddTextBox(_("Compress to:"), wxT(""), 12);
         mSilenceCompressPercentT->SetValidator(vldComp);
         S.AddUnits(wxT("percent"));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   UpdateUI();
}

bool EffectTruncSilence::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   return true;
}

bool EffectTruncSilence::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

// EffectTruncSilence implementation

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
   if (srcIter == src.end())
   {
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
         while (curDest->end <= nsStart)
         {
            ++destIter;
            if (destIter == dest.end())
            {
               return;
            }
            curDest = *destIter;
         }

         // Check for splitting dest region in two
         if (nsStart > curDest->start && nsEnd < curDest->end)
         {
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
            if (nextIt == dest.end())
            {
               dest.Append(r);
            }
            else
            {
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
            {
               return;
            }
            curDest = *destIter;
         }

         // Check for all dest regions that need to be removed completely
         while (nsStart <= curDest->start && nsEnd >= curDest->end)
         {
            destIter = dest.erase(destIter);
            if (destIter == dest.end())
            {
               return;
            }
            curDest = *destIter;
         }

         // Check for truncating the beginning of dest region
         if (nsStart <= curDest->start &&
               nsEnd > curDest->start && nsEnd < curDest->end)
         {
            curDest->start = nsEnd;
         }
      }

      if (lastRun)
      {
         // done
         lastRun = false;
      }
      else
      {
         // Next non-silent region starts at the end of this silent region
         nsStart = curSrc->end;
         ++srcIter;
         if (srcIter == src.end())
         {
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
   for (int j = 0; j < blendFrameCount; ++j)
   {
      bufOutput[j] = (float)((bufBefore[j] * beforeFactor) + (bufAfter[j] * afterFactor));
      beforeFactor -= adjFactor;
      afterFactor  += adjFactor;
   }
}

void EffectTruncSilence::UpdateUI()
{
   switch (mActionIndex)
   {
   case kTruncate:
      mTruncLongestAllowedSilenceT->Enable(true);
      mSilenceCompressPercentT->Enable(false);
      break;
   case kCompress:
      mTruncLongestAllowedSilenceT->Enable(false);
      mSilenceCompressPercentT->Enable(true);
   }
}

void EffectTruncSilence::OnControlChange(wxCommandEvent & WXUNUSED(evt))
{
   mActionChoice->GetValidator()->TransferFromWindow();

   UpdateUI();

   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }
}
