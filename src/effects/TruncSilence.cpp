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
#include "TruncSilence.h"

#include <algorithm>
#include <list>
#include <limits>
#include <math.h>

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/valgen.h>

#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"

// Declaration of RegionList
class RegionList : public std::list < Region > {};

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
Param( Independent, bool,  XO("Independent"), false,     false,   true,                      1  );

static const size_t DEF_BlendFrameCount = 100;

// Lower bound on the amount of silence to find at a time -- this avoids
// detecting silence repeatedly in low-frequency sounds.
static const double DEF_MinTruncMs = 0.001; 

// Typical fraction of total time taken by detection (better to guess low)
const double detectFrac = 0.4;

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
   mbIndependent = DEF_Independent;

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
   parms.Write(KEY_Independent, mbIndependent);
   
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
   ReadAndVerifyBool(Independent);

   mInitialAllowedSilence = Minimum;
   mTruncLongestAllowedSilence = Truncate;
   mSilenceCompressPercent = Compress;
   mTruncDbChoiceIndex = DbIndex;
   mActionIndex = ActIndex;
   mbIndependent = Independent;

   // Readjust for 2.1.0 or before
   if (mActionIndex >= kNumActions)
   {
      mActionIndex -= kNumActions;
   }

   return true;
}

// Effect implementation

double EffectTruncSilence::CalcPreviewInputLength(double /* previewLength */)
{
   double inputLength = mT1 - mT0;
   double minInputLength = inputLength;

   // Master list of silent regions
   RegionList silences;

   // Start with the whole selection silent
   silences.push_back(Region(mT0, mT1));

   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   int whichTrack = 0;

   for (Track *t = iter.First(); t; t = iter.Next()) {
      WaveTrack *const wt = static_cast<WaveTrack *>(t);

      RegionList trackSilences;

      auto index = wt->TimeToLongSamples(mT0);
      sampleCount silentFrame = 0; // length of the current silence

      Analyze(silences, trackSilences, wt, &silentFrame, &index, whichTrack, &inputLength, &minInputLength);

      whichTrack++;
   }
   return inputLength;
}


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
   const bool success =
      mbIndependent
      ? ProcessIndependently()
      : ProcessAll();

   if (success)
      ReplaceProcessedTracks(true);

   return success;
}

bool EffectTruncSilence::ProcessIndependently()
{
   unsigned nGroups = 0;

   const bool syncLock = ::GetActiveProject()->IsSyncLocked();

   // Check if it's permissible
   {
      SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
      for (Track *track = iter.First(); track;
         track = iter.Next(true) // skip linked tracks
      ) {
         if (syncLock) {
            Track *const link = track->GetLink();
            SyncLockedTracksIterator syncIter(mTracks);
            for (Track *track2 = syncIter.StartWith(track); track2; track2 = syncIter.Next()) {
               if (track2->GetKind() == Track::Wave &&
                  !(track2 == track || track2 == link) &&
                  track2->GetSelected()) {
                  ::wxMessageBox(_("When truncating independently, there may only be one selected audio track in each Sync-Locked Track Group."));
                  return false;
               }
            }
         }

         ++nGroups;
      }
   }

   if (nGroups == 0)
      // nothing to do
      return true;

   // Now do the work

   // Copy tracks
   CopyInputTracks(Track::All);
   double newT1 = 0.0;

   {
      unsigned iGroup = 0;
      SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
      for (Track *track = iter.First(); track;
         ++iGroup, track = iter.Next(true) // skip linked tracks
      ) {
         Track *const link = track->GetLink();
         Track *const last = link ? link : track;

         RegionList silences;

         if (!FindSilences(silences, mOutputTracks.get(), track, last))
            return false;
         // Treat tracks in the sync lock group only
         Track *groupFirst, *groupLast;
         if (syncLock) {
            SyncLockedTracksIterator syncIter(mOutputTracks.get());
            groupFirst = syncIter.StartWith(track);
            groupLast = syncIter.Last();
         }
         else {
            groupFirst = track;
            groupLast = last;
         }
         double totalCutLen = 0.0;
         if (!DoRemoval(silences, iGroup, nGroups, groupFirst, groupLast, totalCutLen))
            return false;
         newT1 = std::max(newT1, mT1 - totalCutLen);
      }
   }

   mT1 = newT1;

   return true;
}

bool EffectTruncSilence::ProcessAll()
{
   // Copy tracks
   CopyInputTracks(Track::All);

   // Master list of silent regions.
   // This list should always be kept in order.
   RegionList silences;

   SelectedTrackListOfKindIterator iter(Track::Wave, mTracks);
   if (FindSilences(silences, mTracks, iter.First(), iter.Last())) {
      TrackListIterator iterOut(mOutputTracks.get());
      double totalCutLen = 0.0;
      Track *const first = iterOut.First();
      if (DoRemoval(silences, 0, 1, first, iterOut.Last(), totalCutLen)) {
         mT1 -= totalCutLen;
         return true;
      }
   }

   return false;
}

bool EffectTruncSilence::FindSilences
   (RegionList &silences, TrackList *list, Track *firstTrack, Track *lastTrack)
{
   // Start with the whole selection silent
   silences.push_back(Region(mT0, mT1));

   // Remove non-silent regions in each track
   SelectedTrackListOfKindIterator iter(Track::Wave, list);
   int whichTrack = 0;
   bool lastSeen = false;
   for (Track *t = iter.StartWith(firstTrack); !lastSeen && t; t = iter.Next())
   {
      lastSeen = (t == lastTrack);
      WaveTrack *const wt = static_cast<WaveTrack *>(t);

      // Smallest silent region to detect in frames
      auto minSilenceFrames =
         sampleCount(std::max(mInitialAllowedSilence, DEF_MinTruncMs) * wt->GetRate());

      //
      // Scan the track for silences
      //
      RegionList trackSilences;

      auto index = wt->TimeToLongSamples(mT0);
      sampleCount silentFrame = 0;

      // Detect silences
      bool cancelled = !(Analyze(silences, trackSilences, wt, &silentFrame, &index, whichTrack));

      // Buffer has been freed, so we're OK to return if cancelled
      if (cancelled)
      {
         ReplaceProcessedTracks(false);
         return false;
      }

      if (silentFrame >= minSilenceFrames)
      {
         // Track ended in silence -- record region
         trackSilences.push_back(Region(
            wt->LongSamplesToTime(index - silentFrame),
            wt->LongSamplesToTime(index)
         ));
      }

      // Intersect with the overall silent region list
      Intersect(silences, trackSilences);
      whichTrack++;
   }

   return true;
}

bool EffectTruncSilence::DoRemoval
(const RegionList &silences, unsigned iGroup, unsigned nGroups, Track *firstTrack, Track *lastTrack,
 double &totalCutLen)
{
   //
   // Now remove the silent regions from all selected / sync-lock selected tracks.
   //

   // Loop over detected regions in reverse (so cuts don't change time values
   // down the line)
   int whichReg = 0;
   RegionList::const_reverse_iterator rit;
   for (rit = silences.rbegin(); rit != silences.rend(); ++rit)
   {
      const Region &region = *rit;
      const Region *const r = &region;

      // Progress dialog and cancellation. Do additional cleanup before return.
      const double frac = detectFrac +
         (1 - detectFrac) * (iGroup + whichReg / double(silences.size())) / nGroups;
      if (TotalProgress(frac))
      {
         ReplaceProcessedTracks(false);
         return false;
      }

      // Intersection may create regions smaller than allowed; ignore them.
      // Allow one nanosecond extra for consistent results with exact milliseconds of allowed silence.
      if ((r->end - r->start) < (mInitialAllowedSilence - 0.000000001))
         continue;

      // Find NEW silence length as requested
      double inLength = r->end - r->start;
      double outLength;

      switch (mActionIndex)
      {
      case kTruncate:
         outLength = std::min(mTruncLongestAllowedSilence, inLength);
         break;
      case kCompress:
         outLength = mInitialAllowedSilence +
                        (inLength - mInitialAllowedSilence) * mSilenceCompressPercent / 100.0;
         break;
      default: // Not currently used.
         outLength = std::min(mInitialAllowedSilence +
                              (inLength - mInitialAllowedSilence) * mSilenceCompressPercent / 100.0,
                           mTruncLongestAllowedSilence);
      }

      double cutLen = inLength - outLength;
      totalCutLen += cutLen;

      TrackListIterator iterOut(mOutputTracks.get());
      bool lastSeen = false;
      for (Track *t = iterOut.StartWith(firstTrack); t && !lastSeen; t = iterOut.Next())
      {
         lastSeen = (t == lastTrack);
         if (!(t->GetSelected() || t->IsSyncLockSelected()))
            continue;

         // Don't waste time past the end of a track
         if (t->GetEndTime() < r->start)
            continue;

         double cutStart = (r->start + r->end - cutLen) / 2;
         double cutEnd = cutStart + cutLen;
         if (t->GetKind() == Track::Wave)
         {
            // In WaveTracks, clear with a cross-fade
            WaveTrack *const wt = static_cast<WaveTrack*>(t);
            auto blendFrames = mBlendFrameCount;
            // Round start/end times to frame boundaries
            cutStart = wt->LongSamplesToTime(wt->TimeToLongSamples(cutStart));
            cutEnd = wt->LongSamplesToTime(wt->TimeToLongSamples(cutEnd));

            // Make sure the cross-fade does not affect non-silent frames
            if (wt->LongSamplesToTime(blendFrames) > inLength)
            {
               // Result is not more than blendFrames:
               blendFrames = wt->TimeToLongSamples(inLength).as_size_t();
            }

            // Perform cross-fade in memory
            float *buf1 = new float[blendFrames];
            float *buf2 = new float[blendFrames];
            auto t1 = wt->TimeToLongSamples(cutStart) - blendFrames / 2;
            auto t2 = wt->TimeToLongSamples(cutEnd) - blendFrames / 2;

            wt->Get((samplePtr)buf1, floatSample, t1, blendFrames);
            wt->Get((samplePtr)buf2, floatSample, t2, blendFrames);

            for (decltype(blendFrames) i = 0; i < blendFrames; ++i)
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
         else
            // Non-wave tracks: just do a sync-lock adjust
            t->SyncLockAdjust(cutEnd, cutStart);
      }
      ++whichReg;
   }

   return true;
}

bool EffectTruncSilence::Analyze(RegionList& silenceList,
                                 RegionList& trackSilences,
                                 WaveTrack* wt,
                                 sampleCount* silentFrame,
                                 sampleCount* index,
                                 int whichTrack,
                                 double* inputLength /*= NULL*/,
                                 double* minInputLength /*= NULL*/)
{
   // Smallest silent region to detect in frames
   auto minSilenceFrames = sampleCount(std::max( mInitialAllowedSilence, DEF_MinTruncMs) * wt->GetRate());

   double truncDbSilenceThreshold = Enums::Db2Signal[mTruncDbChoiceIndex];
   auto blockLen = wt->GetMaxBlockSize();
   auto start = wt->TimeToLongSamples(mT0);
   auto end = wt->TimeToLongSamples(mT1);
   sampleCount outLength = 0;

   double previewLength;
   gPrefs->Read(wxT("/AudioIO/EffectsPreviewLen"), &previewLength, 6.0);
   // Minimum required length in samples.
   const sampleCount previewLen( previewLength * wt->GetRate() );

   // Keep position in overall silences list for optimization
   RegionList::iterator rit(silenceList.begin());

   // Allocate buffer
   float *buffer = new float[blockLen];

   // Loop through current track
   while (*index < end) {
      if (inputLength && ((outLength >= previewLen) || (*index - start > wt->TimeToLongSamples(*minInputLength)))) {
         *inputLength = std::min<double>(*inputLength, *minInputLength);
         if (outLength >= previewLen) {
            *minInputLength = *inputLength;
         }
         return true;
      }

      if (!inputLength) {
         // Show progress dialog, test for cancellation
         bool cancelled = TotalProgress(
               detectFrac * (whichTrack +
                             (*index - start).as_double() /
                             (end - start).as_double()) /
                             (double)GetNumWaveTracks());
         if (cancelled) {
            delete [] buffer;
            return false;
         }
      }

      // Optimization: if not in a silent region skip ahead to the next one

      double curTime = wt->LongSamplesToTime(*index);
      for ( ; rit != silenceList.end(); ++rit) {
         // Find the first silent region ending after current time
         if (rit->end >= curTime) {
            break;
         }
      }

      if (rit == silenceList.end()) {
         // No more regions -- no need to process the rest of the track
         if (inputLength) {
            // Add available samples up to previewLength.
            auto remainingTrackSamples = wt->TimeToLongSamples(wt->GetEndTime()) - *index;
            auto requiredTrackSamples = previewLen - outLength;
            outLength += (remainingTrackSamples > requiredTrackSamples)? requiredTrackSamples : remainingTrackSamples;
         }

         break;
      }
      else if (rit->start > curTime) {
         // End current silent region, skip ahead
         if (*silentFrame >= minSilenceFrames)  {
            trackSilences.push_back(Region(
               wt->LongSamplesToTime(*index - *silentFrame),
               wt->LongSamplesToTime(*index)
            ));
         }
         *silentFrame = 0;
         auto newIndex = wt->TimeToLongSamples(rit->start);
         if (inputLength) {
            auto requiredTrackSamples = previewLen - outLength;
            // Add non-silent sample to outLength
            outLength += ((newIndex - *index) > requiredTrackSamples)? requiredTrackSamples : newIndex - *index;
         }

         *index = newIndex;
      }
      // End of optimization

      // Limit size of current block if we've reached the end
      auto count = limitSampleBufferSize( blockLen, end - *index );

      // Fill buffer
      wt->Get((samplePtr)(buffer), floatSample, *index, count);

      // Look for silenceList in current block
      for (decltype(count) i = 0; i < count; ++i) {
         if (inputLength && ((outLength >= previewLen) || (outLength > wt->TimeToLongSamples(*minInputLength)))) {
            *inputLength = wt->LongSamplesToTime(*index + i) - wt->LongSamplesToTime(start);
            break;
         }

         if (fabs(buffer[i]) < truncDbSilenceThreshold) {
            (*silentFrame)++;
         }
         else {
            sampleCount allowed = 0;
            if (*silentFrame >= minSilenceFrames) {
               if (inputLength) {
                  switch (mActionIndex) {
                     case kTruncate:
                        outLength += wt->TimeToLongSamples(mTruncLongestAllowedSilence);
                        break;
                     case kCompress:
                        allowed = wt->TimeToLongSamples(mInitialAllowedSilence);
                        outLength += sampleCount(
                           allowed.as_double() +
                              (*silentFrame - allowed).as_double()
                                 * mSilenceCompressPercent / 100.0
                        );
                        break;
                     // default: // Not currently used.
                  }
               }

               // Record the silent region
               trackSilences.push_back(Region(
                  wt->LongSamplesToTime(*index + i - *silentFrame),
                  wt->LongSamplesToTime(*index + i)
               ));
            }
            else if (inputLength) {   // included as part of non-silence
               outLength += *silentFrame;
            }
            *silentFrame = 0;
            if (inputLength) {
                ++outLength;   // Add non-silent sample to outLength
            }
         }
      }
      // Next block
      *index += count;
   }
   delete [] buffer;

   if (inputLength) {
      *inputLength = std::min<double>(*inputLength, *minInputLength);
      if (outLength >= previewLen) {
         *minInputLength = *inputLength;
      }
   }

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

      S.StartMultiColumn(2, wxALIGN_CENTER_HORIZONTAL);
      {
         mIndependent = S.AddCheckBox(_("Truncate tracks independently"),
            mbIndependent ? wxT("true") : wxT("false"));
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

   mbIndependent = mIndependent->IsChecked();

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
   RegionList::iterator curDest = destIter;

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
      RegionList::const_iterator curSrc;

      if (lastRun)
      {
         // The last non-silent region extends as far as possible
         nsEnd = std::numeric_limits<double>::max();
      }
      else
      {
         curSrc = srcIter;
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
            curDest = destIter;
         }

         // Check for splitting dest region in two
         if (nsStart > curDest->start && nsEnd < curDest->end)
         {
            // The second region
            Region r(nsEnd, curDest->end);

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
               dest.push_back(r);
            else
               dest.insert(nextIt, r);
            ++destIter;          // (now points at the newly-inserted region)

            curDest = destIter;
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
            curDest = destIter;
         }

         // Check for all dest regions that need to be removed completely
         while (nsStart <= curDest->start && nsEnd >= curDest->end)
         {
            destIter = dest.erase(destIter);
            if (destIter == dest.end())
            {
               return;
            }
            curDest = destIter;
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

/*
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
*/

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
