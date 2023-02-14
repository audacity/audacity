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


#include "TruncSilence.h"
#include "LoadEffects.h"

#include <algorithm>
#include <list>
#include <limits>
#include <math.h>

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/valgen.h>

#include "Prefs.h"
#include "Project.h"
#include "../ProjectSettings.h"
#include "ShuttleGui.h"
#include "../SyncLock.h"
#include "WaveTrack.h"
#include "valnum.h"
#include "../widgets/AudacityMessageBox.h"

class Enums {
public:
   static const size_t    NumDbChoices;
   static const EnumValueSymbol DbChoices[];
};

const EnumValueSymbol Enums::DbChoices[] = {
   // Table of text values, only for reading what was stored in legacy config
   // files.
   // It was inappropriate to make this a discrete choice control.
   { wxT("-20 dB") },
   { wxT("-25 dB") },
   { wxT("-30 dB") },
   { wxT("-35 dB") },
   { wxT("-40 dB") },
   { wxT("-45 dB") },
   { wxT("-50 dB") },
   { wxT("-55 dB") },
   { wxT("-60 dB") },
   { wxT("-65 dB") },
   { wxT("-70 dB") },
   { wxT("-75 dB") },
   { wxT("-80 dB") }
};

// Map from position in table above to numerical value.
static inline double enumToDB( int val ) { return -( 5.0 * val + 20.0 ); }

const size_t Enums::NumDbChoices = WXSIZEOF(Enums::DbChoices);

using Region = WaveTrack::Region;

// Declaration of RegionList
class RegionList : public std::list < Region > {};

const EnumValueSymbol EffectTruncSilence::kActionStrings[nActions] =
{
   { XO("Truncate Detected Silence") },
   { XO("Compress Excess Silence") }
};

static CommandParameters::ObsoleteMap kObsoleteActions[] = {
   // Compatible with 2.1.0 and before
   { wxT("0"), 0 }, // Remap to Truncate Detected Silence
   { wxT("1"), 1 }, // Remap to Compress Excess Silence
};

static const size_t nObsoleteActions = WXSIZEOF( kObsoleteActions );

const EffectParameterMethods& EffectTruncSilence::Parameters() const
{
   static CapturedParameters<EffectTruncSilence,
      Threshold, ActIndex, Minimum, Truncate, Compress, Independent
   > parameters;
   return parameters;
}

static const size_t DEF_BlendFrameCount = 100;

// Lower bound on the amount of silence to find at a time -- this avoids
// detecting silence repeatedly in low-frequency sounds.
static const double DEF_MinTruncMs = 0.001; 

// Typical fraction of total time taken by detection (better to guess low)
const double detectFrac = 0.4;

const ComponentInterfaceSymbol EffectTruncSilence::Symbol
{ XO("Truncate Silence") };

namespace{ BuiltinEffectsModule::Registration< EffectTruncSilence > reg; }

BEGIN_EVENT_TABLE(EffectTruncSilence, wxEvtHandler)
   EVT_CHOICE(wxID_ANY, EffectTruncSilence::OnControlChange)
   EVT_TEXT(wxID_ANY, EffectTruncSilence::OnControlChange)
END_EVENT_TABLE()

EffectTruncSilence::EffectTruncSilence()
{
   Parameters().Reset(*this);

   SetLinearEffectFlag(false);

   // This used to be changeable via the audacity.cfg/registry.  Doubtful that was
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

// ComponentInterface implementation

ComponentInterfaceSymbol EffectTruncSilence::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectTruncSilence::GetDescription() const
{
   return XO("Automatically reduces the length of passages where the volume is below a specified level");
}

ManualPageID EffectTruncSilence::ManualPage() const
{
   return L"Truncate_Silence";
}

// EffectDefinitionInterface implementation

EffectType EffectTruncSilence::GetType() const
{
   return EffectTypeProcess;
}

bool EffectTruncSilence::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   Effect::LoadSettings(parms, settings);

   // A bit of special treatment for two parameters

   // This control migrated from a choice to a text box in version 2.3.0
   double myThreshold {};
   bool newParams = [&] {
      double temp;
      if (!parms.ReadAndVerify(Threshold.key,
         &temp, Threshold.def, Threshold.min, Threshold.max))
         return false;
      myThreshold = temp;
      return true;
   } ();

   if ( !newParams ) {
      int temp;
      // Use legacy param:
      if (!parms.ReadAndVerify(L"Db", &temp, 0,
         Enums::DbChoices, Enums::NumDbChoices))
         return false;
      myThreshold = enumToDB( temp );
   }

   {
      int temp;
      if (!parms.ReadAndVerify( ActIndex.key, &temp, ActIndex.def,
         kActionStrings, nActions, kObsoleteActions, nObsoleteActions))
         return false;
   
      // TODO:  fix this when settings are really externalized
      const_cast<int&>(mActionIndex) = temp;
   }
   // TODO:  fix this when settings are really externalized
   const_cast<double&>(mThresholdDB) = myThreshold;
   return true;
}

// Effect implementation

double EffectTruncSilence::CalcPreviewInputLength(
   const EffectSettings &, double /* previewLength */) const
{
   double inputLength = mT1 - mT0;
   double minInputLength = inputLength;

   // Master list of silent regions
   RegionList silences;

   // Start with the whole selection silent
   silences.push_back(Region(mT0, mT1));

   int whichTrack = 0;

   for (auto wt : inputTracks()->Selected< const WaveTrack >()) {
      RegionList trackSilences;

      auto index = wt->TimeToLongSamples(mT0);
      sampleCount silentFrame = 0; // length of the current silence

      Analyze(silences, trackSilences, wt, &silentFrame, &index, whichTrack, &inputLength, &minInputLength);

      whichTrack++;
   }
   return inputLength;
}


bool EffectTruncSilence::Process(EffectInstance &, EffectSettings &)
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

   const auto &settings = ProjectSettings::Get( *FindProject() );
   const bool syncLock = settings.IsSyncLocked();

   // Check if it's permissible
   {
      for (auto track : inputTracks()->SelectedLeaders< const WaveTrack >() ) {
         if (syncLock) {
            auto channels = TrackList::Channels(track);
            auto otherTracks =
               SyncLock::Group(track).Filter<const WaveTrack>()
                  + &Track::IsSelected
                  - [&](const Track *pTrack){
                        return channels.contains(pTrack); };
            if (otherTracks) {
               ::Effect::MessageBox(
                  XO(
"When truncating independently, there may only be one selected audio track in each Sync-Locked Track Group.") );
               return false;
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
   CopyInputTracks(true);
   double newT1 = 0.0;

   {
      unsigned iGroup = 0;
      for (auto track : mOutputTracks->SelectedLeaders< WaveTrack >() ) {
         Track *const last = *TrackList::Channels(track).rbegin();

         RegionList silences;

         if (!FindSilences(silences, mOutputTracks.get(), track, last))
            return false;
         // Treat tracks in the sync lock group only
         Track *groupFirst, *groupLast;
         if (syncLock) {
            auto trackRange = SyncLock::Group(track);
            groupFirst = *trackRange.begin();
            groupLast = *trackRange.rbegin();
         }
         else {
            groupFirst = track;
            groupLast = last;
         }
         double totalCutLen = 0.0;
         if (!DoRemoval(silences, iGroup, nGroups, groupFirst, groupLast, totalCutLen))
            return false;
         newT1 = std::max(newT1, mT1 - totalCutLen);

         ++iGroup;
      }
   }

   mT1 = newT1;

   return true;
}

bool EffectTruncSilence::ProcessAll()
{
   // Copy tracks
   CopyInputTracks(true);

   // Master list of silent regions.
   // This list should always be kept in order.
   RegionList silences;

   auto trackRange0 = inputTracks()->Selected< const WaveTrack >();
   if (FindSilences(
         silences, inputTracks(), *trackRange0.begin(), *trackRange0.rbegin())) {
      auto trackRange = mOutputTracks->Any();
      double totalCutLen = 0.0;
      if (DoRemoval(silences, 0, 1,
         *trackRange.begin(), *trackRange.rbegin(), totalCutLen)) {
         mT1 -= totalCutLen;
         return true;
      }
   }

   return false;
}

bool EffectTruncSilence::FindSilences
   (RegionList &silences, const TrackList *list,
    const Track *firstTrack, const Track *lastTrack)
{
   // Start with the whole selection silent
   silences.push_back(Region(mT0, mT1));

   // Remove non-silent regions in each track
   int whichTrack = 0;
   for (auto wt :
           list->Selected< const WaveTrack >()
               .StartingWith( firstTrack ).EndingAfter( lastTrack ) )
   {
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

      const double cutLen = std::max(0.0, inLength - outLength);
      // Don't waste time cutting nothing.
      if( cutLen == 0.0 )
         continue;
      
      totalCutLen += cutLen;

      double cutStart = (r->start + r->end - cutLen) / 2;
      double cutEnd = cutStart + cutLen;
      (mOutputTracks->Any()
         .StartingWith(firstTrack).EndingAfter(lastTrack)
         + &SyncLock::IsSelectedOrSyncLockSelected
         - [&](const Track *pTrack) { return
           // Don't waste time past the end of a track
           pTrack->GetEndTime() < r->start;
         }
      ).Visit(
         [&](WaveTrack *wt) {

            // In WaveTracks, clear with a cross-fade
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
            Floats buf1{ blendFrames };
            Floats buf2{ blendFrames };
            auto t1 = wt->TimeToLongSamples(cutStart) - blendFrames / 2;
            auto t2 = wt->TimeToLongSamples(cutEnd) - blendFrames / 2;

            wt->GetFloats(buf1.get(), t1, blendFrames);
            wt->GetFloats(buf2.get(), t2, blendFrames);

            for (decltype(blendFrames) i = 0; i < blendFrames; ++i)
            {
               buf1[i] = ((blendFrames-i) * buf1[i] + i * buf2[i]) /
                         (double)blendFrames;
            }

            // Perform the cut
            wt->Clear(cutStart, cutEnd);

            // Write cross-faded data
            wt->Set((samplePtr)buf1.get(), floatSample, t1, blendFrames,
               // This effect mostly shifts samples to remove silences, and
               // does only a little bit of floating point calculations to
               // cross-fade the splices, over a 100 sample interval by default.
               // Don't dither.
               narrowestSampleFormat);
         },
         [&](Track *t) {
            // Non-wave tracks: just do a sync-lock adjust
            t->SyncLockAdjust(cutEnd, cutStart);
         }
      );
      ++whichReg;
   }

   return true;
}

bool EffectTruncSilence::Analyze(RegionList& silenceList,
                                 RegionList& trackSilences,
                                 const WaveTrack *wt,
                                 sampleCount* silentFrame,
                                 sampleCount* index,
                                 int whichTrack,
                                 double* inputLength /*= NULL*/,
                                 double* minInputLength /*= NULL*/) const
{
   // Smallest silent region to detect in frames
   auto minSilenceFrames = sampleCount(std::max( mInitialAllowedSilence, DEF_MinTruncMs) * wt->GetRate());

   double truncDbSilenceThreshold = DB_TO_LINEAR( mThresholdDB );
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
   Floats buffer{ blockLen };

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
         if (cancelled)
            return false;
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
      wt->GetFloats((buffer.get()), *index, count);

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

   if (inputLength) {
      *inputLength = std::min<double>(*inputLength, *minInputLength);
      if (outLength >= previewLen) {
         *minInputLength = *inputLength;
      }
   }

   return true;
}


std::unique_ptr<EffectUIValidator> EffectTruncSilence::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
   wxASSERT(nActions == WXSIZEOF(kActionStrings));

   S.AddSpace(0, 5);

   S.StartStatic(XO("Detect Silence"));
   {
      S.StartMultiColumn(3, wxALIGN_CENTER_HORIZONTAL);
      {
         // Threshold
         mThresholdText = S
            .Validator<FloatingPointValidator<double>>(
               3, &mThresholdDB, NumValidatorStyle::NO_TRAILING_ZEROES,
               Threshold.min, Threshold.max )
            .NameSuffix(XO("db"))
            .AddTextBox(XXO("&Threshold:"), wxT(""), 0);
         S.AddUnits(XO("dB"));

         // Ignored silence
         mInitialAllowedSilenceT = S.Validator<FloatingPointValidator<double>>(
               3, &mInitialAllowedSilence,
               NumValidatorStyle::NO_TRAILING_ZEROES,
               Minimum.min, Minimum.max)
            .NameSuffix(XO("seconds"))
            .AddTextBox(XXO("&Duration:"), wxT(""), 12);
         S.AddUnits(XO("seconds"));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(XO("Action"));
   {
      S.StartHorizontalLay();
      {
         // Action choices
         auto actionChoices = Msgids( kActionStrings, nActions );
         mActionChoice = S
            .Validator<wxGenericValidator>(&mActionIndex)
            .MinSize( { -1, -1 } )
            .AddChoice( {}, actionChoices );
      }
      S.EndHorizontalLay();
      S.StartMultiColumn(3, wxALIGN_CENTER_HORIZONTAL);
      {
         // Truncation / Compression factor

         mTruncLongestAllowedSilenceT = S.Validator<FloatingPointValidator<double>>(
               3, &mTruncLongestAllowedSilence,
               NumValidatorStyle::NO_TRAILING_ZEROES,
               Truncate.min, Truncate.max )
            .NameSuffix(XO("seconds"))
            .AddTextBox(XXO("Tr&uncate to:"), wxT(""), 12);
         S.AddUnits(XO("seconds"));

         mSilenceCompressPercentT = S.Validator<FloatingPointValidator<double>>(
               3, &mSilenceCompressPercent,
               NumValidatorStyle::NO_TRAILING_ZEROES,
               Compress.min, Compress.max )
            .NameSuffix(XO("%"))
            .AddTextBox(XXO("C&ompress to:"), wxT(""), 12);
         S.AddUnits(XO("%"));
      }
      S.EndMultiColumn();

      S.StartMultiColumn(2, wxALIGN_CENTER_HORIZONTAL);
      {
         mIndependent = S.AddCheckBox(XXO("Trunc&ate tracks independently"),
            mbIndependent);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   UpdateUI();
   return nullptr;
}

bool EffectTruncSilence::TransferDataToWindow(const EffectSettings &)
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   return true;
}

bool EffectTruncSilence::TransferDataFromWindow(EffectSettings &)
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

   if (!EffectUIValidator::EnableApply(
      mUIParent, mUIParent->TransferDataFromWindow()))
   {
      return;
   }
}

bool EffectTruncSilence::NeedsDither() const
{
   return false;
}
