/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.cpp

  Dominic Mazzoni

*******************************************************************//**

\class WaveTrack
\brief A Track that contains audio waveform data.

*//****************************************************************//**

\class WaveTrack::Location
\brief Used only by WaveTrack, a special way to hold location that
can accommodate merged regions.

*//****************************************************************//**

\class TrackFactory
\brief Used to create a WaveTrack, or a LabelTrack..  Implementation
of the functions of this class are dispersed through the different
Track classes.

*//*******************************************************************/


#include "WaveTrack.h"
#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/debug.h>

#include <float.h>
#include <math.h>
#include <algorithm>
#include "MemoryX.h"

#include "float_cast.h"

#include "Envelope.h"
#include "Sequence.h"
#include "Spectrum.h"

#include "Project.h"
#include "Internat.h"

#include "AudioIO.h"
#include "Prefs.h"

#include "ondemand/ODManager.h"

#include "effects/TimeWarper.h"
#include "prefs/SpectrumPrefs.h"
#include "prefs/WaveformPrefs.h"

#include "Experimental.h"

using std::max;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
bool WaveTrack::mMonoAsVirtualStereo;
#endif

WaveTrack::Holder TrackFactory::DuplicateWaveTrack(WaveTrack &orig)
{
   return std::unique_ptr<WaveTrack>
   { static_cast<WaveTrack*>(orig.Duplicate().release()) };
}


WaveTrack::Holder TrackFactory::NewWaveTrack(sampleFormat format, double rate)
{
   return std::unique_ptr<WaveTrack>
   { safenew WaveTrack(mDirManager, format, rate) };
}

WaveTrack::WaveTrack(const std::shared_ptr<DirManager> &projDirManager, sampleFormat format, double rate) :
   Track(projDirManager)
{
   if (format == (sampleFormat)0)
   {
      format = GetActiveProject()->GetDefaultFormat();
   }
   if (rate == 0)
   {
      rate = GetActiveProject()->GetRate();
   }

   // Force creation always:
   WaveformSettings &settings = GetIndependentWaveformSettings();

   mDisplay = FindDefaultViewMode();
   if (mDisplay == obsoleteWaveformDBDisplay) {
      mDisplay = Waveform;
      settings.scaleType = WaveformSettings::stLogarithmic;
   }

   mLegacyProjectFileOffset = 0;

   mFormat = format;
   mRate = (int) rate;
   mGain = 1.0;
   mPan = 0.0;
   SetDefaultName(gPrefs->Read(wxT("/GUI/TrackNames/DefaultTrackName"), _("Audio Track")));
   SetName(GetDefaultName());
   mDisplayMin = -1.0;
   mDisplayMax = 1.0;
   mSpectrumMin = mSpectrumMax = -1; // so values will default to settings
   mLastScaleType = -1;
   mLastdBRange = -1;
   mAutoSaveIdent = 0;
}

WaveTrack::WaveTrack(const WaveTrack &orig):
   Track(orig)
   , mpSpectrumSettings(orig.mpSpectrumSettings
      ? std::make_unique<SpectrogramSettings>(*orig.mpSpectrumSettings)
      : nullptr
   )
   , mpWaveformSettings(orig.mpWaveformSettings 
      ? std::make_unique<WaveformSettings>(*orig.mpWaveformSettings)
      : nullptr
   )
{
   mLastScaleType = -1;
   mLastdBRange = -1;

   mLegacyProjectFileOffset = 0;

   Init(orig);

   for (const auto &clip : orig.mClips)
      mClips.push_back(make_movable<WaveClip>(*clip, mDirManager));
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack &orig)
{
   Track::Init(orig);
   mFormat = orig.mFormat;
   mRate = orig.mRate;
   mGain = orig.mGain;
   mPan = orig.mPan;
   SetDefaultName(orig.GetDefaultName());
   SetName(orig.GetName());
   mDisplay = orig.mDisplay;
   mDisplayMin = orig.mDisplayMin;
   mDisplayMax = orig.mDisplayMax;
   mSpectrumMin = orig.mSpectrumMin;
   mSpectrumMax = orig.mSpectrumMax;
   mDisplayLocationsCache.clear();
}

void WaveTrack::Merge(const Track &orig)
{
   if (orig.GetKind() == Wave)
   {
      const WaveTrack &wt = static_cast<const WaveTrack&>(orig);
      mDisplay = wt.mDisplay;
      mGain    = wt.mGain;
      mPan     = wt.mPan;
      mDisplayMin = wt.mDisplayMin;
      mDisplayMax = wt.mDisplayMax;
      SetSpectrogramSettings(wt.mpSpectrumSettings
         ? std::make_unique<SpectrogramSettings>(*wt.mpSpectrumSettings) : nullptr);
      SetWaveformSettings
         (wt.mpWaveformSettings ? std::make_unique<WaveformSettings>(*wt.mpWaveformSettings) : nullptr);
   }
   Track::Merge(orig);
}

WaveTrack::~WaveTrack()
{
   //Let the ODManager know this WaveTrack is disappearing.
   //Deschedules tasks associated with this track.
   if(ODManager::IsInstanceCreated())
      ODManager::Instance()->RemoveWaveTrack(this);
}

double WaveTrack::GetOffset() const
{
   return GetStartTime();
}

void WaveTrack::SetOffset(double o)
{
   double delta = o - GetOffset();

   for (const auto &clip : mClips)
      clip->SetOffset(clip->GetOffset() + delta);

   mOffset = o;
}

//static
WaveTrack::WaveTrackDisplay WaveTrack::FindDefaultViewMode()
{
   // PRL:  Bugs 1043, 1044
   // 2.1.1 writes a NEW key for this preference, which got NEW values,
   // to avoid confusing version 2.1.0 if it reads the preference file afterwards.
   // Prefer the NEW preference key if it is present

   WaveTrack::WaveTrackDisplay viewMode;
   gPrefs->Read(wxT("/GUI/DefaultViewModeNew"), (int*)&viewMode, -1);

   // Default to the old key only if not, default the value if it's not there either
   wxASSERT(WaveTrack::MinDisplay >= 0);
   if (viewMode < 0) {
      int oldMode;
      gPrefs->Read(wxT("/GUI/DefaultViewMode"), &oldMode,
         (int)(WaveTrack::Waveform));
      viewMode = WaveTrack::ConvertLegacyDisplayValue(oldMode);
   }

   // Now future-proof 2.1.1 against a recurrence of this sort of bug!
   viewMode = WaveTrack::ValidateWaveTrackDisplay(viewMode);

   return viewMode;
}

// static
WaveTrack::WaveTrackDisplay
WaveTrack::ConvertLegacyDisplayValue(int oldValue)
{
   // Remap old values.
   enum OldValues {
      Waveform,
      WaveformDB,
      Spectrogram,
      SpectrogramLogF,
      Pitch,
   };

   WaveTrackDisplay newValue;
   switch (oldValue) {
   default:
   case Waveform:
      newValue = WaveTrack::Waveform; break;
   case WaveformDB:
      newValue = WaveTrack::obsoleteWaveformDBDisplay; break;
   case Spectrogram:
   case SpectrogramLogF:
   case Pitch:
      newValue = WaveTrack::Spectrum; break;
      /*
   case SpectrogramLogF:
      newValue = WaveTrack::SpectrumLogDisplay; break;
   case Pitch:
      newValue = WaveTrack::PitchDisplay; break;
      */
   }
   return newValue;
}

// static
WaveTrack::WaveTrackDisplay
WaveTrack::ValidateWaveTrackDisplay(WaveTrackDisplay display)
{
   switch (display) {
      // non-obsolete codes
   case Waveform:
   case obsoleteWaveformDBDisplay:
   case Spectrum:
      return display;

      // obsolete codes
   case obsolete1: // was SpectrumLogDisplay
   case obsolete2: // was SpectralSelectionDisplay
   case obsolete3: // was SpectralSelectionLogDisplay
   case obsolete4: // was PitchDisplay
      return Spectrum;

      // codes out of bounds (from future prefs files?)
   default:
      return MinDisplay;
   }
}

void WaveTrack::SetLastScaleType() const
{
   mLastScaleType = GetWaveformSettings().scaleType;
}

void WaveTrack::SetLastdBRange() const
{
   mLastdBRange = GetWaveformSettings().dBRange;
}

void WaveTrack::GetDisplayBounds(float *min, float *max) const
{
   *min = mDisplayMin;
   *max = mDisplayMax;
}

void WaveTrack::SetDisplayBounds(float min, float max) const
{
   mDisplayMin = min;
   mDisplayMax = max;
}

void WaveTrack::GetSpectrumBounds(float *min, float *max) const
{
   const double rate = GetRate();

   const SpectrogramSettings &settings = GetSpectrogramSettings();
   const SpectrogramSettings::ScaleType type = settings.scaleType;

   const float top = (rate / 2.);

   float bottom;
   if (type == SpectrogramSettings::stLinear)
      bottom = 0.0f;
   else if (type == SpectrogramSettings::stPeriod) {
      // special case
      const auto half = settings.GetFFTLength() / 2;
      // EAC returns no data for below this frequency:
      const float bin2 = rate / half;
      bottom = bin2;
   }
   else
      // logarithmic, etc.
      bottom = 1.0f;

   {
      float spectrumMax = mSpectrumMax;
      if (spectrumMax < 0)
         spectrumMax = settings.maxFreq;
      if (spectrumMax < 0)
         *max = top;
      else
         *max = std::max(bottom, std::min(top, spectrumMax));
   }

   {
      float spectrumMin = mSpectrumMin;
      if (spectrumMin < 0)
         spectrumMin = settings.minFreq;
      if (spectrumMin < 0)
         *min = std::max(bottom, top / 1000.0f);
      else
         *min = std::max(bottom, std::min(top, spectrumMin));
   }
}

void WaveTrack::SetSpectrumBounds(float min, float max) const
{
   mSpectrumMin = min;
   mSpectrumMax = max;
}

Track::Holder WaveTrack::Duplicate() const
{
   return Track::Holder{ safenew WaveTrack{ *this } };
}

double WaveTrack::GetRate() const
{
   return mRate;
}

void WaveTrack::SetRate(double newRate)
{
   mRate = (int) newRate;
   for (const auto &clip : mClips)
      clip->SetRate((int)newRate);
}

float WaveTrack::GetGain() const
{
   return mGain;
}

void WaveTrack::SetGain(float newGain)
{
   mGain = newGain;
}

float WaveTrack::GetPan() const
{
   return mPan;
}

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
bool WaveTrack::SetPan(float newPan)
{
   float p=mPan;
   bool panZero=false;
   int temp;

   if (newPan > 1.0)
      mPan = 1.0;
   else if (newPan < -1.0)
      mPan = -1.0;
   else
      mPan = newPan;

   if(mDisplay == WaveTrack::WaveformDisplay && mChannel == Track::MonoChannel && (p == 0.0f && newPan != 0.0f || p != 0.0f && newPan == 0.0f) && mMonoAsVirtualStereo)
   {
      panZero=true;
      if(!mPan){
         mHeight = mHeight + mHeightv;
      }else{
         temp = mHeight;
         mHeight = temp*mPerY;
         mHeightv = temp - mHeight;
      }
      ReorderList();
   }

   return panZero;
}

#else // EXPERIMENTAL_OUTPUT_DISPLAY
void WaveTrack::SetPan(float newPan)
{
   if (newPan > 1.0)
      mPan = 1.0;
   else if (newPan < -1.0)
      mPan = -1.0;
   else
      mPan = newPan;
}
#endif // EXPERIMENTAL_OUTPUT_DISPLAY

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
void WaveTrack::SetVirtualState(bool state, bool half)
{
   int temp;

   if(half)
      mPerY = 0.5;

   if(state){
      if(mPan){
         temp = mHeight;
         mHeight = temp*mPerY;
         mHeightv = temp - mHeight;
      }
      ReorderList();
   }else{
      if(mPan){
         mHeight = mHeight + mHeightv;
      }
   }
}

int WaveTrack::GetMinimizedHeight() const
{
   if (GetLink()) {
      return 20;
   }

   if(GetChannel() == MonoChannel && GetPan() != 0 && mMonoAsVirtualStereo &&  mDisplay == WaveformDisplay)
      return 20;
   else
      return 40;
}

void WaveTrack::VirtualStereoInit()
{
   int temp;

   if(mChannel == Track::MonoChannel && mPan != 0.0f && mMonoAsVirtualStereo){
      temp = mHeight;
      mHeight = temp*mPerY;
      mHeightv = temp - mHeight;
      ReorderList(false);
   }
}
#endif

float WaveTrack::GetChannelGain(int channel) const
{
   float left = 1.0;
   float right = 1.0;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if(mVirtualStereo)
      channel = 3;
#endif

   if (mPan < 0)
      right = (mPan + 1.0);
   else if (mPan > 0)
      left = 1.0 - mPan;

   if ((channel%2) == 0)
      return left*mGain;
   else
      return right*mGain;
}

bool WaveTrack::ConvertToSampleFormat(sampleFormat format)
{
   for (const auto &clip : mClips)
      clip->ConvertToSampleFormat(format);
   mFormat = format;

   return true;
}

bool WaveTrack::IsEmpty(double t0, double t1)
{
   //printf("Searching for overlap in %.6f...%.6f\n", t0, t1);
   for (const auto &clip : mClips)
   {
      if (!clip->BeforeClip(t1) && !clip->AfterClip(t0)) {
         //printf("Overlapping clip: %.6f...%.6f\n",
         //       clip->GetStartTime(),
         //       clip->GetEndTime());
         // We found a clip that overlaps this region
         return false;
      }
   }
   //printf("No overlap found\n");

   // Otherwise, no clips overlap this region
   return true;
}

Track::Holder WaveTrack::Cut(double t0, double t1)
{
   if (t1 < t0)
      return{};

   auto tmp = Copy(t0, t1);

   if (!tmp)
      return{};

   if (!Clear(t0, t1))
      return{};

   return tmp;
}

Track::Holder WaveTrack::SplitCut(double t0, double t1)
{
   if (t1 < t0)
      return{};

   // SplitCut is the same as 'Copy', then 'SplitDelete'
   auto tmp = Copy(t0, t1);
   if (!tmp)
      return{};
   if (!SplitDelete(t0, t1))
      return{};

   return tmp;
}

#if 0
Track::Holder WaveTrack::CutAndAddCutLine(double t0, double t1)
{
   if (t1 < t0)
      return {};

   // Cut is the same as 'Copy', then 'Delete'
   auto tmp = Copy(t0, t1);
   if (!tmp)
      return {};
   if (!ClearAndAddCutLine(t0, t1))
      return {};

   return tmp;
}
#endif



//Trim trims within a clip, rather than trimming everything.
//If a bound is outside a clip, it trims everything.
bool WaveTrack::Trim (double t0, double t1)
{
   bool inside0 = false;
   bool inside1 = false;
   //Keeps track of the offset of the first clip greater than
   // the left selection t0.
   double firstGreaterOffset = -1;

   for (const auto &clip : mClips)
   {
      //Find the first clip greater than the offset.
      //If we end up clipping the entire track, this is useful.
      if(firstGreaterOffset < 0 &&
            clip->GetStartTime() >= t0)
         firstGreaterOffset = clip->GetStartTime();

      if(t1 > clip->GetStartTime() && t1 < clip->GetEndTime())
      {
         if (!clip->Clear(t1,clip->GetEndTime()))
            return false;
         inside1 = true;
      }

      if(t0 > clip->GetStartTime() && t0 < clip->GetEndTime())
      {
         if (!clip->Clear(clip->GetStartTime(),t0))
            return false;
         clip->SetOffset(t0);
         inside0 = true;
      }
   }

   //if inside0 is false, then the left selector was between
   //clips, so DELETE everything to its left.
   if(false == inside1)
   {
      if (!Clear(t1,GetEndTime()))
         return false;
   }

   if(false == inside0)
   {
      if (!SplitDelete(0,t0))
         return false;
   }

   return true;
}




Track::Holder WaveTrack::Copy(double t0, double t1) const
{
   if (t1 <= t0)
      return{};

   WaveTrack *newTrack;
   Track::Holder result
   { newTrack = safenew WaveTrack{ mDirManager } };

   newTrack->Init(*this);

   for (const auto &clip : mClips)
   {
      if (t0 <= clip->GetStartTime() && t1 >= clip->GetEndTime())
      {
         // Whole clip is in copy region
         //printf("copy: clip %i is in copy region\n", (int)clip);

         newTrack->mClips.push_back
            (make_movable<WaveClip>(*clip, mDirManager));
         WaveClip *const newClip = newTrack->mClips.back().get();
         newClip->RemoveAllCutLines();
         newClip->Offset(-t0);
      }
      else
      if (t1 > clip->GetStartTime() && t0 < clip->GetEndTime())
      {
         // Clip is affected by command
         //printf("copy: clip %i is affected by command\n", (int)clip);

         auto newClip = make_movable<WaveClip>(*clip, mDirManager);
         newClip->RemoveAllCutLines();
         double clip_t0 = t0;
         double clip_t1 = t1;
         if (clip_t0 < clip->GetStartTime())
            clip_t0 = clip->GetStartTime();
         if (clip_t1 > clip->GetEndTime())
            clip_t1 = clip->GetEndTime();

         //printf("copy: clip_t0=%f, clip_t1=%f\n", clip_t0, clip_t1);

         newClip->Offset(-t0);
         if (newClip->GetOffset() < 0)
            newClip->SetOffset(0);

         //printf("copy: clip offset is now %f\n", newClip->GetOffset());

         if (!newClip->CreateFromCopy(clip_t0, clip_t1, clip.get()))
         {
            //printf("paste: CreateFromCopy(%f, %f, %i) returns false, quitting\n",
            //   clip_t0, clip_t1, (int)clip);
            // JKC: July 2007, previously we did 'return false' here which
            // could leave *dest undefined.
            // I think this is dealing with clips that don't have any sequence content
            // i.e. we don't copy cut lines and such - anyone like to explain more?
         }
         else
         {
            newTrack->mClips.push_back(std::move(newClip)); // transfer ownership
         }
      }
   }

   // AWD, Oct 2009: If the selection ends in whitespace, create a placeholder
   // clip representing that whitespace
   if (newTrack->GetEndTime() + 1.0 / newTrack->GetRate() < t1 - t0)
   {
      auto placeholder = make_movable<WaveClip>(mDirManager,
            newTrack->GetSampleFormat(),
            static_cast<int>(newTrack->GetRate()));
      placeholder->SetIsPlaceholder(true);
      if ( ! placeholder->InsertSilence(
               0, (t1 - t0) - newTrack->GetEndTime()) )
      {
      }
      else
      {
         placeholder->Offset(newTrack->GetEndTime());
         newTrack->mClips.push_back(std::move(placeholder)); // transfer ownership
      }
   }

   return result;
}

Track::Holder WaveTrack::CopyNonconst(double t0, double t1)
{
   return Copy(t0, t1);
}

bool WaveTrack::Clear(double t0, double t1)
{
   return HandleClear(t0, t1, false, false);
}

bool WaveTrack::ClearAndAddCutLine(double t0, double t1)
{
   return HandleClear(t0, t1, true, false);
}

const SpectrogramSettings &WaveTrack::GetSpectrogramSettings() const
{
   if (mpSpectrumSettings)
      return *mpSpectrumSettings;
   else
      return SpectrogramSettings::defaults();
}

SpectrogramSettings &WaveTrack::GetSpectrogramSettings()
{
   if (mpSpectrumSettings)
      return *mpSpectrumSettings;
   else
      return SpectrogramSettings::defaults();
}

SpectrogramSettings &WaveTrack::GetIndependentSpectrogramSettings()
{
   if (!mpSpectrumSettings)
      mpSpectrumSettings =
      std::make_unique<SpectrogramSettings>(SpectrogramSettings::defaults());
   return *mpSpectrumSettings;
}

void WaveTrack::SetSpectrogramSettings(std::unique_ptr<SpectrogramSettings> &&pSettings)
{
   if (mpSpectrumSettings != pSettings) {
      mpSpectrumSettings = std::move(pSettings);
   }
}

const WaveformSettings &WaveTrack::GetWaveformSettings() const
{
   if (mpWaveformSettings)
      return *mpWaveformSettings;
   else
      return WaveformSettings::defaults();
}

WaveformSettings &WaveTrack::GetWaveformSettings()
{
   if (mpWaveformSettings)
      return *mpWaveformSettings;
   else
      return WaveformSettings::defaults();
}

WaveformSettings &WaveTrack::GetIndependentWaveformSettings()
{
   if (!mpWaveformSettings)
      mpWaveformSettings = std::make_unique<WaveformSettings>(WaveformSettings::defaults());
   return *mpWaveformSettings;
}

void WaveTrack::SetWaveformSettings(std::unique_ptr<WaveformSettings> &&pSettings)
{
   if (mpWaveformSettings != pSettings) {
      mpWaveformSettings = std::move(pSettings);
   }
}

//
// ClearAndPaste() is a specialized version of HandleClear()
// followed by Paste() and is used mostly by effects that
// can't replace track data directly using Get()/Set().
//
// HandleClear() removes any cut/split lines lines with the
// cleared range, but, in most cases, effects want to preserve
// the existing cut/split lines, so they are saved before the
// HandleClear()/Paste() and restored after.
//
// If the pasted track overlaps two or more clips, then it will
// be pasted with visible split lines.  Normally, effects do not
// want these extra lines, so they may be merged out.
//
bool WaveTrack::ClearAndPaste(double t0, // Start of time to clear
                              double t1, // End of time to clear
                              const Track *src, // What to paste
                              bool preserve, // Whether to reinsert splits/cuts
                              bool merge, // Whether to remove 'extra' splits
                              TimeWarper *effectWarper // How does time change
                              )
{
   double dur = wxMin(t1 - t0, src->GetEndTime());
   wxArrayDouble splits;
   WaveClipHolders cuts;

   // If duration is 0, then it's just a plain paste
   if (dur == 0.0) {
      return Paste(t0, src);
   }

   // If provided time warper was NULL, use a default one that does nothing
   IdentityTimeWarper localWarper;
   TimeWarper *warper = NULL;
   if (effectWarper != NULL) {
      warper = effectWarper;
   } else {
      warper = &localWarper;
   }

   // Align to a sample
   t0 = LongSamplesToTime(TimeToLongSamples(t0));
   t1 = LongSamplesToTime(TimeToLongSamples(t1));

   // Save the cut/split lines whether preserving or not since merging
   // needs to know if a clip boundary is being crossed since Paste()
   // will add split lines around the pasted clip if so.
   for (const auto &clip : mClips) {
      double st;

      // Remember clip boundaries as locations to split
      st = LongSamplesToTime(TimeToLongSamples(clip->GetStartTime()));
      if (st >= t0 && st <= t1 && splits.Index(st) == wxNOT_FOUND) {
         splits.Add(st);
      }

      st = LongSamplesToTime(TimeToLongSamples(clip->GetEndTime()));
      if (st >= t0 && st <= t1 && splits.Index(st) == wxNOT_FOUND) {
         splits.Add(st);
      }

      // Search for cut lines
      auto &cutlines = clip->GetCutLines();
      // May erase from cutlines, so don't use range-for
      for (auto it = cutlines.begin(); it != cutlines.end(); ) {
         WaveClip *cut = it->get();
         double cs = LongSamplesToTime(TimeToLongSamples(clip->GetOffset() +
                                                         cut->GetOffset()));

         // Remember cut point
         if (cs >= t0 && cs <= t1) {

            // Remember the absolute offset and add to our cuts array.
            cut->SetOffset(cs);
            cuts.push_back(std::move(*it)); // transfer ownership!
            it = cutlines.erase(it);
         }
         else
            ++it;
      }
   }

   // Now, clear the selection
   if (HandleClear(t0, t1, false, false)) {

      // And paste in the NEW data
      if (Paste(t0, src)) {
         // First, merge the NEW clip(s) in with the existing clips
         if (merge && splits.GetCount() > 0)
         {
            // Now t1 represents the absolute end of the pasted data.
            t1 = t0 + src->GetEndTime();

            // Get a sorted array of the clips
            auto clips = SortedClipArray();

            // Scan the sorted clips for the first clip whose start time
            // exceeds the pasted regions end time.
            {
               WaveClip *prev = nullptr;
               for (const auto clip : clips) {
                  // Merge this clip and the previous clip if the end time
                  // falls within it and this isn't the first clip in the track.
                  if (fabs(t1 - clip->GetStartTime()) < WAVETRACK_MERGE_POINT_TOLERANCE) {
                     if (prev) {
                        bool bResult = MergeClips(GetClipIndex(prev), GetClipIndex(clip));
                        wxASSERT(bResult); // TO DO: Actually handle this.
                        wxUnusedVar(bResult);
                     }
                     break;
                  }
                  prev = clip;
               }
            }

            // Refill the array since clips have changed.
            clips = SortedClipArray();

            {
               // Scan the sorted clips to look for the start of the pasted
               // region.
               WaveClip *prev = nullptr;
               for (const auto clip : clips) {
                  if (prev) {
                     bool bResult = MergeClips(GetClipIndex(prev), GetClipIndex(clip));
                     wxASSERT(bResult); // TO DO: Actually handle this.
                     wxUnusedVar(bResult);
                     break;
                  }
                  if (fabs(t0 - clip->GetEndTime()) < WAVETRACK_MERGE_POINT_TOLERANCE)
                     // Merge this clip and the next clip if the start time
                     // falls within it and this isn't the last clip in the track.
                     prev = clip;
                  else
                     prev = nullptr;
               }
            }
         }

         // Restore cut/split lines
         if (preserve) {

            // Restore the split lines, transforming the position appropriately
            for (const auto split: splits) {
               SplitAt(warper->Warp(split));
            }

            // Restore the saved cut lines, also transforming if time altered
            for (const auto &clip : mClips) {
               double st;
               double et;

               st = clip->GetStartTime();
               et = clip->GetEndTime();

               // Scan the cuts for any that live within this clip
               for (auto it = cuts.begin(); it != cuts.end();) {
                  WaveClip *cut = it->get();
                  double cs = cut->GetOffset();

                  // Offset the cut from the start of the clip and add it to
                  // this clips cutlines.
                  if (cs >= st && cs <= et) {
                     cut->SetOffset(warper->Warp(cs) - st);
                     clip->GetCutLines().push_back( std::move(*it) ); // transfer ownership!
                     it = cuts.erase(it);
                  }
                  else
                     ++it;
               }
            }
         }
      }
   }

   return true;
}

bool WaveTrack::SplitDelete(double t0, double t1)
{
   bool addCutLines = false;
   bool split = true;
   return HandleClear(t0, t1, addCutLines, split);
}

namespace
{
   WaveClipHolders::const_iterator
      FindClip(const WaveClipHolders &list, const WaveClip *clip, int *distance = nullptr)
   {
      if (distance)
         *distance = 0;
      auto it = list.begin();
      for (const auto end = list.end(); it != end; ++it)
      {
         if (it->get() == clip)
            break;
         if (distance)
            ++*distance;
      }
      return it;
   }

   WaveClipHolders::iterator
      FindClip(WaveClipHolders &list, const WaveClip *clip, int *distance = nullptr)
   {
      if (distance)
         *distance = 0;
      auto it = list.begin();
      for (const auto end = list.end(); it != end; ++it)
      {
         if (it->get() == clip)
            break;
         if (distance)
            ++*distance;
      }
      return it;
   }
}

movable_ptr<WaveClip> WaveTrack::RemoveAndReturnClip(WaveClip* clip)
{
   // Be clear about who owns the clip!!
   auto it = FindClip(mClips, clip);
   if (it != mClips.end()) {
      auto result = std::move(*it); // Array stops owning the clip, before we shrink it
      mClips.erase(it);
      return result;
   }
   else
      return {};
}

void WaveTrack::AddClip(movable_ptr<WaveClip> &&clip)
{
   // Uncomment the following line after we correct the problem of zero-length clips
   //if (CanInsertClip(clip))
      mClips.push_back(std::move(clip)); // transfer ownership
}

bool WaveTrack::HandleClear(double t0, double t1,
                            bool addCutLines, bool split)
{
   if (t1 < t0)
      return false;

   bool editClipCanMove = true;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove);

   WaveClipPointers clipsToDelete;
   WaveClipHolders clipsToAdd;

   // We only add cut lines when deleting in the middle of a single clip
   // The cut line code is not really prepared to handle other situations
   if (addCutLines)
   {
      for (const auto &clip : mClips)
      {
         if (!clip->BeforeClip(t1) && !clip->AfterClip(t0) &&
               (clip->BeforeClip(t0) || clip->AfterClip(t1)))
         {
            addCutLines = false;
            break;
         }
      }
   }

   for (const auto &clip : mClips)
   {
      if (clip->BeforeClip(t0) && clip->AfterClip(t1))
      {
         // Whole clip must be deleted - remember this
         clipsToDelete.push_back(clip.get());
      }
      else if (!clip->BeforeClip(t1) && !clip->AfterClip(t0))
      {
         // Clip data is affected by command
         if (addCutLines)
         {
            if (!clip->ClearAndAddCutLine(t0,t1))
               return false;
         } else
         {
            if (split) {
               // Three cases:

               if (clip->BeforeClip(t0)) {
                  // Delete from the left edge
                  clip->Clear(clip->GetStartTime(), t1);
                  clip->Offset(t1-clip->GetStartTime());
               } else
               if (clip->AfterClip(t1)) {
                  // Delete to right edge
                  clip->Clear(t0, clip->GetEndTime());
               } else
               {
                  // Delete in the middle of the clip...we actually create two
                  // NEW clips out of the left and right halves...

                  // left
                  clipsToAdd.push_back(make_movable<WaveClip>(*clip, mDirManager));
                  clipsToAdd.back()->Clear(t0, clip->GetEndTime());

                  // right
                  clipsToAdd.push_back(make_movable<WaveClip>(*clip, mDirManager));
                  WaveClip *const right = clipsToAdd.back().get();
                  right->Clear(clip->GetStartTime(), t1);
                  right->Offset(t1 - clip->GetStartTime());

                  clipsToDelete.push_back(clip.get());
               }
            }
            else { // (We are not doing a split cut)
               /* We are going to DELETE part of the clip here. The clip may
                * have envelope points, and we need to ensure that the envelope
                * outside of the cleared region is not affected. This means
                * putting in "glue" points where the clip enters and leaves the
                * region being cleared. If one of the ends of the clip is inside
                * the region, then one of the glue points will be redundant. */
               // clip->Clear keeps points < t0 and >= t1 via Envelope::CollapseRegion
               if (clip->GetEnvelope()->GetNumberOfPoints() > 0) {   // don't insert env pts if none exist
                  double val;
                  if (clip->WithinClip(t0))
                     {  // start of region within clip
                     val = clip->GetEnvelope()->GetValue(t0);
                     clip->GetEnvelope()->Insert(t0 - clip->GetOffset() - 1.0/clip->GetRate(), val);
                     }
                  if (clip->WithinClip(t1))
                     {  // end of region within clip
                     val = clip->GetEnvelope()->GetValue(t1);
                     clip->GetEnvelope()->Insert(t1 - clip->GetOffset(), val);
                     }
               }
               if (!clip->Clear(t0,t1))
                  return false;
               clip->GetEnvelope()->RemoveUnneededPoints(t0);
            }
         }
      } else
      if (clip->BeforeClip(t1))
      {
         // Clip is "behind" the region -- offset it unless we're splitting
         // or we're using the "don't move other clips" mode
         if (!split && editClipCanMove)
            clip->Offset(-(t1-t0));
      }
   }

   for (const auto &clip: clipsToDelete)
   {
      auto myIt = FindClip(mClips, clip);
      if (myIt != mClips.end())
         mClips.erase(myIt); // deletes the clip!
      else
         wxASSERT(false);
   }

   for (auto &clip: clipsToAdd)
      mClips.push_back(std::move(clip)); // transfer ownership

   return true;
}

bool WaveTrack::SyncLockAdjust(double oldT1, double newT1)
{
   if (newT1 > oldT1) {
      // Insert space within the track

      // JKC: This is a rare case where using >= rather than > on a float matters.
      // GetEndTime() looks through the clips and may give us EXACTLY the same
      // value as T1, when T1 was set to be at the end of one of those clips.
      if (oldT1 >= GetEndTime())
         return true;

      // If track is empty at oldT1 insert whitespace; otherwise, silence
      if (IsEmpty(oldT1, oldT1))
      {
         bool ret = true;

         // Check if clips can move
         bool clipsCanMove = true;
         gPrefs->Read(wxT("/GUI/EditClipCanMove"), &clipsCanMove);
         if (clipsCanMove) {
            auto tmp = Cut (oldT1, GetEndTime() + 1.0/GetRate());
            if (!tmp)
               return false;

            ret = Paste(newT1, tmp.get());
            wxASSERT(ret);
         }

         return ret;
      }
      else {
         // AWD: Could just use InsertSilence() on its own here, but it doesn't
         // follow EditClipCanMove rules (Paste() does it right)
         AudacityProject *p = GetActiveProject();
         if (!p) return false;
         TrackFactory *f = p->GetTrackFactory();
         if (!f) return false;
         auto tmp = f->NewWaveTrack(GetSampleFormat(), GetRate());

         bool bResult = tmp->InsertSilence(0.0, newT1 - oldT1);
         wxASSERT(bResult); // TO DO: Actually handle this.
         wxUnusedVar(bResult);
         tmp->Flush();
         bResult = Paste(oldT1, tmp.get());
         wxASSERT(bResult); // TO DO: Actually handle this.
         wxUnusedVar(bResult);
      }
   }
   else if (newT1 < oldT1) {
      return Clear(newT1, oldT1);
   }

   // fall-through: no change
   return true;
}

bool WaveTrack::Paste(double t0, const Track *src)
{
   bool editClipCanMove = true;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove);

   if( src == NULL )
      return false;

   if (src->GetKind() != Track::Wave)
      return false;

   const WaveTrack* other = static_cast<const WaveTrack*>(src);

   //
   // Pasting is a bit complicated, because with the existence of multiclip mode,
   // we must guess the behaviour the user wants.
   //
   // Currently, two modes are implemented:
   //
   // - If a single clip should be pasted, and it should be pasted inside another
   //   clip, no NEW clips are generated. The audio is simply inserted.
   //   This resembles the old (pre-multiclip support) behaviour. However, if
   //   the clip is pasted outside of any clip, a NEW clip is generated. This is
   //   the only behaviour which is different to what was done before, but it
   //   shouldn't confuse users too much.
   //
   // - If multiple clips should be pasted, or a single clip that does not fill
   // the duration of the pasted track, these are always pasted as single
   // clips, and the current clip is splitted, when necessary. This may seem
   // strange at first, but it probably is better than trying to auto-merge
   // anything. The user can still merge the clips by hand (which should be a
   // simple command reachable by a hotkey or single mouse click).
   //

   if (other->GetNumClips() == 0)
      return false;

   //printf("paste: we have at least one clip\n");

   bool singleClipMode = (other->GetNumClips() == 1 &&
         other->GetStartTime() == 0.0);

   double insertDuration = other->GetEndTime();
   //printf("Check if we need to make room for the pasted data\n");

   // Make room for the pasted data
   if (editClipCanMove) {
      if (!singleClipMode) {
         // We need to insert multiple clips, so split the current clip and
         // move everything to the right, then try to paste again
         if (!IsEmpty(t0, GetEndTime())) {
            auto tmp = Cut(t0, GetEndTime()+1.0/mRate);
            bool bResult = Paste(t0 + insertDuration, tmp.get());
            wxASSERT(bResult); // TO DO: Actually handle this.
            wxUnusedVar(bResult);
         }
      }
      else {
         // We only need to insert one single clip, so just move all clips
         // to the right of the paste point out of the way
         for (const auto &clip : mClips)
         {
            if (clip->GetStartTime() > t0-(1.0/mRate))
               clip->Offset(insertDuration);
         }
      }
   }

   if (singleClipMode)
   {
      // Single clip mode
      // printf("paste: checking for single clip mode!\n");

      WaveClip *insideClip = NULL;

      for (const auto &clip : mClips)
      {
         if (editClipCanMove)
         {
            if (clip->WithinClip(t0))
            {
               //printf("t0=%.6f: inside clip is %.6f ... %.6f\n",
               //       t0, clip->GetStartTime(), clip->GetEndTime());
               insideClip = clip.get();
               break;
            }
         } else
         {
            // If clips are immovable we also allow prepending to clips
            if (clip->WithinClip(t0) ||
                  TimeToLongSamples(t0) == clip->GetStartSample())
            {
               insideClip = clip.get();
               break;
            }
         }
      }

      if (insideClip)
      {
         // Exhibit traditional behaviour
         //printf("paste: traditional behaviour\n");
         if (!editClipCanMove)
         {
            // We did not move other clips out of the way already, so
            // check if we can paste without having to move other clips
            for (const auto &clip : mClips)
            {
               if (clip->GetStartTime() > insideClip->GetStartTime() &&
                   insideClip->GetEndTime() + insertDuration >
                                                      clip->GetStartTime())
               {
                  wxMessageBox(
                     _("There is not enough room available to paste the selection"),
                     _("Error"), wxICON_STOP);
                  return false;
               }
            }
         }

         return insideClip->Paste(t0, other->GetClipByIndex(0));
      }

      // Just fall through and exhibit NEW behaviour
   }

   // Insert NEW clips
   //printf("paste: multi clip mode!\n");

   if (!editClipCanMove && !IsEmpty(t0, t0+insertDuration-1.0/mRate))
   {
      wxMessageBox(
         _("There is not enough room available to paste the selection"),
         _("Error"), wxICON_STOP);
      return false;
   }

   for (const auto &clip : other->mClips)
   {
      // AWD Oct. 2009: Don't actually paste in placeholder clips
      if (!clip->GetIsPlaceholder())
      {
         auto newClip = make_movable<WaveClip>(*clip, mDirManager);
         newClip->Resample(mRate);
         newClip->Offset(t0);
         newClip->MarkChanged();
         mClips.push_back(std::move(newClip)); // transfer ownership
      }
   }
   return true;
}

bool WaveTrack::Silence(double t0, double t1)
{
   if (t1 < t0)
      return false;

   auto start = (sampleCount)floor(t0 * mRate + 0.5);
   auto len = (sampleCount)floor(t1 * mRate + 0.5) - start;
   bool result = true;

   for (const auto &clip : mClips)
   {
      auto clipStart = clip->GetStartSample();
      auto clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy = start+len - clipStart;
         if (samplesToCopy > clip->GetNumSamples())
            samplesToCopy = clip->GetNumSamples();
         auto startDelta = clipStart - start;
         decltype(startDelta) inclipDelta = 0;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            startDelta = 0;
         }

         if (!clip->GetSequence()->SetSilence(inclipDelta, samplesToCopy))
         {
            wxASSERT(false); // should always work
            return false;
         }
         clip->MarkChanged();
      }
   }

   return result;
}

bool WaveTrack::InsertSilence(double t, double len)
{
   if (len <= 0)
      return false;

   if (mClips.empty())
   {
      // Special case if there is no clip yet
      WaveClip* clip = CreateClip();
      return clip->InsertSilence(0, len);
   }

   for (const auto &clip : mClips)
   {
      if (clip->BeforeClip(t))
         clip->Offset(len);
      else if (clip->WithinClip(t))
      {
         if (!clip->InsertSilence(t, len)) {
            return false;
         }
      }
   }

   return true;
}

//Performs the opposite of Join
//Analyses selected region for possible Joined clips and disjoins them
bool WaveTrack::Disjoin(double t0, double t1)
{
   auto minSamples = TimeToLongSamples( WAVETRACK_MERGE_POINT_TOLERANCE );
   size_t maxAtOnce = 1048576;
   float *buffer = new float[ maxAtOnce ];
   Regions regions;

   wxBusyCursor busy;

   for (const auto &clip : mClips)
   {
      double startTime = clip->GetStartTime();
      double endTime = clip->GetEndTime();

      if( endTime < t0 || startTime > t1 )
         continue;

      if( t0 > startTime )
         startTime = t0;
      if( t1 < endTime )
         endTime = t1;

      //simply look for a sequence of zeroes and if the sequence
      //is greater than minimum number, split-DELETE the region

      sampleCount seqStart = -1;
      sampleCount start, end;
      clip->TimeToSamplesClip( startTime, &start );
      clip->TimeToSamplesClip( endTime, &end );

      auto len = ( end - start );
      for( decltype(len) done = 0; done < len; done += maxAtOnce )
      {
         auto numSamples = limitSampleBufferSize( maxAtOnce, len - done );

         clip->GetSamples( ( samplePtr )buffer, floatSample, start + done,
               numSamples );
         for( decltype(numSamples) i = 0; i < numSamples; i++ )
         {
            auto curSamplePos = start + done + i;

            //start a NEW sequence
            if( buffer[ i ] == 0.0 && seqStart == -1 )
               seqStart = curSamplePos;
            else if( buffer[ i ] != 0.0 || curSamplePos == end - 1 )
            {
               if( seqStart != -1 )
               {
                  decltype(end) seqEnd;

                  //consider the end case, where selection ends in zeroes
                  if( curSamplePos == end - 1 && buffer[ i ] == 0.0 )
                     seqEnd = end;
                  else
                     seqEnd = curSamplePos;
                  if( seqEnd - seqStart + 1 > minSamples )
                  {
                     regions.push_back(Region(
                        seqStart.as_double() / GetRate()
                                              + clip->GetStartTime(),
                        seqEnd.as_double() / GetRate()
                                              + clip->GetStartTime()));
                  }
                  seqStart = -1;
               }
            }
         }
      }
   }

   for( unsigned int i = 0; i < regions.size(); i++ )
   {
      const Region &region = regions.at(i);
      SplitDelete(region.start, region.end );
   }

   delete[] buffer;
   return true;
}

bool WaveTrack::Join(double t0, double t1)
{
   // Merge all WaveClips overlapping selection into one

   WaveClipPointers clipsToDelete;
   WaveClip *newClip;

   for (const auto &clip: mClips)
   {
      if (clip->GetStartTime() < t1-(1.0/mRate) &&
          clip->GetEndTime()-(1.0/mRate) > t0) {

         // Put in sorted order
         auto it = clipsToDelete.begin(), end = clipsToDelete.end();
         for (; it != end; ++it)
            if ((*it)->GetStartTime() > clip->GetStartTime())
               break;
         //printf("Insert clip %.6f at position %d\n", clip->GetStartTime(), i);
         clipsToDelete.insert(it, clip.get());
      }
   }

   //if there are no clips to DELETE, nothing to do
   if( clipsToDelete.size() == 0 )
      return true;

   newClip = CreateClip();
   double t = clipsToDelete[0]->GetOffset();
   newClip->SetOffset(t);
   for (const auto &clip : clipsToDelete)
   {
      //printf("t=%.6f adding clip (offset %.6f, %.6f ... %.6f)\n",
      //       t, clip->GetOffset(), clip->GetStartTime(), clip->GetEndTime());

      if (clip->GetOffset() - t > (1.0 / mRate)) {
         double addedSilence = (clip->GetOffset() - t);
         //printf("Adding %.6f seconds of silence\n");
         bool bResult = newClip->InsertSilence(t, addedSilence);
         wxASSERT(bResult); // TO DO: Actually handle this.
         wxUnusedVar(bResult);
         t += addedSilence;
      }

      //printf("Pasting at %.6f\n", t);
      bool bResult = newClip->Paste(t, clip);
      wxASSERT(bResult); // TO DO: Actually handle this.
      wxUnusedVar(bResult);
      t = newClip->GetEndTime();

      auto it = FindClip(mClips, clip);
      mClips.erase(it); // deletes the clip
   }

   return true;
}

bool WaveTrack::Append(samplePtr buffer, sampleFormat format,
                       size_t len, unsigned int stride /* = 1 */,
                       XMLWriter *blockFileLog /* = NULL */)
{
   return RightmostOrNewClip()->Append(buffer, format, len, stride,
                                        blockFileLog);
}

bool WaveTrack::AppendAlias(const wxString &fName, sampleCount start,
                            size_t len, int channel,bool useOD)
{
   return RightmostOrNewClip()->AppendAlias(fName, start, len, channel, useOD);
}


bool WaveTrack::AppendCoded(const wxString &fName, sampleCount start,
                            size_t len, int channel, int decodeType)
{
   return RightmostOrNewClip()->AppendCoded(fName, start, len, channel, decodeType);
}

///gets an int with OD flags so that we can determine which ODTasks should be run on this track after save/open, etc.
unsigned int WaveTrack::GetODFlags()
{
   unsigned int ret = 0;
   for (const auto &clip : mClips)
   {
      ret = ret | clip->GetSequence()->GetODFlags();
   }
   return ret;
}


sampleCount WaveTrack::GetBlockStart(sampleCount s) const
{
   for (const auto &clip : mClips)
   {
      const auto startSample = (sampleCount)floor(0.5 + clip->GetStartTime()*mRate);
      const auto endSample = startSample + clip->GetNumSamples();
      if (s >= startSample && s < endSample)
         return startSample + clip->GetSequence()->GetBlockStart(s - startSample);
   }

   return -1;
}

size_t WaveTrack::GetBestBlockSize(sampleCount s) const
{
   auto bestBlockSize = GetMaxBlockSize();

   for (const auto &clip : mClips)
   {
      auto startSample = (sampleCount)floor(clip->GetStartTime()*mRate + 0.5);
      auto endSample = startSample + clip->GetNumSamples();
      if (s >= startSample && s < endSample)
      {
         bestBlockSize = clip->GetSequence()->GetBestBlockSize(s - startSample);
         break;
      }
   }

   return bestBlockSize;
}

size_t WaveTrack::GetMaxBlockSize() const
{
   decltype(GetMaxBlockSize()) maxblocksize = 0;
   for (const auto &clip : mClips)
   {
      maxblocksize = std::max(maxblocksize, clip->GetSequence()->GetMaxBlockSize());
   }

   if (maxblocksize == 0)
   {
      // We really need the maximum block size, so create a
      // temporary sequence to get it.
      maxblocksize = Sequence{ mDirManager, mFormat }.GetMaxBlockSize();
   }

   wxASSERT(maxblocksize > 0);

   return maxblocksize;
}

size_t WaveTrack::GetIdealBlockSize()
{
   return NewestOrNewClip()->GetSequence()->GetIdealBlockSize();
}

bool WaveTrack::Flush()
{
   // After appending, presumably.  Do this to the clip that gets appended.
   return RightmostOrNewClip()->Flush();
}

bool WaveTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("wavetrack"))) {
      double dblValue;
      long nValue;
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("rate")))
         {
            // mRate is an int, but "rate" in the project file is a float.
            if (!XMLValueChecker::IsGoodString(strValue) ||
                  !Internat::CompatibleToDouble(strValue, &dblValue) ||
                  (dblValue < 1.0) || (dblValue > 1000000.0)) // allow a large range to be read
               return false;
            mRate = lrint(dblValue);
         }
         else if (!wxStrcmp(attr, wxT("offset")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue))
         {
            // Offset is only relevant for legacy project files. The value
            // is cached until the actual WaveClip containing the legacy
            // track is created.
            mLegacyProjectFileOffset = dblValue;
         }
         else if (!wxStrcmp(attr, wxT("mute")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mMute = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("solo")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mSolo = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("height")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mHeight = nValue;
         else if (!wxStrcmp(attr, wxT("minimized")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mMinimized = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("isSelected")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            this->SetSelected(nValue != 0);
         else if (!wxStrcmp(attr, wxT("gain")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue))
            mGain = dblValue;
         else if (!wxStrcmp(attr, wxT("pan")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue) &&
                  (dblValue >= -1.0) && (dblValue <= 1.0))
            mPan = dblValue;
         else if (!wxStrcmp(attr, wxT("name")) && XMLValueChecker::IsGoodString(strValue))
            mName = strValue;
         else if (!wxStrcmp(attr, wxT("channel")))
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) ||
                  !XMLValueChecker::IsValidChannel(nValue))
               return false;
            mChannel = nValue;
         }
         else if (!wxStrcmp(attr, wxT("linked")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            SetLinked(nValue != 0);
         else if (!wxStrcmp(attr, wxT("autosaveid")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mAutoSaveIdent = (int) nValue;

      } // while
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      VirtualStereoInit();
#endif
      return true;
   }

   return false;
}

void WaveTrack::HandleXMLEndTag(const wxChar * WXUNUSED(tag))
{
   // In case we opened a pre-multiclip project, we need to
   // simulate closing the waveclip tag.
   NewestOrNewClip()->HandleXMLEndTag(wxT("waveclip"));
}

XMLTagHandler *WaveTrack::HandleXMLChild(const wxChar *tag)
{
   //
   // This is legacy code (1.2 and previous) and is not called for NEW projects!
   //
   if (!wxStrcmp(tag, wxT("sequence")) || !wxStrcmp(tag, wxT("envelope")))
   {
      // This is a legacy project, so set the cached offset
      NewestOrNewClip()->SetOffset(mLegacyProjectFileOffset);

      // Legacy project file tracks are imported as one single wave clip
      if (!wxStrcmp(tag, wxT("sequence")))
         return NewestOrNewClip()->GetSequence();
      else if (!wxStrcmp(tag, wxT("envelope")))
         return NewestOrNewClip()->GetEnvelope();
   }

   // JKC... for 1.1.0, one step better than what we had, but still badly broken.
   //If we see a waveblock at this level, we'd better generate a sequence.
   if( !wxStrcmp( tag, wxT("waveblock" )))
   {
      // This is a legacy project, so set the cached offset
      NewestOrNewClip()->SetOffset(mLegacyProjectFileOffset);
      Sequence *pSeq = NewestOrNewClip()->GetSequence();
      return pSeq;
   }

   //
   // This is for the NEW file format (post-1.2)
   //
   if (!wxStrcmp(tag, wxT("waveclip")))
      return CreateClip();
   else
      return NULL;
}

void WaveTrack::WriteXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("wavetrack"));
   if (mAutoSaveIdent)
   {
      xmlFile.WriteAttr(wxT("autosaveid"), mAutoSaveIdent);
   }
   xmlFile.WriteAttr(wxT("name"), mName);
   xmlFile.WriteAttr(wxT("channel"), mChannel);
   xmlFile.WriteAttr(wxT("linked"), mLinked);
   xmlFile.WriteAttr(wxT("mute"), mMute);
   xmlFile.WriteAttr(wxT("solo"), mSolo);
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   int height;
   if(MONO_PAN)
      height = mHeight + mHeightv;
   else
      height = this->GetActualHeight();
   xmlFile.WriteAttr(wxT("height"), height);
#else
   xmlFile.WriteAttr(wxT("height"), this->GetActualHeight());
#endif
   xmlFile.WriteAttr(wxT("minimized"), this->GetMinimized());
   xmlFile.WriteAttr(wxT("isSelected"), this->GetSelected());
   xmlFile.WriteAttr(wxT("rate"), mRate);
   xmlFile.WriteAttr(wxT("gain"), (double)mGain);
   xmlFile.WriteAttr(wxT("pan"), (double)mPan);

   for (const auto &clip : mClips)
   {
      clip->WriteXML(xmlFile);
   }

   xmlFile.EndTag(wxT("wavetrack"));
}

bool WaveTrack::GetErrorOpening()
{
   for (const auto &clip : mClips)
      if (clip->GetSequence()->GetErrorOpening())
         return true;

   return false;
}

bool WaveTrack::Lock() const
{
   for (const auto &clip : mClips)
      clip->Lock();

   return true;
}

bool WaveTrack::CloseLock()
{
   for (const auto &clip : mClips)
      clip->CloseLock();

   return true;
}


bool WaveTrack::Unlock() const
{
   for (const auto &clip : mClips)
      clip->Unlock();

   return true;
}

AUDACITY_DLL_API sampleCount WaveTrack::TimeToLongSamples(double t0) const
{
   return sampleCount( floor(t0 * mRate + 0.5) );
}

double WaveTrack::LongSamplesToTime(sampleCount pos) const
{
   return pos.as_double() / mRate;
}

double WaveTrack::GetStartTime() const
{
   bool found = false;
   double best = 0.0;

   if (mClips.empty())
      return 0;

   for (const auto &clip : mClips)
      if (!found)
      {
         found = true;
         best = clip->GetStartTime();
      }
      else if (clip->GetStartTime() < best)
         best = clip->GetStartTime();

   return best;
}

double WaveTrack::GetEndTime() const
{
   bool found = false;
   double best = 0.0;

   if (mClips.empty())
      return 0;

   for (const auto &clip : mClips)
      if (!found)
      {
         found = true;
         best = clip->GetEndTime();
      }
      else if (clip->GetEndTime() > best)
         best = clip->GetEndTime();

   return best;
}

//
// Getting/setting samples.  The sample counts here are
// expressed relative to t=0.0 at the track's sample rate.
//

bool WaveTrack::GetMinMax(float *min, float *max,
                          double t0, double t1) const
{
   bool clipFound = false;

   *min = FLT_MAX;   // we need these at extremes to make sure we find true min and max
   *max = -FLT_MAX;

   if (t0 > t1)
      return false;

   if (t0 == t1)
      return true;

   bool result = true;

   for (const auto &clip: mClips)
   {
      if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      {
         clipFound = true;
         float clipmin, clipmax;
         if (clip->GetMinMax(&clipmin, &clipmax, t0, t1))
         {
            if (clipmin < *min)
               *min = clipmin;
            if (clipmax > *max)
               *max = clipmax;
         } else
         {
            result = false;
         }
      }
   }

   if(!clipFound)
   {
      *min = float(0.0);   // sensible defaults if no clips found
      *max = float(0.0);
   }

   return result;
}

bool WaveTrack::GetRMS(float *rms, double t0, double t1)
{
   *rms = float(0.0);

   if (t0 > t1)
      return false;

   if (t0 == t1)
      return true;

   bool result = true;
   double sumsq = 0.0;
   sampleCount length = 0;

   for (const auto &clip: mClips)
   {
      // If t1 == clip->GetStartTime() or t0 == clip->GetEndTime(), then the clip
      // is not inside the selection, so we don't want it.
      // if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      {
         float cliprms;
         sampleCount clipStart, clipEnd;

         if (clip->GetRMS(&cliprms, t0, t1))
         {
            clip->TimeToSamplesClip(wxMax(t0, clip->GetStartTime()), &clipStart);
            clip->TimeToSamplesClip(wxMin(t1, clip->GetEndTime()), &clipEnd);
            sumsq += cliprms * cliprms * (clipEnd - clipStart).as_float();
            length += (clipEnd - clipStart);
         }
         else
         {
            result = false;
         }
      }
   }
   *rms = length > 0 ? sqrt(sumsq / length.as_double()) : 0.0;

   return result;
}

bool WaveTrack::Get(samplePtr buffer, sampleFormat format,
                    sampleCount start, size_t len, fillFormat fill ) const
{
   // Simple optimization: When this buffer is completely contained within one clip,
   // don't clear anything (because we won't have to). Otherwise, just clear
   // everything to be on the safe side.
   bool doClear = true;
   for (const auto &clip: mClips)
   {
      if (start >= clip->GetStartSample() && start+len <= clip->GetEndSample())
      {
         doClear = false;
         break;
      }
   }
   if (doClear)
   {
      // Usually we fill in empty sapce with zero
      if( fill == fillZero )
         ClearSamples(buffer, format, 0, len);
      // but we don't have to.
      else if( fill==fillTwo )
      {
         wxASSERT( format==floatSample );
         float * pBuffer = (float*)buffer;
         for(int i=0;i<len;i++)
            pBuffer[i]=2.0f;
      }
      else
      {
         wxFAIL_MSG(wxT("Invalid fill format"));
      }
   }

   for (const auto &clip: mClips)
   {
      auto clipStart = clip->GetStartSample();
      auto clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy =
            std::min( start+len - clipStart, clip->GetNumSamples() );
         auto startDelta = clipStart - start;
         decltype(startDelta) inclipDelta = 0;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            // samplesToCopy is now either len or
            //    (clipEnd - clipStart) - (start - clipStart)
            //    == clipEnd - start > 0
            // samplesToCopy is not more than len
            //
            startDelta = 0;
            // startDelta is zero
         }
         else {
            // startDelta is nonnegative and less than than len
            // samplesToCopy is positive and not more than len
         }

         if (!clip->GetSamples(
               (samplePtr)(((char*)buffer) +
                           startDelta.as_size_t() *
                           SAMPLE_SIZE(format)),
               format, inclipDelta, samplesToCopy.as_size_t() ))
         {
            wxASSERT(false); // should always work
            return false;
         }
      }
   }

   return true;
}

bool WaveTrack::Set(samplePtr buffer, sampleFormat format,
                    sampleCount start, size_t len)
{
   bool result = true;

   for (const auto &clip: mClips)
   {
      auto clipStart = clip->GetStartSample();
      auto clipEnd = clip->GetEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy =
            std::min( start+len - clipStart, clip->GetNumSamples() );
         auto startDelta = clipStart - start;
         decltype(startDelta) inclipDelta = 0;
         if (startDelta < 0)
         {
            inclipDelta = -startDelta; // make positive value
            samplesToCopy -= inclipDelta;
            // samplesToCopy is now either len or
            //    (clipEnd - clipStart) - (start - clipStart)
            //    == clipEnd - start > 0
            // samplesToCopy is not more than len
            //
            startDelta = 0;
            // startDelta is zero
         }
         else {
            // startDelta is nonnegative and less than than len
            // samplesToCopy is positive and not more than len
         }

         if (!clip->SetSamples(
               (samplePtr)(((char*)buffer) +
                           startDelta.as_size_t() *
                           SAMPLE_SIZE(format)),
               format, inclipDelta, samplesToCopy.as_size_t() ))
         {
            wxASSERT(false); // should always work
            return false;
         }
         clip->MarkChanged();
      }
   }

   return result;
}

void WaveTrack::GetEnvelopeValues(double *buffer, size_t bufferLen,
                                  double t0) const
{
   // The output buffer corresponds to an unbroken span of time which the callers expect
   // to be fully valid.  As clips are processed below, the output buffer is updated with
   // envelope values from any portion of a clip, start, end, middle, or none at all.
   // Since this does not guarantee that the entire buffer is filled with values we need
   // to initialize the entire buffer to a default value.
   //
   // This does mean that, in the cases where a usuable clip is located, the buffer value will
   // be set twice.  Unfortunately, there is no easy way around this since the clips are not
   // stored in increasing time order.  If they were, we could just track the time as the
   // buffer is filled.
   for (decltype(bufferLen) i = 0; i < bufferLen; i++)
   {
      buffer[i] = 1.0;
   }

   double startTime = t0;
   auto tstep = 1.0 / mRate;
   double endTime = t0 + tstep * bufferLen;
   for (const auto &clip: mClips)
   {
      // IF clip intersects startTime..endTime THEN...
      auto dClipStartTime = clip->GetStartTime();
      auto dClipEndTime = clip->GetEndTime();
      if ((dClipStartTime < endTime) && (dClipEndTime > startTime))
      {
         auto rbuf = buffer;
         auto rlen = bufferLen;
         auto rt0 = t0;

         if (rt0 < dClipStartTime)
         {
            // This is not more than the number of samples in
            // (endTime - startTime) which is bufferLen:
            auto nDiff = (sampleCount)floor((dClipStartTime - rt0) * mRate + 0.5);
            auto snDiff = nDiff.as_size_t();
            rbuf += snDiff;
            wxASSERT(snDiff <= rlen);
            rlen -= snDiff;
            rt0 = dClipStartTime;
         }

         if (rt0 + rlen*tstep > dClipEndTime)
         {
            auto nClipLen = clip->GetEndSample() - clip->GetStartSample();

            if (nClipLen <= 0) // Testing for bug 641, this problem is consistently '== 0', but doesn't hurt to check <.
               return;

            // This check prevents problem cited in http://bugzilla.audacityteam.org/show_bug.cgi?id=528#c11,
            // Gale's cross_fade_out project, which was already corrupted by bug 528.
            // This conditional prevents the previous write past the buffer end, in clip->GetEnvelope() call.
            // Never increase rlen here.
            // PRL bug 827:  rewrote it again
            rlen = limitSampleBufferSize( rlen, nClipLen );
            rlen = std::min(rlen, size_t(floor(0.5 + (dClipEndTime - rt0) / tstep)));
         }
         clip->GetEnvelope()->GetValues(rbuf, rlen, rt0, tstep);
      }
   }
}

WaveClip* WaveTrack::GetClipAtX(int xcoord)
{
   for (const auto &clip: mClips)
   {
      wxRect r;
      clip->GetDisplayRect(&r);
      if (xcoord >= r.x && xcoord < r.x+r.width)
         return clip.get();
   }

   return NULL;
}

WaveClip* WaveTrack::GetClipAtSample(sampleCount sample)
{
   for (const auto &clip: mClips)
   {
      auto start = clip->GetStartSample();
      auto len   = clip->GetNumSamples();

      if (sample >= start && sample < start + len)
         return clip.get();
   }

   return NULL;
}

Envelope* WaveTrack::GetEnvelopeAtX(int xcoord)
{
   WaveClip* clip = GetClipAtX(xcoord);
   if (clip)
      return clip->GetEnvelope();
   else
      return NULL;
}

// Search for any active DragPoint on the current track
Envelope* WaveTrack::GetActiveEnvelope(void)
{
   for (const auto &clip : mClips)
   {
      Envelope* env = clip->GetEnvelope() ;
      if (env->GetDragPoint() >= 0)
         return env;
   }
   return NULL;
}

Sequence* WaveTrack::GetSequenceAtX(int xcoord)
{
   WaveClip* clip = GetClipAtX(xcoord);
   if (clip)
      return clip->GetSequence();
   else
      return NULL;
}

WaveClip* WaveTrack::CreateClip()
{
   mClips.push_back(make_movable<WaveClip>(mDirManager, mFormat, mRate));
   return mClips.back().get();
}

WaveClip* WaveTrack::NewestOrNewClip()
{
   if (mClips.empty()) {
      WaveClip *clip = CreateClip();
      clip->SetOffset(mOffset);
      return clip;
   }
   else
      return mClips.back().get();
}

WaveClip* WaveTrack::RightmostOrNewClip()
{
   if (mClips.empty()) {
      WaveClip *clip = CreateClip();
      clip->SetOffset(mOffset);
      return clip;
   }
   else
   {
      auto it = mClips.begin();
      WaveClip *rightmost = (*it++).get();
      double maxOffset = rightmost->GetOffset();
      for (auto end = mClips.end(); it != end; ++it)
      {
         WaveClip *clip = it->get();
         double offset = clip->GetOffset();
         if (maxOffset < offset)
            maxOffset = offset, rightmost = clip;
      }
      return rightmost;
   }
}

int WaveTrack::GetClipIndex(const WaveClip* clip) const
{
   int result;
   FindClip(mClips, clip, &result);
   return result;
}

WaveClip* WaveTrack::GetClipByIndex(int index)
{
   if(index < (int)mClips.size())
      return mClips[index].get();
   else
      return nullptr;
}

const WaveClip* WaveTrack::GetClipByIndex(int index) const
{
   return const_cast<WaveTrack&>(*this).GetClipByIndex(index);
}

int WaveTrack::GetNumClips() const
{
   return mClips.size();
}

bool WaveTrack::CanOffsetClip(WaveClip* clip, double amount,
                              double *allowedAmount /* = NULL */)
{
   if (allowedAmount)
      *allowedAmount = amount;

   for (const auto &c: mClips)
   {
      if (c.get() != clip && c->GetStartTime() < clip->GetEndTime()+amount &&
                       c->GetEndTime() > clip->GetStartTime()+amount)
      {
         if (!allowedAmount)
            return false; // clips overlap

         if (amount > 0)
         {
            if (c->GetStartTime()-clip->GetEndTime() < *allowedAmount)
               *allowedAmount = c->GetStartTime()-clip->GetEndTime();
            if (*allowedAmount < 0)
               *allowedAmount = 0;
         } else
         {
            if (c->GetEndTime()-clip->GetStartTime() > *allowedAmount)
               *allowedAmount = c->GetEndTime()-clip->GetStartTime();
            if (*allowedAmount > 0)
               *allowedAmount = 0;
         }
      }
   }

   if (allowedAmount)
   {
      if (*allowedAmount == amount)
         return true;

      // Check if the NEW calculated amount would not violate
      // any other constraint
      if (!CanOffsetClip(clip, *allowedAmount, NULL)) {
         *allowedAmount = 0; // play safe and don't allow anything
         return false;
      }
      else
         return true;
   } else
      return true;
}

bool WaveTrack::CanInsertClip(WaveClip* clip)
{
   for (const auto &c : mClips)
   {
      if (c->GetStartTime() < clip->GetEndTime() && c->GetEndTime() > clip->GetStartTime())
         return false; // clips overlap
   }

   return true;
}

bool WaveTrack::Split( double t0, double t1 )
{
   bool ret = SplitAt( t0 );
   if( ret && t0 != t1 )
      ret = SplitAt( t1 );
   return ret;
}

bool WaveTrack::SplitAt(double t)
{
   for (const auto &c : mClips)
   {
      if (c->WithinClip(t))
      {
         double val;
         t = LongSamplesToTime(TimeToLongSamples(t)); // put t on a sample
         val = c->GetEnvelope()->GetValue(t);
         //make two envelope points to preserve the value.
         //handle the case where we split on the 1st sample (without this we hit an assert)
         if(t - 1.0/c->GetRate() >= c->GetOffset())
            c->GetEnvelope()->Insert(t - c->GetOffset() - 1.0/c->GetRate(), val);  // frame end points
         c->GetEnvelope()->Insert(t - c->GetOffset(), val);
         auto newClip = make_movable<WaveClip>(*c, mDirManager);
         if (!c->Clear(t, c->GetEndTime()))
         {
            return false;
         }
         if (!newClip->Clear(c->GetStartTime(), t))
         {
            return false;
         }

         //offset the NEW clip by the splitpoint (noting that it is already offset to c->GetStartTime())
         sampleCount here = llrint(floor(((t - c->GetStartTime()) * mRate) + 0.5));
         newClip->Offset(here.as_double()/(double)mRate);
         // This could invalidate the iterators for the loop!  But we return
         // at once so it's okay
         mClips.push_back(std::move(newClip)); // transfer ownership
         return true;
      }
   }

   return true;
}

void WaveTrack::UpdateLocationsCache() const
{
   auto clips = SortedClipArray();

   mDisplayLocationsCache.clear();

   // Count number of display locations
   int num = 0;
   {
      const WaveClip *prev = nullptr;
      for (const auto clip : clips)
      {
         num += clip->NumCutLines();

         if (prev && fabs(prev->GetEndTime() -
                          clip->GetStartTime()) < WAVETRACK_MERGE_POINT_TOLERANCE)
            ++num;

         prev = clip;
      }
   }

   if (num == 0)
      return;

   // Alloc necessary number of display locations
   mDisplayLocationsCache.reserve(num);

   // Add all display locations to cache
   int curpos = 0;

   const WaveClip *previousClip = nullptr;
   for (const auto clip: clips)
   {
      for (const auto &cc : clip->GetCutLines())
      {
         // Add cut line expander point
         mDisplayLocationsCache.push_back(WaveTrackLocation{
            clip->GetOffset() + cc->GetOffset(),
            WaveTrackLocation::locationCutLine
         });
         curpos++;
      }

      if (previousClip)
      {
         if (fabs(previousClip->GetEndTime() - clip->GetStartTime())
                                          < WAVETRACK_MERGE_POINT_TOLERANCE)
         {
            // Add merge point
            mDisplayLocationsCache.push_back(WaveTrackLocation{
               previousClip->GetEndTime(),
               WaveTrackLocation::locationMergePoint,
               GetClipIndex(previousClip),
               GetClipIndex(clip)
            });
            curpos++;
         }
      }

      previousClip = clip;
   }

   wxASSERT(curpos == num);
}

// Expand cut line (that is, re-insert audio, then DELETE audio saved in cut line)
bool WaveTrack::ExpandCutLine(double cutLinePosition, double* cutlineStart,
                              double* cutlineEnd)
{
   bool editClipCanMove = true;
   gPrefs->Read(wxT("/GUI/EditClipCanMove"), &editClipCanMove);

   // Find clip which contains this cut line
   for (const auto &clip : mClips)
   {
      double start = 0, end = 0;

      if (clip->FindCutLine(cutLinePosition, &start, &end))
      {
         if (!editClipCanMove)
         {
            // We are not allowed to move the other clips, so see if there
            // is enough room to expand the cut line
            for (const auto &clip2: mClips)
            {
               if (clip2->GetStartTime() > clip->GetStartTime() &&
                   clip->GetEndTime() + end - start > clip2->GetStartTime())
               {
                  wxMessageBox(
                     _("There is not enough room available to expand the cut line"),
                     _("Error"), wxICON_STOP);
                  return false;
               }
            }
         }

         if (!clip->ExpandCutLine(cutLinePosition))
            return false;

         if (cutlineStart)
            *cutlineStart = start;
         if (cutlineEnd)
            *cutlineEnd = end;

         // Move clips which are to the right of the cut line
         if (editClipCanMove)
         {
            for (const auto &clip2 : mClips)
            {
               if (clip2->GetStartTime() > clip->GetStartTime())
                  clip2->Offset(end - start);
            }
         }

         return true;
      }
   }

   return false;
}

bool WaveTrack::RemoveCutLine(double cutLinePosition)
{
   for (const auto &clip : mClips)
      if (clip->RemoveCutLine(cutLinePosition))
         return true;

   return false;
}

bool WaveTrack::MergeClips(int clipidx1, int clipidx2)
{
   WaveClip* clip1 = GetClipByIndex(clipidx1);
   WaveClip* clip2 = GetClipByIndex(clipidx2);

   if (!clip1 || !clip2) // Could happen if one track of a linked pair had a split and the other didn't.
      return false;

   // Append data from second clip to first clip
   if (!clip1->Paste(clip1->GetEndTime(), clip2))
      return false;

   // Delete second clip
   auto it = FindClip(mClips, clip2);
   mClips.erase(it);

   return true;
}

bool WaveTrack::Resample(int rate, ProgressDialog *progress)
{
   for (const auto &clip : mClips)
      if (!clip->Resample(rate, progress))
      {
         wxLogDebug( wxT("Resampling problem!  We're partially resampled") );
         // FIXME: The track is now in an inconsistent state since some
         //        clips are resampled and some are not
         return false;
      }

   mRate = rate;

   return true;
}

namespace {
   template < typename Cont1, typename Cont2 >
   Cont1 FillSortedClipArray(const Cont2& mClips)
   {
      Cont1 clips;
      for (const auto &clip : mClips)
         clips.push_back(clip.get());
      std::sort(clips.begin(), clips.end(),
         [](const WaveClip *a, const WaveClip *b)
      { return a->GetStartTime() < b->GetStartTime(); });
      return clips;
   }
}

WaveClipPointers WaveTrack::SortedClipArray()
{
   return FillSortedClipArray<WaveClipPointers>(mClips);
}

WaveClipConstPointers WaveTrack::SortedClipArray() const
{
   return FillSortedClipArray<WaveClipConstPointers>(mClips);
}

///Deletes all clips' wavecaches.  Careful, This may not be threadsafe.
void WaveTrack::ClearWaveCaches()
{
   for (const auto &clip : mClips)
      clip->ClearWaveCache();
}

///Adds an invalid region to the wavecache so it redraws that portion only.
void WaveTrack::AddInvalidRegion(sampleCount startSample, sampleCount endSample)
{
   for (const auto &clip : mClips)
      clip->AddInvalidRegion(startSample, endSample);
}

int WaveTrack::GetAutoSaveIdent()
{
   return mAutoSaveIdent;
}

void WaveTrack::SetAutoSaveIdent(int ident)
{
   mAutoSaveIdent = ident;
}

WaveTrackCache::~WaveTrackCache()
{
   Free();
}

void WaveTrackCache::SetTrack(const WaveTrack *pTrack)
{
   if (mPTrack != pTrack) {
      if (pTrack) {
         mBufferSize = pTrack->GetMaxBlockSize();
         if (!mPTrack ||
             mPTrack->GetMaxBlockSize() != mBufferSize) {
            Free();
            mBuffers[0].data = new float[mBufferSize];
            mBuffers[1].data = new float[mBufferSize];
         }
      }
      else
         Free();
      mPTrack = pTrack;
      mNValidBuffers = 0;
   }
}

constSamplePtr WaveTrackCache::Get(sampleFormat format,
   sampleCount start, size_t len)
{
   if (format == floatSample && len > 0) {
      const auto end = start + len;

      bool fillFirst = (mNValidBuffers < 1);
      bool fillSecond = (mNValidBuffers < 2);

      // Discard cached results that we no longer need
      if (mNValidBuffers > 0 &&
          (end <= mBuffers[0].start ||
           start >= mBuffers[mNValidBuffers - 1].end())) {
         // Complete miss
         fillFirst = true;
         fillSecond = true;
      }
      else if (mNValidBuffers == 2 &&
               start >= mBuffers[1].start &&
               end > mBuffers[1].end()) {
         // Request starts in the second buffer and extends past it.
         // Discard the first buffer.
         // (But don't deallocate the buffer space.)
         float *save = mBuffers[0].data;
         mBuffers[0] = mBuffers[1];
         mBuffers[1].data = save;
         fillSecond = true;
         mNValidBuffers = 1;
      }
      else if (mNValidBuffers > 0 &&
         start < mBuffers[0].start &&
         0 <= mPTrack->GetBlockStart(start)) {
         // Request is not a total miss but starts before the cache,
         // and there is a clip to fetch from.
         // Not the access pattern for drawing spectrogram or playback,
         // but maybe scrubbing causes this.
         // Move the first buffer into second place, and later
         // refill the first.
         // (This case might be useful when marching backwards through
         // the track, as with scrubbing.)
         float *save = mBuffers[1].data;
         mBuffers[1] = mBuffers[0];
         mBuffers[0].data = save;
         fillFirst = true;
         fillSecond = false;
         // Cache is not in a consistent state yet
         mNValidBuffers = 0;
      }

      // Refill buffers as needed
      if (fillFirst) {
         const auto start0 = mPTrack->GetBlockStart(start);
         if (start0 >= 0) {
            const auto len0 = mPTrack->GetBestBlockSize(start0);
            wxASSERT(len0 <= mBufferSize);
            if (!mPTrack->Get(samplePtr(mBuffers[0].data), floatSample, start0, len0))
               return 0;
            mBuffers[0].start = start0;
            mBuffers[0].len = len0;
            if (!fillSecond &&
                mBuffers[0].end() != mBuffers[1].start)
               fillSecond = true;
            // Keep the partially updated state consistent:
            mNValidBuffers = fillSecond ? 1 : 2;
         }
         else {
            // Request may fall between the clips of a track.
            // Invalidate all.  WaveTrack::Get() will return zeroes.
            mNValidBuffers = 0;
            fillSecond = false;
         }
      }
      wxASSERT(!fillSecond || mNValidBuffers > 0);
      if (fillSecond) {
         mNValidBuffers = 1;
         const auto end0 = mBuffers[0].end();
         if (end > end0) {
            const auto start1 = mPTrack->GetBlockStart(end0);
            if (start1 == end0) {
               const auto len1 = mPTrack->GetBestBlockSize(start1);
               wxASSERT(len1 <= mBufferSize);
               if (!mPTrack->Get(samplePtr(mBuffers[1].data), floatSample, start1, len1))
                  return 0;
               mBuffers[1].start = start1;
               mBuffers[1].len = len1;
               mNValidBuffers = 2;
            }
         }
      }
      wxASSERT(mNValidBuffers < 2 || mBuffers[0].end() == mBuffers[1].start);

      samplePtr buffer = 0;
      auto remaining = len;

      // Possibly get an initial portion that is uncached

      // This may be negative
      const auto initLen =
         mNValidBuffers < 1 ? sampleCount( len )
            : std::min(sampleCount( len ), mBuffers[0].start - start);

      if (initLen > 0) {
         // This might be fetching zeroes between clips
         mOverlapBuffer.Resize(len, format);
         // initLen is not more than len:
         auto sinitLen = initLen.as_size_t();
         if (!mPTrack->Get(mOverlapBuffer.ptr(), format, start, sinitLen))
            return 0;
         wxASSERT( sinitLen <= remaining );
         remaining -= sinitLen;
         start += initLen;
         buffer = mOverlapBuffer.ptr() + sinitLen * SAMPLE_SIZE(format);
      }

      // Now satisfy the request from the buffers
      for (int ii = 0; ii < mNValidBuffers && remaining > 0; ++ii) {
         const auto starti = start - mBuffers[ii].start;
         // Treatment of initLen above establishes this loop invariant,
         // and statements below preserve it:
         wxASSERT(starti >= 0);

         // This may be negative
         const auto leni =
            std::min( sampleCount( remaining ), mBuffers[ii].len - starti );
         if (initLen <= 0 && leni == len) {
            // All is contiguous already.  We can completely avoid copying
            // leni is nonnegative, therefore start falls within mBuffers[ii],
            // so starti is bounded between 0 and buffer length
            return samplePtr(mBuffers[ii].data + starti.as_size_t() );
         }
         else if (leni > 0) {
            // leni is nonnegative, therefore start falls within mBuffers[ii]
            // But we can't satisfy all from one buffer, so copy
            if (buffer == 0) {
               mOverlapBuffer.Resize(len, format);
               buffer = mOverlapBuffer.ptr();
            }
            // leni is positive and not more than remaining
            const size_t size = sizeof(float) * leni.as_size_t();
            // starti is less than mBuffers[ii].len and nonnegative
            memcpy(buffer, mBuffers[ii].data + starti.as_size_t(), size);
            wxASSERT( leni <= remaining );
            remaining -= leni.as_size_t();
            start += leni;
            buffer += size;
         }
      }

      if (remaining > 0) {
         // Very big request!
         // Fall back to direct fetch
         if (buffer == 0) {
            mOverlapBuffer.Resize(len, format);
            buffer = mOverlapBuffer.ptr();
         }
         if (!mPTrack->Get(buffer, format, start, remaining))
            return 0;
      }

      return mOverlapBuffer.ptr();
   }

   // Cache works only for float format.
   mOverlapBuffer.Resize(len, format);
   if (mPTrack->Get(mOverlapBuffer.ptr(), format, start, len))
      return mOverlapBuffer.ptr();
   else
      return 0;
}

void WaveTrackCache::Free()
{
   mBuffers[0].Free();
   mBuffers[1].Free();
   mOverlapBuffer.Free();
   mNValidBuffers = 0;
}
