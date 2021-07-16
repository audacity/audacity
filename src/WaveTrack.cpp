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

*//****************************************************************/

/*!
@class WaveTrackFactory
@brief Used to create or clone a WaveTrack, with appropriate context
from the project that will own the track.
*/


#include "WaveTrack.h"



#include "WaveClip.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/debug.h>

#include <float.h>
#include <math.h>
#include <algorithm>

#include "float_cast.h"

#include "Envelope.h"
#include "Sequence.h"

#include "ProjectFileIORegistry.h"
#include "ProjectSettings.h"

#include "Prefs.h"

#include "effects/TimeWarper.h"
#include "prefs/QualitySettings.h"
#include "prefs/SpectrogramSettings.h"
#include "prefs/TracksPrefs.h"
#include "prefs/TracksBehaviorsPrefs.h"
#include "prefs/WaveformSettings.h"

#include "InconsistencyException.h"

#include "tracks/ui/TrackView.h"
#include "tracks/ui/TrackControls.h"

using std::max;

static ProjectFileIORegistry::Entry registerFactory{
   wxT( "wavetrack" ),
   []( AudacityProject &project ){
      auto &trackFactory = WaveTrackFactory::Get( project );
      auto &tracks = TrackList::Get( project );
      auto result = tracks.Add(trackFactory.NewWaveTrack());
      TrackView::Get( *result );
      TrackControls::Get( *result );
      return result;
   }
};

WaveTrack::Holder WaveTrackFactory::DuplicateWaveTrack(const WaveTrack &orig)
{
   return std::static_pointer_cast<WaveTrack>( orig.Duplicate() );
}


WaveTrack::Holder WaveTrackFactory::NewWaveTrack(sampleFormat format, double rate)
{
   if (format == (sampleFormat)0)
      format = QualitySettings::SampleFormatChoice();
   if (rate == 0)
      rate = mSettings.GetRate();
   return std::make_shared<WaveTrack> ( mpFactory, format, rate );
}

WaveTrack::WaveTrack( const SampleBlockFactoryPtr &pFactory,
   sampleFormat format, double rate )
   : PlayableTrack()
   , mpFactory(pFactory)
{
   mLegacyProjectFileOffset = 0;

   mFormat = format;
   mRate = (int) rate;
   mGain = 1.0;
   mPan = 0.0;
   mOldGain[0] = 0.0;
   mOldGain[1] = 0.0;
   mWaveColorIndex = 0;
   SetDefaultName(TracksPrefs::GetDefaultAudioTrackNamePreference());
   SetName(GetDefaultName());
   mDisplayMin = -1.0;
   mDisplayMax = 1.0;
   mSpectrumMin = mSpectrumMax = -1; // so values will default to settings
   mLastScaleType = -1;
   mLastdBRange = -1;
}

WaveTrack::WaveTrack(const WaveTrack &orig):
   PlayableTrack(orig)
   , mpFactory( orig.mpFactory )
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
      mClips.push_back
         ( std::make_unique<WaveClip>( *clip, mpFactory, true ) );
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack &orig)
{
   PlayableTrack::Init(orig);
   mpFactory = orig.mpFactory;
   
   mFormat = orig.mFormat;
   mWaveColorIndex = orig.mWaveColorIndex;
   mRate = orig.mRate;
   mGain = orig.mGain;
   mPan = orig.mPan;
   mOldGain[0] = 0.0;
   mOldGain[1] = 0.0;
   SetDefaultName(orig.GetDefaultName());
   SetName(orig.GetName());
   mDisplayMin = orig.mDisplayMin;
   mDisplayMax = orig.mDisplayMax;
   mSpectrumMin = orig.mSpectrumMin;
   mSpectrumMax = orig.mSpectrumMax;
   mDisplayLocationsCache.clear();
}

void WaveTrack::Reinit(const WaveTrack &orig)
{
   Init(orig);

   {
      auto &settings = orig.mpSpectrumSettings;
      if (settings)
         mpSpectrumSettings = std::make_unique<SpectrogramSettings>(*settings);
      else
         mpSpectrumSettings.reset();
   }

   {
      auto &settings = orig.mpWaveformSettings;
      if (settings)
         mpWaveformSettings = std::make_unique<WaveformSettings>(*settings);
      else
         mpWaveformSettings.reset();
   }
}

void WaveTrack::Merge(const Track &orig)
{
   orig.TypeSwitch( [&](const WaveTrack *pwt) {
      const WaveTrack &wt = *pwt;
      mGain    = wt.mGain;
      mPan     = wt.mPan;
      mDisplayMin = wt.mDisplayMin;
      mDisplayMax = wt.mDisplayMax;
      SetSpectrogramSettings(wt.mpSpectrumSettings
         ? std::make_unique<SpectrogramSettings>(*wt.mpSpectrumSettings) : nullptr);
      SetWaveformSettings
         (wt.mpWaveformSettings ? std::make_unique<WaveformSettings>(*wt.mpWaveformSettings) : nullptr);
   });
   PlayableTrack::Merge(orig);
}

WaveTrack::~WaveTrack()
{
}

double WaveTrack::GetOffset() const
{
   return GetStartTime();
}

/*! @excsafety{No-fail} */
void WaveTrack::SetOffset(double o)
{
   double delta = o - GetOffset();

   for (const auto &clip : mClips)
      // assume No-fail-guarantee
      clip->SetOffset(clip->GetOffset() + delta);

   mOffset = o;
}

auto WaveTrack::GetChannelIgnoringPan() const -> ChannelType {
   return mChannel;
}

auto WaveTrack::GetChannel() const -> ChannelType
{
   if( mChannel != Track::MonoChannel )
      return mChannel; 
   auto pan = GetPan();
   if( pan < -0.99 )
      return Track::LeftChannel;
   if( pan >  0.99 )
      return Track::RightChannel;
   return mChannel;
}

void WaveTrack::SetPanFromChannelType()
{ 
   if( mChannel == Track::LeftChannel )
      SetPan( -1.0f );
   else if( mChannel == Track::RightChannel )
      SetPan( 1.0f );
};

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

int WaveTrack::ZeroLevelYCoordinate(wxRect rect) const
{
   return rect.GetTop() +
      (int)((mDisplayMax / (mDisplayMax - mDisplayMin)) * rect.height);
}

template< typename Container >
static Container MakeIntervals(const std::vector<WaveClipHolder> &clips)
{
   Container result;
   for (const auto &clip: clips) {
      result.emplace_back( clip->GetStartTime(), clip->GetEndTime(),
         std::make_unique<WaveTrack::IntervalData>( clip ) );
   }
   return result;
}

Track::Holder WaveTrack::PasteInto( AudacityProject &project ) const
{
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto &pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
   auto pNewTrack = EmptyCopy( pSampleBlockFactory );
   pNewTrack->Paste(0.0, this);
   return pNewTrack;
}

auto WaveTrack::GetIntervals() const -> ConstIntervals
{
   return MakeIntervals<ConstIntervals>( mClips );
}

auto WaveTrack::GetIntervals() -> Intervals
{
   return MakeIntervals<Intervals>( mClips );
}

Track::Holder WaveTrack::Clone() const
{
   return std::make_shared<WaveTrack>( *this );
}

double WaveTrack::GetRate() const
{
   return mRate;
}

void WaveTrack::SetRate(double newRate)
{
   wxASSERT( newRate > 0 );
   newRate = std::max( 1.0, newRate );
   auto ratio = mRate / newRate;
   mRate = (int) newRate;
   for (const auto &clip : mClips) {
      clip->SetRate((int)newRate);
      clip->SetOffset( clip->GetOffset() * ratio );
   }
}

float WaveTrack::GetGain() const
{
   return mGain;
}

void WaveTrack::SetGain(float newGain)
{
   if (mGain != newGain) {
      mGain = newGain;
      Notify();
   }
}

float WaveTrack::GetPan() const
{
   return mPan;
}

void WaveTrack::SetPan(float newPan)
{
   if (newPan > 1.0)
      newPan = 1.0;
   else if (newPan < -1.0)
      newPan = -1.0;

   if ( mPan != newPan ) {
      mPan = newPan;
      Notify();
   }
}

float WaveTrack::GetChannelGain(int channel) const
{
   float left = 1.0;
   float right = 1.0;

   if (mPan < 0)
      right = (mPan + 1.0);
   else if (mPan > 0)
      left = 1.0 - mPan;

   if ((channel%2) == 0)
      return left*mGain;
   else
      return right*mGain;
}

float WaveTrack::GetOldChannelGain(int channel) const
{
   return mOldGain[channel%2];
}

void WaveTrack::SetOldChannelGain(int channel, float gain)
{
   mOldGain[channel % 2] = gain;
}



/*! @excsafety{Strong} */
void WaveTrack::SetWaveColorIndex(int colorIndex)
{
   for (const auto &clip : mClips)
      clip->SetColourIndex( colorIndex );
   mWaveColorIndex = colorIndex;
}

sampleCount WaveTrack::GetNumSamples() const
{
   sampleCount result{ 0 };

   for (const auto& clip : mClips)
      result += clip->GetNumSamples();

   return result;
}

/*! @excsafety{Weak} -- Might complete on only some clips */
void WaveTrack::ConvertToSampleFormat(sampleFormat format,
   const std::function<void(size_t)> & progressReport)
{
   for (const auto& clip : mClips)
      clip->ConvertToSampleFormat(format, progressReport);
   mFormat = format;
}


bool WaveTrack::IsEmpty(double t0, double t1) const
{
   if (t0 > t1)
      return true;

   //wxPrintf("Searching for overlap in %.6f...%.6f\n", t0, t1);
   for (const auto &clip : mClips)
   {
      if (!clip->BeforeClip(t1) && !clip->AfterClip(t0)) {
         //wxPrintf("Overlapping clip: %.6f...%.6f\n",
         //       clip->GetStartTime(),
         //       clip->GetEndTime());
         // We found a clip that overlaps this region
         return false;
      }
   }
   //wxPrintf("No overlap found\n");

   // Otherwise, no clips overlap this region
   return true;
}

Track::Holder WaveTrack::Cut(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto tmp = Copy(t0, t1);

   Clear(t0, t1);

   return tmp;
}

/*! @excsafety{Strong} */
Track::Holder WaveTrack::SplitCut(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   // SplitCut is the same as 'Copy', then 'SplitDelete'
   auto tmp = Copy(t0, t1);

   SplitDelete(t0, t1);

   return tmp;
}

#if 0
Track::Holder WaveTrack::CutAndAddCutLine(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   // Cut is the same as 'Copy', then 'Delete'
   auto tmp = Copy(t0, t1);

   ClearAndAddCutLine(t0, t1);

   return tmp;
}
#endif



//Trim trims within a clip, rather than trimming everything.
//If a bound is outside a clip, it trims everything.
/*! @excsafety{Weak} */
void WaveTrack::Trim (double t0, double t1)
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
         clip->Clear(t1,clip->GetEndTime());
         inside1 = true;
      }

      if(t0 > clip->GetStartTime() && t0 < clip->GetEndTime())
      {
         clip->Clear(clip->GetStartTime(),t0);
         clip->SetOffset(t0);
         inside0 = true;
      }
   }

   //if inside0 is false, then the left selector was between
   //clips, so DELETE everything to its left.
   if(!inside1 && t1 < GetEndTime())
      Clear(t1,GetEndTime());

   if(!inside0 && t0 > GetStartTime())
      SplitDelete(GetStartTime(), t0);
}




WaveTrack::Holder WaveTrack::EmptyCopy(
   const SampleBlockFactoryPtr &pFactory ) const
{
   auto result = std::make_shared<WaveTrack>( pFactory, mFormat, mRate );
   result->Init(*this);
   result->mpFactory = pFactory ? pFactory : mpFactory;
   return result;
}

Track::Holder WaveTrack::Copy(double t0, double t1, bool forClipboard) const
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto result = EmptyCopy();
   WaveTrack *newTrack = result.get();

   // PRL:  Why shouldn't cutlines be copied and pasted too?  I don't know, but
   // that was the old behavior.  But this function is also used by the
   // Duplicate command and I changed its behavior in that case.

   for (const auto &clip : mClips)
   {
      if (t0 <= clip->GetStartTime() && t1 >= clip->GetEndTime())
      {
         // Whole clip is in copy region
         //wxPrintf("copy: clip %i is in copy region\n", (int)clip);

         newTrack->mClips.push_back
            (std::make_unique<WaveClip>(*clip, mpFactory, ! forClipboard));
         WaveClip *const newClip = newTrack->mClips.back().get();
         newClip->Offset(-t0);
      }
      else if (t1 > clip->GetStartTime() && t0 < clip->GetEndTime())
      {
         // Clip is affected by command
         //wxPrintf("copy: clip %i is affected by command\n", (int)clip);

         const double clip_t0 = std::max(t0, clip->GetStartTime());
         const double clip_t1 = std::min(t1, clip->GetEndTime());

         auto newClip = std::make_unique<WaveClip>
            (*clip, mpFactory, ! forClipboard, clip_t0, clip_t1);

         //wxPrintf("copy: clip_t0=%f, clip_t1=%f\n", clip_t0, clip_t1);

         newClip->Offset(-t0);
         if (newClip->GetOffset() < 0)
            newClip->SetOffset(0);

         newTrack->mClips.push_back(std::move(newClip)); // transfer ownership
      }
   }

   // AWD, Oct 2009: If the selection ends in whitespace, create a placeholder
   // clip representing that whitespace
   // PRL:  Only if we want the track for pasting into other tracks.  Not if it
   // goes directly into a project as in the Duplicate command.
   if (forClipboard &&
       newTrack->GetEndTime() + 1.0 / newTrack->GetRate() < t1 - t0)
   {
      auto placeholder = std::make_unique<WaveClip>(mpFactory,
            newTrack->GetSampleFormat(),
            static_cast<int>(newTrack->GetRate()),
            0 /*colourindex*/);
      placeholder->SetIsPlaceholder(true);
      placeholder->InsertSilence(0, (t1 - t0) - newTrack->GetEndTime());
      placeholder->Offset(newTrack->GetEndTime());
      newTrack->mClips.push_back(std::move(placeholder)); // transfer ownership
   }

   return result;
}

Track::Holder WaveTrack::CopyNonconst(double t0, double t1)
{
   return Copy(t0, t1);
}

/*! @excsafety{Strong} */
void WaveTrack::Clear(double t0, double t1)
{
   HandleClear(t0, t1, false, false);
}

/*! @excsafety{Strong} */
void WaveTrack::ClearAndAddCutLine(double t0, double t1)
{
   HandleClear(t0, t1, true, false);
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

void WaveTrack::UseSpectralPrefs( bool bUse )
{  
   if( bUse ){
      if( !mpSpectrumSettings )
         return;
      // reset it, and next we will be getting the defaults.
      mpSpectrumSettings.reset();
   }
   else {
      if( mpSpectrumSettings )
         return;
      GetIndependentSpectrogramSettings();
   }
}



const WaveformSettings &WaveTrack::GetWaveformSettings() const
{
   // Create on demand
   return const_cast<WaveTrack*>(this)->GetWaveformSettings();
}

WaveformSettings &WaveTrack::GetWaveformSettings()
{
   // Create on demand
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
// HandleClear() removes any cut/split lines with the
// cleared range, but, in most cases, effects want to preserve
// the existing cut/split lines, so they are saved before the
// HandleClear()/Paste() and restored after.
//
// If the pasted track overlaps two or more clips, then it will
// be pasted with visible split lines.  Normally, effects do not
// want these extra lines, so they may be merged out.
//
/*! @excsafety{Weak} -- This WaveTrack remains destructible in case of AudacityException.
But some of its cutline clips may have been destroyed. */
void WaveTrack::ClearAndPaste(double t0, // Start of time to clear
                              double t1, // End of time to clear
                              const Track *src, // What to paste
                              bool preserve, // Whether to reinsert splits/cuts
                              bool merge, // Whether to remove 'extra' splits
                              const TimeWarper *effectWarper // How does time change
                              )
{
   double dur = std::min(t1 - t0, src->GetEndTime());

   // If duration is 0, then it's just a plain paste
   if (dur == 0.0) {
      // use Weak-guarantee
      Paste(t0, src);
      return;
   }

   std::vector<EnvPoint> envPoints;
   std::vector<double> splits;
   WaveClipHolders cuts;

   // If provided time warper was NULL, use a default one that does nothing
   IdentityTimeWarper localWarper;
   const TimeWarper *warper = (effectWarper ? effectWarper : &localWarper);

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
      if (st >= t0 && st <= t1 && !make_iterator_range(splits).contains(st)) {
         splits.push_back(st);
      }

      st = LongSamplesToTime(TimeToLongSamples(clip->GetEndTime()));
      if (st >= t0 && st <= t1 && !make_iterator_range(splits).contains(st)) {
         splits.push_back(st);
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

      // Save the envelope points
      const auto &env = *clip->GetEnvelope();
      for (size_t i = 0, numPoints = env.GetNumberOfPoints(); i < numPoints; ++i) {
         envPoints.push_back(env[i]);
      }
   }

   const auto tolerance = 2.0 / GetRate();

   // Now, clear the selection
   HandleClear(t0, t1, false, false);
   {
      // And paste in the NEW data
      Paste(t0, src);
      {
         // First, merge the NEW clip(s) in with the existing clips
         if (merge && splits.size() > 0)
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
                  if (fabs(t1 - clip->GetStartTime()) < tolerance) {
                     if (prev)
                        MergeClips(GetClipIndex(prev), GetClipIndex(clip));
                     break;
                  }
                  prev = clip;
               }
            }
         }

         // Refill the array since clips have changed.
         auto clips = SortedClipArray();

         {
            // Scan the sorted clips to look for the start of the pasted
            // region.
            WaveClip *prev = nullptr;
            for (const auto clip : clips) {
               if (prev) {
                  // It must be that clip is what was pasted and it begins where
                  // prev ends.
                  // use Weak-guarantee
                  MergeClips(GetClipIndex(prev), GetClipIndex(clip));
                  break;
               }
               if (fabs(t0 - clip->GetEndTime()) < tolerance)
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

      // Restore the envelope points
      for (auto point : envPoints) {
         auto t = warper->Warp(point.GetT());
         if (auto clip = GetClipAtTime(t))
            clip->GetEnvelope()->Insert(t, point.GetVal());
      }
   }
}

/*! @excsafety{Strong} */
void WaveTrack::SplitDelete(double t0, double t1)
{
   bool addCutLines = false;
   bool split = true;
   HandleClear(t0, t1, addCutLines, split);
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

std::shared_ptr<WaveClip> WaveTrack::RemoveAndReturnClip(WaveClip* clip)
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

bool WaveTrack::AddClip(const std::shared_ptr<WaveClip> &clip)
{
   if (clip->GetSequence()->GetFactory() != this->mpFactory)
      return false;

   // Uncomment the following line after we correct the problem of zero-length clips
   //if (CanInsertClip(clip))
      mClips.push_back(clip); // transfer ownership

   return true;
}

/*! @excsafety{Strong} */
void WaveTrack::HandleClear(double t0, double t1,
                            bool addCutLines, bool split)
{
   // For debugging, use an ASSERT so that we stop
   // closer to the problem.
   wxASSERT( t1 >= t0 );
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   bool editClipCanMove = GetEditClipsCanMove();

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
            // Don't modify this clip in place, because we want a strong
            // guarantee, and might modify another clip
            clipsToDelete.push_back( clip.get() );
            auto newClip = std::make_unique<WaveClip>( *clip, mpFactory, true );
            newClip->ClearAndAddCutLine( t0, t1 );
            clipsToAdd.push_back( std::move( newClip ) );
         }
         else
         {
            if (split) {
               // Three cases:

               if (clip->BeforeClip(t0)) {
                  // Delete from the left edge

                  // Don't modify this clip in place, because we want a strong
                  // guarantee, and might modify another clip
                  clipsToDelete.push_back( clip.get() );
                  auto newClip = std::make_unique<WaveClip>( *clip, mpFactory, true );
                  newClip->Clear(clip->GetStartTime(), t1);
                  newClip->Offset(t1-clip->GetStartTime());

                  clipsToAdd.push_back( std::move( newClip ) );
               }
               else if (clip->AfterClip(t1)) {
                  // Delete to right edge

                  // Don't modify this clip in place, because we want a strong
                  // guarantee, and might modify another clip
                  clipsToDelete.push_back( clip.get() );
                  auto newClip = std::make_unique<WaveClip>( *clip, mpFactory, true );
                  newClip->Clear(t0, clip->GetEndTime());

                  clipsToAdd.push_back( std::move( newClip ) );
               }
               else {
                  // Delete in the middle of the clip...we actually create two
                  // NEW clips out of the left and right halves...

                  // left
                  clipsToAdd.push_back
                     ( std::make_unique<WaveClip>( *clip, mpFactory, true ) );
                  clipsToAdd.back()->Clear(t0, clip->GetEndTime());

                  // right
                  clipsToAdd.push_back
                     ( std::make_unique<WaveClip>( *clip, mpFactory, true ) );
                  WaveClip *const right = clipsToAdd.back().get();
                  right->Clear(clip->GetStartTime(), t1);
                  right->Offset(t1 - clip->GetStartTime());

                  clipsToDelete.push_back(clip.get());
               }
            }
            else {
               // (We are not doing a split cut)

               // Don't modify this clip in place, because we want a strong
               // guarantee, and might modify another clip
               clipsToDelete.push_back( clip.get() );
               auto newClip = std::make_unique<WaveClip>( *clip, mpFactory, true );

               // clip->Clear keeps points < t0 and >= t1 via Envelope::CollapseRegion
               newClip->Clear(t0,t1);

               clipsToAdd.push_back( std::move( newClip ) );
            }
         }
      }
   }

   // Only now, change the contents of this track
   // use No-fail-guarantee for the rest

   for (const auto &clip : mClips)
   {
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
}

void WaveTrack::SyncLockAdjust(double oldT1, double newT1)
{
   if (newT1 > oldT1) {
      // Insert space within the track

      // JKC: This is a rare case where using >= rather than > on a float matters.
      // GetEndTime() looks through the clips and may give us EXACTLY the same
      // value as T1, when T1 was set to be at the end of one of those clips.
      if (oldT1 >= GetEndTime())
         return;

      // If track is empty at oldT1 insert whitespace; otherwise, silence
      if (IsEmpty(oldT1, oldT1))
      {
         // Check if clips can move
         bool clipsCanMove = true;
         gPrefs->Read(wxT("/GUI/EditClipCanMove"), &clipsCanMove);
         if (clipsCanMove) {
            auto tmp = Cut (oldT1, GetEndTime() + 1.0/GetRate());

            Paste(newT1, tmp.get());
         }
         return;
      }
      else {
         // AWD: Could just use InsertSilence() on its own here, but it doesn't
         // follow EditClipCanMove rules (Paste() does it right)
         auto tmp = std::make_shared<WaveTrack>(
            mpFactory, GetSampleFormat(), GetRate() );

         tmp->InsertSilence(0.0, newT1 - oldT1);
         tmp->Flush();
         Paste(oldT1, tmp.get());
      }
   }
   else if (newT1 < oldT1) {
      Clear(newT1, oldT1);
   }
}

/*! @excsafety{Weak} */
void WaveTrack::Paste(double t0, const Track *src)
{
   bool editClipCanMove = GetEditClipsCanMove();

   bool bOk = src && src->TypeSwitch< bool >( [&](const WaveTrack *other) {

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
      // clips, and the current clip is split, when necessary. This may seem
      // strange at first, but it probably is better than trying to auto-merge
      // anything. The user can still merge the clips by hand (which should be a
      // simple command reachable by a hotkey or single mouse click).
      //

      if (other->GetNumClips() == 0)
         return true;

      //wxPrintf("paste: we have at least one clip\n");

      bool singleClipMode = (other->GetNumClips() == 1 &&
            other->GetStartTime() == 0.0);

      const double insertDuration = other->GetEndTime();
      if( insertDuration != 0 && insertDuration < 1.0/mRate )
         // PRL:  I added this check to avoid violations of preconditions in other WaveClip and Sequence
         // methods, but allow the value 0 so I don't subvert the purpose of commit
         // 739422ba70ceb4be0bb1829b6feb0c5401de641e which causes append-recording always to make
         // a new clip.
         return true;

      //wxPrintf("Check if we need to make room for the pasted data\n");

      // Make room for the pasted data
      if (editClipCanMove) {
         if (!singleClipMode) {
            // We need to insert multiple clips, so split the current clip and
            // move everything to the right, then try to paste again
            if (!IsEmpty(t0, GetEndTime())) {
               auto tmp = Cut(t0, GetEndTime()+1.0/mRate);
               Paste(t0 + insertDuration, tmp.get());
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
         // wxPrintf("paste: checking for single clip mode!\n");

         WaveClip *insideClip = NULL;

         for (const auto &clip : mClips)
         {
            if (editClipCanMove)
            {
               if (clip->WithinClip(t0))
               {
                  //wxPrintf("t0=%.6f: inside clip is %.6f ... %.6f\n",
                  //       t0, clip->GetStartTime(), clip->GetEndTime());
                  insideClip = clip.get();
                  break;
               }
            }
            else
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
            //wxPrintf("paste: traditional behaviour\n");
            if (!editClipCanMove)
            {
               // We did not move other clips out of the way already, so
               // check if we can paste without having to move other clips
               for (const auto &clip : mClips)
               {
                  if (clip->GetStartTime() > insideClip->GetStartTime() &&
                      insideClip->GetEndTime() + insertDuration >
                                                         clip->GetStartTime())
                     // Strong-guarantee in case of this path
                     // not that it matters.
                     throw SimpleMessageBoxException{
                        ExceptionType::BadUserAction,
                        XO("There is not enough room available to paste the selection"),
                        XO("Warning"),
                        "Error:_Insufficient_space_in_track"
                     };
               }
            }

            insideClip->Paste(t0, other->GetClipByIndex(0));
            return true;
         }

         // Just fall through and exhibit NEW behaviour

      }

      // Insert NEW clips
      //wxPrintf("paste: multi clip mode!\n");

      if (!editClipCanMove && !IsEmpty(t0, t0+insertDuration-1.0/mRate))
         // Strong-guarantee in case of this path
         // not that it matters.
         throw SimpleMessageBoxException{
            ExceptionType::BadUserAction,
            XO("There is not enough room available to paste the selection"),
            XO("Warning"),
            "Error:_Insufficient_space_in_track"
         };

      for (const auto &clip : other->mClips)
      {
         // AWD Oct. 2009: Don't actually paste in placeholder clips
         if (!clip->GetIsPlaceholder())
         {
            auto newClip =
               std::make_unique<WaveClip>( *clip, mpFactory, true );
            newClip->Resample(mRate);
            newClip->Offset(t0);
            newClip->MarkChanged();
            mClips.push_back(std::move(newClip)); // transfer ownership
         }
      }
      return true;
   } );

   if( !bOk )
      // THROW_INCONSISTENCY_EXCEPTION; // ?
      (void)0;// Empty if intentional.
}

void WaveTrack::Silence(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto start = (sampleCount)floor(t0 * mRate + 0.5);
   auto len = (sampleCount)floor(t1 * mRate + 0.5) - start;

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

         clip->GetSequence()->SetSilence(inclipDelta, samplesToCopy);
         clip->MarkChanged();
      }
   }
}

/*! @excsafety{Strong} */
void WaveTrack::InsertSilence(double t, double len)
{
   // Nothing to do, if length is zero.
   // Fixes Bug 1626
   if( len == 0 )
      return;
   if (len <= 0)
      THROW_INCONSISTENCY_EXCEPTION;

   if (mClips.empty())
   {
      // Special case if there is no clip yet
      auto clip = std::make_unique<WaveClip>(mpFactory, mFormat, mRate, this->GetWaveColorIndex());
      clip->InsertSilence(0, len);
      // use No-fail-guarantee
      mClips.push_back( std::move( clip ) );
      return;
   }
   else {
      // Assume at most one clip contains t
      const auto end = mClips.end();
      const auto it = std::find_if( mClips.begin(), end,
         [&](const WaveClipHolder &clip) { return clip->WithinClip(t); } );

      // use Strong-guarantee
      if (it != end)
         it->get()->InsertSilence(t, len);

      // use No-fail-guarantee
      for (const auto &clip : mClips)
      {
         if (clip->BeforeClip(t))
            clip->Offset(len);
      }
   }
}

//Performs the opposite of Join
//Analyses selected region for possible Joined clips and disjoins them
/*! @excsafety{Weak} */
void WaveTrack::Disjoin(double t0, double t1)
{
   auto minSamples = TimeToLongSamples( WAVETRACK_MERGE_POINT_TOLERANCE );
   const size_t maxAtOnce = 1048576;
   Floats buffer{ maxAtOnce };
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

         clip->GetSamples( ( samplePtr )buffer.get(), floatSample, start + done,
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
}

/*! @excsafety{Weak} */
void WaveTrack::Join(double t0, double t1)
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
         //wxPrintf("Insert clip %.6f at position %d\n", clip->GetStartTime(), i);
         clipsToDelete.insert(it, clip.get());
      }
   }

   //if there are no clips to DELETE, nothing to do
   if( clipsToDelete.size() == 0 )
      return;

   newClip = CreateClip();
   double t = clipsToDelete[0]->GetOffset();
   newClip->SetOffset(t);
   for (const auto &clip : clipsToDelete)
   {
      //wxPrintf("t=%.6f adding clip (offset %.6f, %.6f ... %.6f)\n",
      //       t, clip->GetOffset(), clip->GetStartTime(), clip->GetEndTime());

      if (clip->GetOffset() - t > (1.0 / mRate)) {
         double addedSilence = (clip->GetOffset() - t);
         //wxPrintf("Adding %.6f seconds of silence\n");
         auto offset = clip->GetOffset();
         auto value = clip->GetEnvelope()->GetValue( offset );
         newClip->AppendSilence( addedSilence, value );
         t += addedSilence;
      }

      //wxPrintf("Pasting at %.6f\n", t);
      newClip->Paste(t, clip);

      t = newClip->GetEndTime();

      auto it = FindClip(mClips, clip);
      mClips.erase(it); // deletes the clip
   }
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveTrack::Append(constSamplePtr buffer, sampleFormat format,
                       size_t len, unsigned int stride /* = 1 */)
{
   return RightmostOrNewClip()->Append(buffer, format, len, stride);
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
      maxblocksize = Sequence{ mpFactory, mFormat }.GetMaxBlockSize();
   }

   wxASSERT(maxblocksize > 0);

   return maxblocksize;
}

size_t WaveTrack::GetIdealBlockSize()
{
   return NewestOrNewClip()->GetSequence()->GetIdealBlockSize();
}

/*! @excsafety{Mixed} */
/*! @excsafety{No-fail} -- The rightmost clip will be in a flushed state. */
/*! @excsafety{Partial}
-- Some initial portion (maybe none) of the append buffer of the rightmost
clip gets appended; no previously saved contents are lost. */
void WaveTrack::Flush()
{
   // After appending, presumably.  Do this to the clip that gets appended.
   RightmostOrNewClip()->Flush();
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
         else if (this->PlayableTrack::HandleXMLAttribute(attr, value))
         {}
         else if (this->Track::HandleCommonXMLAttribute(attr, strValue))
            ;
         else if (!wxStrcmp(attr, wxT("gain")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue))
            mGain = dblValue;
         else if (!wxStrcmp(attr, wxT("pan")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue) &&
                  (dblValue >= -1.0) && (dblValue <= 1.0))
            mPan = dblValue;
         else if (!wxStrcmp(attr, wxT("channel")))
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) ||
                  !XMLValueChecker::IsValidChannel(nValue))
               return false;
            mChannel = static_cast<Track::ChannelType>( nValue );
         }
         else if (!wxStrcmp(attr, wxT("linked")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            SetLinked(nValue != 0);
         else if (!wxStrcmp(attr, wxT("colorindex")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  strValue.ToLong(&nValue))
            // Don't use SetWaveColorIndex as it sets the clips too.
            mWaveColorIndex  = nValue;
         else if (!wxStrcmp(attr, wxT("sampleformat")) &&
                  XMLValueChecker::IsGoodInt(strValue) &&
                  strValue.ToLong(&nValue) &&
                  XMLValueChecker::IsValidSampleFormat(nValue))
            mFormat = static_cast<sampleFormat>(nValue);
      } // while
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

void WaveTrack::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   xmlFile.StartTag(wxT("wavetrack"));
   this->Track::WriteCommonXMLAttributes( xmlFile );
   xmlFile.WriteAttr(wxT("channel"), mChannel);
   xmlFile.WriteAttr(wxT("linked"), mLinked);
   this->PlayableTrack::WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("rate"), mRate);
   xmlFile.WriteAttr(wxT("gain"), (double)mGain);
   xmlFile.WriteAttr(wxT("pan"), (double)mPan);
   xmlFile.WriteAttr(wxT("colorindex"), mWaveColorIndex );
   xmlFile.WriteAttr(wxT("sampleformat"), static_cast<long>(mFormat) );

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

bool WaveTrack::CloseLock()
{
   for (const auto &clip : mClips)
      clip->CloseLock();

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

std::pair<float, float> WaveTrack::GetMinMax(
   double t0, double t1, bool mayThrow) const
{
   std::pair<float, float> results {
      // we need these at extremes to make sure we find true min and max
      FLT_MAX, -FLT_MAX
   };
   bool clipFound = false;

   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return results;
   }

   if (t0 == t1)
      return results;

   for (const auto &clip: mClips)
   {
      if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      {
         clipFound = true;
         auto clipResults = clip->GetMinMax(t0, t1, mayThrow);
         if (clipResults.first < results.first)
            results.first = clipResults.first;
         if (clipResults.second > results.second)
            results.second = clipResults.second;
      }
   }

   if(!clipFound)
   {
      results = { 0.f, 0.f }; // sensible defaults if no clips found
   }

   return results;
}

float WaveTrack::GetRMS(double t0, double t1, bool mayThrow) const
{
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return 0.f;
   }

   if (t0 == t1)
      return 0.f;

   double sumsq = 0.0;
   sampleCount length = 0;

   for (const auto &clip: mClips)
   {
      // If t1 == clip->GetStartTime() or t0 == clip->GetEndTime(), then the clip
      // is not inside the selection, so we don't want it.
      // if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      {
         sampleCount clipStart, clipEnd;

         float cliprms = clip->GetRMS(t0, t1, mayThrow);

         clip->TimeToSamplesClip(wxMax(t0, clip->GetStartTime()), &clipStart);
         clip->TimeToSamplesClip(wxMin(t1, clip->GetEndTime()), &clipEnd);
         sumsq += cliprms * cliprms * (clipEnd - clipStart).as_float();
         length += (clipEnd - clipStart);
      }
   }
   return length > 0 ? sqrt(sumsq / length.as_double()) : 0.0;
}

bool WaveTrack::Get(samplePtr buffer, sampleFormat format,
                    sampleCount start, size_t len, fillFormat fill,
                    bool mayThrow, sampleCount * pNumWithinClips) const
{
   // Simple optimization: When this buffer is completely contained within one clip,
   // don't clear anything (because we won't have to). Otherwise, just clear
   // everything to be on the safe side.
   bool doClear = true;
   bool result = true;
   sampleCount samplesCopied = 0;
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
      // Usually we fill in empty space with zero
      if( fill == fillZero )
         ClearSamples(buffer, format, 0, len);
      // but we don't have to.
      else if( fill==fillTwo )
      {
         wxASSERT( format==floatSample );
         float * pBuffer = (float*)buffer;
         for(size_t i=0;i<len;i++)
            pBuffer[i]=2.0f;
      }
      else
      {
         wxFAIL_MSG(wxT("Invalid fill format"));
      }
   }

   // Iterate the clips.  They are not necessarily sorted by time.
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
            // startDelta is nonnegative and less than len
            // samplesToCopy is positive and not more than len
         }

         if (!clip->GetSamples(
               (samplePtr)(((char*)buffer) +
                           startDelta.as_size_t() *
                           SAMPLE_SIZE(format)),
               format, inclipDelta, samplesToCopy.as_size_t(), mayThrow ))
            result = false;
         else
            samplesCopied += samplesToCopy;
      }
   }
   if( pNumWithinClips )
      *pNumWithinClips = samplesCopied;
   return result;
}

/*! @excsafety{Weak} */
void WaveTrack::Set(constSamplePtr buffer, sampleFormat format,
                    sampleCount start, size_t len)
{
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
            // startDelta is nonnegative and less than len
            // samplesToCopy is positive and not more than len
         }

         clip->SetSamples(
               (constSamplePtr)(((const char*)buffer) +
                           startDelta.as_size_t() *
                           SAMPLE_SIZE(format)),
                          format, inclipDelta, samplesToCopy.as_size_t() );
         clip->MarkChanged();
      }
   }
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
   // This does mean that, in the cases where a usable clip is located, the buffer value will
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
         // Samples are obtained for the purpose of rendering a wave track,
         // so quantize time
         clip->GetEnvelope()->GetValues(rbuf, rlen, rt0, tstep);
      }
   }
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

// When the time is both the end of a clip and the start of the next clip, the
// latter clip is returned.
WaveClip* WaveTrack::GetClipAtTime(double time)
{
   
   const auto clips = SortedClipArray();
   auto p = std::find_if(clips.rbegin(), clips.rend(), [&] (WaveClip* const& clip) {
      return time >= clip->GetStartTime() && time <= clip->GetEndTime(); });

   // When two clips are immediately next to each other, the GetEndTime() of the first clip
   // and the GetStartTime() of the second clip may not be exactly equal due to rounding errors.
   // If "time" is the end time of the first of two such clips, and the end time is slightly
   // less than the start time of the second clip, then the first rather than the
   // second clip is found by the above code. So correct this.
   if (p != clips.rend() && p != clips.rbegin() &&
      time == (*p)->GetEndTime() &&
      (*p)->SharesBoundaryWithNextClip(*(p-1))) {
      p--;
   }

   return p != clips.rend() ? *p : nullptr;
}

Envelope* WaveTrack::GetEnvelopeAtTime(double time)
{
   WaveClip* clip = GetClipAtTime(time);
   if (clip)
      return clip->GetEnvelope();
   else
      return NULL;
}

Sequence* WaveTrack::GetSequenceAtTime(double time)
{
   WaveClip* clip = GetClipAtTime(time);
   if (clip)
      return clip->GetSequence();
   else
      return NULL;
}

WaveClip* WaveTrack::CreateClip(double offset)
{
   mClips.emplace_back(std::make_shared<WaveClip>(mpFactory, mFormat, mRate, GetWaveColorIndex()));
   auto clip = mClips.back().get();
   clip->SetOffset(offset);
   return clip;
}

WaveClip* WaveTrack::NewestOrNewClip()
{
   if (mClips.empty()) {
      return CreateClip(mOffset);
   }
   else
      return mClips.back().get();
}

/*! @excsafety{No-fail} */
WaveClip* WaveTrack::RightmostOrNewClip()
{
   if (mClips.empty()) {
      return CreateClip(mOffset);
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

bool WaveTrack::CanOffsetClips(
   const std::vector<WaveClip*> &clips,
   double amount,
   double *allowedAmount /* = NULL */)
{
   if (allowedAmount)
      *allowedAmount = amount;

   const auto &moving = [&](WaveClip *clip){
      // linear search might be improved, but expecting few moving clips
      // compared with the fixed clips
      return clips.end() != std::find( clips.begin(), clips.end(), clip );
   };

   for (const auto &c: mClips) {
      if ( moving( c.get() ) )
         continue;
      for (const auto clip : clips) {
         if (c->GetStartTime() < clip->GetEndTime() + amount &&
            c->GetEndTime() > clip->GetStartTime() + amount)
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
   }

   if (allowedAmount)
   {
      if (*allowedAmount == amount)
         return true;

      // Check if the NEW calculated amount would not violate
      // any other constraint
      if (!CanOffsetClips(clips, *allowedAmount, nullptr)) {
         *allowedAmount = 0; // play safe and don't allow anything
         return false;
      }
      else
         return true;
   } else
      return true;
}

bool WaveTrack::CanInsertClip(
   WaveClip* clip,  double &slideBy, double &tolerance) const
{
   for (const auto &c : mClips)
   {
      double d1 = c->GetStartTime() - (clip->GetEndTime()+slideBy);
      double d2 = (clip->GetStartTime()+slideBy) - c->GetEndTime();
      if ( (d1<0) &&  (d2<0) )
      {
         // clips overlap.
         // Try to rescue it.
         // The rescue logic is not perfect, and will typically
         // move the clip at most once.  
         // We divide by 1000 rather than set to 0, to allow for 
         // a second 'micro move' that is really about rounding error.
         if( -d1 < tolerance ){
            // right edge of clip overlaps slightly.
            // slide clip left a small amount.
            slideBy +=d1;
            tolerance /=1000;
         } else if( -d2 < tolerance ){
            // left edge of clip overlaps slightly.
            // slide clip right a small amount.
            slideBy -= d2;
            tolerance /=1000;
         }
         else
            return false; // clips overlap  No tolerance left.
      }
   }

   return true;
}

/*! @excsafety{Weak} */
void WaveTrack::Split( double t0, double t1 )
{
   SplitAt( t0 );
   if( t0 != t1 )
      SplitAt( t1 );
}

/*! @excsafety{Weak} */
void WaveTrack::SplitAt(double t)
{
   for (const auto &c : mClips)
   {
      if (c->WithinClip(t))
      {
         t = LongSamplesToTime(TimeToLongSamples(t)); // put t on a sample
         auto newClip = std::make_unique<WaveClip>( *c, mpFactory, true );
         c->Clear(t, c->GetEndTime());
         newClip->Clear(c->GetStartTime(), t);

         //offset the NEW clip by the splitpoint (noting that it is already offset to c->GetStartTime())
         sampleCount here = llrint(floor(((t - c->GetStartTime()) * mRate) + 0.5));
         newClip->Offset(here.as_double()/(double)mRate);
         // This could invalidate the iterators for the loop!  But we return
         // at once so it's okay
         mClips.push_back(std::move(newClip)); // transfer ownership
         return;
      }
   }
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
/*! @excsafety{Strong} */
void WaveTrack::ExpandCutLine(double cutLinePosition, double* cutlineStart,
                              double* cutlineEnd)
{
   bool editClipCanMove = GetEditClipsCanMove();

   // Find clip which contains this cut line
   double start = 0, end = 0;
   auto pEnd = mClips.end();
   auto pClip = std::find_if( mClips.begin(), pEnd,
      [&](const WaveClipHolder &clip) {
         return clip->FindCutLine(cutLinePosition, &start, &end); } );
   if (pClip != pEnd)
   {
      auto &clip = *pClip;
      if (!editClipCanMove)
      {
         // We are not allowed to move the other clips, so see if there
         // is enough room to expand the cut line
         for (const auto &clip2: mClips)
         {
            if (clip2->GetStartTime() > clip->GetStartTime() &&
                clip->GetEndTime() + end - start > clip2->GetStartTime())
               // Strong-guarantee in case of this path
               throw SimpleMessageBoxException{
                  ExceptionType::BadUserAction,
                  XO("There is not enough room available to expand the cut line"),
                  XO("Warning"),
                  "Error:_Insufficient_space_in_track"
               };
          }
      }

      clip->ExpandCutLine(cutLinePosition);

      // Strong-guarantee provided that the following gives No-fail-guarantee

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
   }
}

bool WaveTrack::RemoveCutLine(double cutLinePosition)
{
   for (const auto &clip : mClips)
      if (clip->RemoveCutLine(cutLinePosition))
         return true;

   return false;
}

/*! @excsafety{Strong} */
void WaveTrack::MergeClips(int clipidx1, int clipidx2)
{
   WaveClip* clip1 = GetClipByIndex(clipidx1);
   WaveClip* clip2 = GetClipByIndex(clipidx2);

   if (!clip1 || !clip2) // Could happen if one track of a linked pair had a split and the other didn't.
      return; // Don't throw, just do nothing.

   // Append data from second clip to first clip
   // use Strong-guarantee
   clip1->Paste(clip1->GetEndTime(), clip2);
   
   // use No-fail-guarantee for the rest
   // Delete second clip
   auto it = FindClip(mClips, clip2);
   mClips.erase(it);
}

/*! @excsafety{Weak} -- Partial completion may leave clips at differing sample rates!
*/
void WaveTrack::Resample(int rate, ProgressDialog *progress)
{
   for (const auto &clip : mClips)
      clip->Resample(rate, progress);

   mRate = rate;
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

WaveTrackCache::~WaveTrackCache()
{
}

void WaveTrackCache::SetTrack(const std::shared_ptr<const WaveTrack> &pTrack)
{
   if (mPTrack != pTrack) {
      if (pTrack) {
         mBufferSize = pTrack->GetMaxBlockSize();
         if (!mPTrack ||
             mPTrack->GetMaxBlockSize() != mBufferSize) {
            Free();
            mBuffers[0].data = Floats{ mBufferSize };
            mBuffers[1].data = Floats{ mBufferSize };
         }
      }
      else
         Free();
      mPTrack = pTrack;
      mNValidBuffers = 0;
   }
}

const float *WaveTrackCache::GetFloats(
   sampleCount start, size_t len, bool mayThrow)
{
   constexpr auto format = floatSample;
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
         mBuffers[0] .swap ( mBuffers[1] );
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
         mBuffers[0] .swap ( mBuffers[1] );
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
            if (!mPTrack->GetFloats(
                  mBuffers[0].data.get(), start0, len0,
                  fillZero, mayThrow))
               return nullptr;
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
               if (!mPTrack->GetFloats(mBuffers[1].data.get(), start1, len1, fillZero, mayThrow))
                  return nullptr;
               mBuffers[1].start = start1;
               mBuffers[1].len = len1;
               mNValidBuffers = 2;
            }
         }
      }
      wxASSERT(mNValidBuffers < 2 || mBuffers[0].end() == mBuffers[1].start);

      samplePtr buffer = nullptr; // will point into mOverlapBuffer
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
         if (!mPTrack->GetFloats(
            // See comment below about casting
            reinterpret_cast<float *>(mOverlapBuffer.ptr()),
            start, sinitLen, fillZero, mayThrow))
            return nullptr;
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
            return mBuffers[ii].data.get() + starti.as_size_t() ;
         }
         else if (leni > 0) {
            // leni is nonnegative, therefore start falls within mBuffers[ii]
            // But we can't satisfy all from one buffer, so copy
            if (!buffer) {
               mOverlapBuffer.Resize(len, format);
               buffer = mOverlapBuffer.ptr();
            }
            // leni is positive and not more than remaining
            const size_t size = sizeof(float) * leni.as_size_t();
            // starti is less than mBuffers[ii].len and nonnegative
            memcpy(buffer, mBuffers[ii].data.get() + starti.as_size_t(), size);
            wxASSERT( leni <= remaining );
            remaining -= leni.as_size_t();
            start += leni;
            buffer += size;
         }
      }

      if (remaining > 0) {
         // Very big request!
         // Fall back to direct fetch
         if (!buffer) {
            mOverlapBuffer.Resize(len, format);
            buffer = mOverlapBuffer.ptr();
         }
         // See comment below about casting
         if (!mPTrack->GetFloats( reinterpret_cast<float*>(buffer),
            start, remaining, fillZero, mayThrow))
            return 0;
      }

      // Overlap buffer was meant for the more general support of sample formats
      // besides float, which explains the cast
      return reinterpret_cast<const float*>(mOverlapBuffer.ptr());
   }
   else {
#if 0
      // Cache works only for float format.
      mOverlapBuffer.Resize(len, format);
      if (mPTrack->Get(mOverlapBuffer.ptr(), format, start, len, fillZero, mayThrow))
         return mOverlapBuffer.ptr();
#else
      // No longer handling other than float format.  Therefore len is 0.
#endif
      return nullptr;
   }
}

void WaveTrackCache::Free()
{
   mBuffers[0].Free();
   mBuffers[1].Free();
   mOverlapBuffer.Free();
   mNValidBuffers = 0;
}

auto WaveTrack::AllClipsIterator::operator ++ () -> AllClipsIterator &
{
   // The unspecified sequence is a post-order, but there is no
   // promise whether sister nodes are ordered in time.
   if ( !mStack.empty() ) {
      auto &pair =  mStack.back();
      if ( ++pair.first == pair.second ) {
         mStack.pop_back();
      }
      else
         push( (*pair.first)->GetCutLines() );
   }

   return *this;
}

void WaveTrack::AllClipsIterator::push( WaveClipHolders &clips )
{
   auto pClips = &clips;
   while (!pClips->empty()) {
      auto first = pClips->begin();
      mStack.push_back( Pair( first, pClips->end() ) );
      pClips = &(*first)->GetCutLines();
   }
}

#include "SampleBlock.h"
void VisitBlocks(TrackList &tracks, BlockVisitor visitor,
   SampleBlockIDSet *pIDs)
{
   for (auto wt : tracks.Any< const WaveTrack >()) {
      // Scan all clips within current track
      for(const auto &clip : wt->GetAllClips()) {
         // Scan all sample blocks within current clip
         auto blocks = clip->GetSequenceBlockArray();
         for (const auto &block : *blocks) {
            auto &pBlock = block.sb;
            if ( pBlock ) {
               if ( pIDs && !pIDs->insert(pBlock->GetBlockID()).second )
                  continue;
               if ( visitor )
                  visitor( *pBlock );
            }
         }
      }
   }
}

void InspectBlocks(const TrackList &tracks, BlockInspector inspector,
   SampleBlockIDSet *pIDs)
{
   VisitBlocks(
      const_cast<TrackList &>(tracks), std::move( inspector ), pIDs );
}

#include "Project.h"
#include "SampleBlock.h"
static auto TrackFactoryFactory = []( AudacityProject &project ) {
   return std::make_shared< WaveTrackFactory >(
      ProjectSettings::Get( project ),
      SampleBlockFactory::New( project ) );
};

static const AudacityProject::AttachedObjects::RegisteredFactory key2{
   TrackFactoryFactory
};

WaveTrackFactory &WaveTrackFactory::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< WaveTrackFactory >( key2 );
}

const WaveTrackFactory &WaveTrackFactory::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

WaveTrackFactory &WaveTrackFactory::Reset( AudacityProject &project )
{
   auto result = TrackFactoryFactory( project );
   project.AttachedObjects::Assign( key2, result );
   return *result;
}

void WaveTrackFactory::Destroy( AudacityProject &project )
{
   project.AttachedObjects::Assign( key2, nullptr );
}
