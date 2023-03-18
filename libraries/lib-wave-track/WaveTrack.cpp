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
#include <wx/debug.h>
#include <wx/log.h>

#include <float.h>
#include <math.h>
#include <algorithm>
#include <optional>
#include <numeric>

#include "float_cast.h"

#include "Envelope.h"
#include "Sequence.h"

#include "Project.h"
#include "ProjectRate.h"

#include "Prefs.h"
#include "SyncLock.h"
#include "TimeWarper.h"
#include "QualitySettings.h"

#include "InconsistencyException.h"

#include "ProjectFormatExtensionsRegistry.h"

using std::max;

namespace {

bool AreAligned(const WaveClipPointers& a, const WaveClipPointers& b)
{
   if (a.size() != b.size())
      return false;

   const auto compare = [](const WaveClip* a, const WaveClip* b) {
      // clips are aligned if both sequence start/end 
      // points and play start/end points of the first clip match 
      // the corresponding points of the other clip
      return a->GetPlayStartTime() == b->GetPlayStartTime() &&
         a->GetSequenceStartTime() == b->GetSequenceStartTime() &&
         a->GetPlayEndTime() == b->GetPlayEndTime() &&
         a->GetSequenceEndTime() == b->GetSequenceEndTime();
   };

   return std::mismatch(a.begin(), a.end(), b.begin(), compare).first == a.end();
}

//Handles possible future file values
Track::LinkType ToLinkType(int value)
{
   if (value < 0)
      return Track::LinkType::None;
   else if (value > 3)
      return Track::LinkType::Group;
   return static_cast<Track::LinkType>(value);
}

}

double WaveTrack::ProjectNyquistFrequency(const AudacityProject &project)
{
   auto &tracks = TrackList::Get(project);
   return std::max(ProjectRate::Get(project).GetRate(),
      tracks.Any<const WaveTrack>().max(&WaveTrack::GetRate))
      / 2.0;
}

static auto DefaultName = XO("Audio");

wxString WaveTrack::GetDefaultAudioTrackNamePreference()
{
   const auto name = AudioTrackNameSetting.ReadWithDefault(L"");

   if (name.empty() || ( name == DefaultName.MSGID() ))
      // When nothing was specified,
      // the default-default is whatever translation of...
      /* i18n-hint: The default name for an audio track. */
      return DefaultName.Translation();
   else
      return name;
}

static ProjectFileIORegistry::ObjectReaderEntry readerEntry{
   "wavetrack",
   WaveTrack::New
};

std::shared_ptr<WaveTrack> WaveTrackFactory::Create()
{
   return Create(QualitySettings::SampleFormatChoice(), mRate.GetRate());
}

std::shared_ptr<WaveTrack> WaveTrackFactory::Create(sampleFormat format, double rate)
{
   return std::make_shared<WaveTrack>(mpFactory, format, rate);
}

WaveTrack *WaveTrack::New( AudacityProject &project )
{
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto &tracks = TrackList::Get( project );
   auto result = tracks.Add(trackFactory.Create());
   result->AttachedTrackObjects::BuildAll();
   return result;
}

WaveTrack::WaveTrack( const SampleBlockFactoryPtr &pFactory,
   sampleFormat format, double rate )
   : WritableSampleTrack()
   , mpFactory(pFactory)
{
   mLegacyProjectFileOffset = 0;

   mFormat = format;
   mRate = (int) rate;
   mWaveColorIndex = 0;
}

WaveTrack::WaveTrack(const WaveTrack &orig, ProtectedCreationArg &&a)
   : WritableSampleTrack(orig, std::move(a))
   , mpFactory( orig.mpFactory )
{
   mLegacyProjectFileOffset = 0;
   for (const auto &clip : orig.mClips)
      mClips.push_back
         ( std::make_unique<WaveClip>( *clip, mpFactory, true ) );
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack &orig)
{
   WritableSampleTrack::Init(orig);
   mpFactory = orig.mpFactory;
   
   mFormat = orig.mFormat;
   mWaveColorIndex = orig.mWaveColorIndex;
   mRate = orig.mRate;
   DoSetGain(orig.GetGain());
   DoSetPan(orig.GetPan());
}

void WaveTrack::Reinit(const WaveTrack &orig)
{
   Init(orig);

   // Copy attached data from orig.  Nullify data in this where orig had null.
   Attachments &attachments = *this;
   attachments = orig;
}

void WaveTrack::Merge(const Track &orig)
{
   orig.TypeSwitch( [&](const WaveTrack *pwt) {
      const WaveTrack &wt = *pwt;
      DoSetGain(wt.GetGain());
      DoSetPan(wt.GetPan());
      // Copy attached data from orig.  Nullify data in this where orig had null.
      Attachments &attachments = *this;
      attachments = *pwt;
   });
   WritableSampleTrack::Merge(orig);
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
      clip->Offset(delta);

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
}

bool WaveTrack::LinkConsistencyFix(bool doFix, bool completeList)
{
   auto err = !WritableSampleTrack::LinkConsistencyFix(doFix, completeList);
   if (completeList) {
      auto linkType = GetLinkType();
      if (static_cast<int>(linkType) == 1 || //Comes from old audacity version
          linkType == LinkType::Aligned) {
         auto next =
            dynamic_cast<WaveTrack*>(*std::next(GetOwner()->Find(this)));
         if (next == nullptr) {
            //next track is absent or not a wave track, fix and report error
            if (doFix) {
               wxLogWarning(L"Right track %s is expected to be a WaveTrack."
                  "\n Removing link from left wave track %s.",
                  next->GetName(), GetName());
               SetLinkType(LinkType::None);
               SetChannel(MonoChannel);
            }
            err = true;
         }
         else if (doFix) {
            auto newLinkType =
               AreAligned(SortedClipArray(), next->SortedClipArray())
               ? LinkType::Aligned : LinkType::Group;
            //not an error
            if (newLinkType != linkType)
               SetLinkType(newLinkType);
         }
      }
   }
   return !err;
}


static const Track::TypeInfo &typeInfo()
{
   static const Track::TypeInfo info{
      { "wave", "wave", XO("Wave Track") },
      true, &WritableSampleTrack::ClassTypeInfo() };
   return info;
}

auto WaveTrack::GetTypeInfo() const -> const TypeInfo &
{
   return typeInfo();
}

auto WaveTrack::ClassTypeInfo() -> const TypeInfo &
{
   return typeInfo();
}

template< typename Container >
static Container MakeIntervals(const std::vector<WaveClipHolder> &clips)
{
   Container result;
   for (const auto &clip: clips) {
      result.emplace_back( clip->GetPlayStartTime(), clip->GetPlayEndTime(),
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

const WaveClip* WaveTrack::FindClipByName(const wxString& name) const
{
   for (const auto& clip : mClips)
   {
      if (clip->GetName() == name)
         return clip.get();
   }
   return nullptr;
}

Track::Holder WaveTrack::Clone() const
{
   auto result = std::make_shared<WaveTrack>(*this, ProtectedCreationArg{});
   result->Init(*this);
   return result;
}

wxString WaveTrack::MakeClipCopyName(const wxString& originalName) const
{
   auto name = originalName;
   for (auto i = 1;; ++i)
   {
      if (FindClipByName(name) == nullptr)
         return name;
      //i18n-hint Template for clip name generation on copy-paste
      name = XC("%s.%i", "clip name template").Format(originalName, i).Translation();
   }
}

wxString WaveTrack::MakeNewClipName() const
{
   auto name = GetName();
   for (auto i = 1;; ++i)
   {
      if (FindClipByName(name) == nullptr)
         return name;
      //i18n-hint Template for clip name generation on inserting new empty clip
      name = XC("%s %i", "clip name template").Format(GetName(), i).Translation();
   }
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
      clip->SetSequenceStartTime( clip->GetSequenceStartTime() * ratio );
   }
}

float WaveTrack::GetGain() const
{
   return mGain.load(std::memory_order_relaxed);
}

void WaveTrack::DoSetGain(float value)
{
   mGain.store(value, std::memory_order_relaxed);
}

void WaveTrack::SetGain(float newGain)
{
   if (GetGain() != newGain) {
      DoSetGain(newGain);
      Notify();
   }
}

float WaveTrack::GetPan() const
{
   return mPan.load(std::memory_order_relaxed);
}

void WaveTrack::DoSetPan(float value)
{
   mPan.store(value, std::memory_order_relaxed);
}

void WaveTrack::SetPan(float newPan)
{
   if (newPan > 1.0)
      newPan = 1.0;
   else if (newPan < -1.0)
      newPan = -1.0;

   if ( GetPan() != newPan ) {
      DoSetPan(newPan);
      Notify();
   }
}

float WaveTrack::GetChannelGain(int channel) const
{
   float left = 1.0;
   float right = 1.0;

   const auto pan = GetPan();
   if (pan < 0)
      right = (pan + 1.0);
   else if (pan > 0)
      left = 1.0 - pan;

   const auto gain = GetGain();
   if ((channel%2) == 0)
      return left * gain;
   else
      return right * gain;
}

/*! @excsafety{Strong} */
void WaveTrack::SetWaveColorIndex(int colorIndex)
{
   for (const auto &clip : mClips)
      clip->SetColourIndex( colorIndex );
   mWaveColorIndex = colorIndex;
}

sampleCount WaveTrack::GetPlaySamplesCount() const
{
    sampleCount result{ 0 };

    for (const auto& clip : mClips)
        result += clip->GetPlaySamplesCount();

    return result;
}

sampleCount WaveTrack::GetSequenceSamplesCount() const
{
   sampleCount result{ 0 };

   for (const auto& clip : mClips)
      result += clip->GetSequenceSamplesCount();

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
      if (!clip->BeforePlayStartTime(t1) && !clip->AfterPlayEndTime(t0)) {
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

   for (const auto &clip : mClips)
   {
      if(t1 > clip->GetPlayStartTime() && t1 < clip->GetPlayEndTime())
      {
         clip->SetTrimRight(clip->GetTrimRight() + clip->GetPlayEndTime() - t1);
         inside1 = true;
      }

      if(t0 > clip->GetPlayStartTime() && t0 < clip->GetPlayEndTime())
      {
         clip->SetTrimLeft(clip->GetTrimLeft() + t0 - clip->GetPlayStartTime());
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
   const SampleBlockFactoryPtr &pFactory, bool keepLink) const
{
   auto result = std::make_shared<WaveTrack>( pFactory, mFormat, mRate );
   result->Init(*this);
   result->mpFactory = pFactory ? pFactory : mpFactory;
   if (!keepLink)
      result->SetLinkType(LinkType::None);
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
      if (t0 <= clip->GetPlayStartTime() && t1 >= clip->GetPlayEndTime())
      {
         // Whole clip is in copy region
         //wxPrintf("copy: clip %i is in copy region\n", (int)clip);

         newTrack->mClips.push_back
            (std::make_unique<WaveClip>(*clip, mpFactory, ! forClipboard));
         WaveClip *const newClip = newTrack->mClips.back().get();
         newClip->Offset(-t0);
      }
      else if (clip->CountSamples(t0, t1) >= 1)
      {
         // Clip is affected by command
         //wxPrintf("copy: clip %i is affected by command\n", (int)clip);

         auto newClip = std::make_unique<WaveClip>
            (*clip, mpFactory, ! forClipboard, t0, t1);
         newClip->SetName(clip->GetName());

         newClip->Offset(-t0);
         if (newClip->GetPlayStartTime() < 0)
            newClip->SetPlayStartTime(0);

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

namespace {
   
   //Internal structure, which is supposed to contain
   //data related to clip boundaries, and used during 
   //ClearAndPaste to restore old splits after new
   //data is pasted
   struct SplitInfo
   {
      //Time, where boundary is located
      double time;
      //Contains trimmed data, which should be re-appended to
      //the clip to the left from the boundary, may be null
      std::shared_ptr<WaveClip> left;
      //Contains trimmed data, which should be re-appended to
      //the clip to the right from the boundary, may be null
      std::shared_ptr<WaveClip> right;
      //Contains clip name next to the left from the boundary,
      //if present, that needs to be re-assigned to the matching
      //clip after split
      std::optional<wxString> leftClipName;
      //Contains clip name next to the right from the boundary,
      //if present, that needs to be re-assigned to the matching
      //clip after split
      std::optional<wxString> rightClipName;
   };

}

//
// ClearAndPaste() is a specialized version of HandleClear()
// followed by Paste() and is used mostly by effects that
// can't replace track data directly using Get()/Set().
//
// HandleClear() removes any cut/split lines with the
// cleared range, but, in most cases, effects want to preserve
// the existing cut/split lines, trimmed data and clip names,
// so they are saved before the HandleClear()/Paste() and restored after.
// When pasted track has split lines with hidden data at same places 
// as the target one, then only targets hidden data is preserved, and
// hidden data from pasted track is discarded.
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

   std::vector<SplitInfo> splits;
   WaveClipHolders cuts;

   //helper routine, that finds SplitInfo by time value,
   //or creates a new one if no one exists yet
   auto get_split = [&](double time) {
      auto it = std::find_if(splits.begin(), splits.end(), [time](const SplitInfo& split) {
         return split.time == time;
      });
      if(it == splits.end())
         it = splits.insert(
            splits.end(),
            { time, nullptr, nullptr, std::nullopt, std::nullopt }
         );
      return it;
   };

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
      // we need to copy clips, trims and names, because the original ones
      // could be changed later during Clear/Paste routines
      st = LongSamplesToTime(TimeToLongSamples(clip->GetPlayStartTime()));
      if (st >= t0 && st <= t1) {
         auto it = get_split(st);
         if (clip->GetTrimLeft() != 0)
         {
            //keep only hidden left part
            it->right = std::make_shared<WaveClip>(*clip, mpFactory, false);
            it->right->SetTrimLeft(.0);
            it->right->ClearRight(clip->GetPlayStartTime());
         }
         it->rightClipName = clip->GetName();
      }

      st = LongSamplesToTime(TimeToLongSamples(clip->GetPlayEndTime()));
      if (st >= t0 && st <= t1) {
         auto it = get_split(st);
         if (clip->GetTrimRight() != 0)
         {
            //keep only hidden right part
            it->left = std::make_shared<WaveClip>(*clip, mpFactory, false);
            it->left->SetTrimRight(.0);
            it->left->ClearLeft(clip->GetPlayEndTime());
         }
         it->leftClipName = clip->GetName();
      }

      // Search for cut lines
      auto &cutlines = clip->GetCutLines();
      // May erase from cutlines, so don't use range-for
      for (auto it = cutlines.begin(); it != cutlines.end(); ) {
         WaveClip *cut = it->get();
         double cs = LongSamplesToTime(TimeToLongSamples(clip->GetSequenceStartTime() +
                                                         cut->GetSequenceStartTime()));

         // Remember cut point
         if (cs >= t0 && cs <= t1) {

            // Remember the absolute offset and add to our cuts array.
            cut->SetSequenceStartTime(cs);
            cuts.push_back(std::move(*it)); // transfer ownership!
            it = cutlines.erase(it);
         }
         else
            ++it;
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
                  if (fabs(t1 - clip->GetPlayStartTime()) < tolerance) {
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
               if (fabs(t0 - clip->GetPlayEndTime()) < tolerance)
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

         auto attachLeft = [](WaveClip* target, WaveClip* src) 
         {
            wxASSERT(target->GetTrimLeft() == 0);
            if (target->GetTrimLeft() != 0)
               return;

            auto trim = src->GetPlayEndTime() - src->GetPlayStartTime();
            target->Paste(target->GetPlayStartTime(), src);
            target->SetTrimLeft(trim);
            //Play start time needs to be adjusted after 
            //prepending data to the sequence
            target->Offset(-trim);
         };

         auto attachRight = [](WaveClip* target, WaveClip* src)
         {
            wxASSERT(target->GetTrimRight() == 0);
            if (target->GetTrimRight() != 0)
               return;
            
            auto trim = src->GetPlayEndTime() - src->GetPlayStartTime();
            target->Paste(target->GetPlayEndTime(), src);
            target->SetTrimRight(trim);
         };

         // Restore the split lines and trims, transforming the position appropriately
         for (const auto& split: splits) {
            auto at = LongSamplesToTime(TimeToLongSamples(warper->Warp(split.time)));
            for (const auto& clip : GetClips())
            {
               if (clip->WithinPlayRegion(at))//strictly inside
               {
                  auto newClip = std::make_unique<WaveClip>(*clip, mpFactory, true);

                  clip->ClearRight(at);
                  newClip->ClearLeft(at);
                  if (split.left)
                     attachRight(clip.get(), split.left.get());
                  if (split.right)
                     attachLeft(newClip.get(), split.right.get());
                  AddClip(std::move(newClip));
                  break;
               }
               else if (clip->GetPlayStartSample() == TimeToLongSamples(at) && split.right)
               {
                  attachLeft(clip.get(), split.right.get());
                  break;
               }
               else if (clip->GetPlayEndSample() == TimeToLongSamples(at) && split.left)
               {
                  attachRight(clip.get(), split.left.get());
                  break;
               }
            }
         }

         //Restore clip names
         for (const auto& split : splits)
         {
            auto s = TimeToLongSamples(warper->Warp(split.time));
            for (auto& clip : GetClips())
            {
               if (split.rightClipName.has_value() && clip->GetPlayStartSample() == s)
                  clip->SetName(*split.rightClipName);
               else if (split.leftClipName.has_value() && clip->GetPlayEndSample() == s)
                  clip->SetName(*split.leftClipName);
            }
         }

         // Restore the saved cut lines, also transforming if time altered
         for (const auto &clip : mClips) {
            double st;
            double et;

            st = clip->GetPlayStartTime();
            et = clip->GetPlayEndTime();

            // Scan the cuts for any that live within this clip
            for (auto it = cuts.begin(); it != cuts.end();) {
               WaveClip *cut = it->get();
               //cutlines in this array were orphaned previously
               double cs = cut->GetSequenceStartTime();

               // Offset the cut from the start of the clip and add it to
               // this clips cutlines.
               if (cs >= st && cs <= et) {
                  cut->SetSequenceStartTime(warper->Warp(cs) - st);
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
         if (!clip->BeforePlayStartTime(t1) && !clip->AfterPlayEndTime(t0) &&
               (clip->BeforePlayStartTime(t0) || clip->AfterPlayEndTime(t1)))
         {
            addCutLines = false;
            break;
         }
      }
   }

   for (const auto &clip : mClips)
   {
      if (clip->BeforePlayStartTime(t0) && clip->AfterPlayEndTime(t1))
      {
         // Whole clip must be deleted - remember this
         clipsToDelete.push_back(clip.get());
      }
      else if (!clip->BeforePlayStartTime(t1) && !clip->AfterPlayEndTime(t0))
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

               if (clip->BeforePlayStartTime(t0)) {
                  // Delete from the left edge

                  // Don't modify this clip in place, because we want a strong
                  // guarantee, and might modify another clip
                  clipsToDelete.push_back( clip.get() );
                  auto newClip = std::make_unique<WaveClip>( *clip, mpFactory, true );
                  newClip->TrimLeft(t1 - clip->GetPlayStartTime());
                  clipsToAdd.push_back( std::move( newClip ) );
               }
               else if (clip->AfterPlayEndTime(t1)) {
                  // Delete to right edge

                  // Don't modify this clip in place, because we want a strong
                  // guarantee, and might modify another clip
                  clipsToDelete.push_back( clip.get() );
                  auto newClip = std::make_unique<WaveClip>( *clip, mpFactory, true );
                  newClip->TrimRight(clip->GetPlayEndTime() - t0);

                  clipsToAdd.push_back( std::move( newClip ) );
               }
               else {
                  // Delete in the middle of the clip...we actually create two
                  // NEW clips out of the left and right halves...

                  auto leftClip = std::make_unique<WaveClip>(*clip, mpFactory, true);
                  leftClip->TrimRight(clip->GetPlayEndTime() - t0);
                  clipsToAdd.push_back(std::move(leftClip));

                  auto rightClip = std::make_unique<WaveClip>(*clip, mpFactory, true);
                  rightClip->TrimLeft(t1 - rightClip->GetPlayStartTime());
                  clipsToAdd.push_back(std::move(rightClip));

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

   if (!split && editClipCanMove)
   {
      // Clip is "behind" the region -- offset it unless we're splitting
      // or we're using the "don't move other clips" mode
      for (const auto& clip : mClips)
      {
         if (clip->BeforePlayStartTime(t1))
            clip->Offset(-(t1 - t0));
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
         if (EditClipsCanMove.Read()) {
            const auto offset = newT1 - oldT1;
            for(const auto& clip : mClips)
            {
               if (clip->GetPlayStartTime() > oldT1 - (1.0 / mRate))
                  clip->Offset(offset);
            }
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

void WaveTrack::PasteWaveTrack(double t0, const WaveTrack* other)
{
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
        return;

    //wxPrintf("paste: we have at least one clip\n");

    bool singleClipMode = other->GetNumClips() == 1 &&
        std::abs(other->GetStartTime()) < LongSamplesToTime(1) * 0.5;

    const double insertDuration = other->GetEndTime();
    if (insertDuration != 0 && insertDuration < 1.0 / mRate)
        // PRL:  I added this check to avoid violations of preconditions in other WaveClip and Sequence
        // methods, but allow the value 0 so I don't subvert the purpose of commit
        // 739422ba70ceb4be0bb1829b6feb0c5401de641e which causes append-recording always to make
        // a new clip.
        return;

    //wxPrintf("Check if we need to make room for the pasted data\n");

    auto pastingFromTempTrack = !other->GetOwner();
    bool editClipCanMove = GetEditClipsCanMove();

    // Make room for the pasted data
    if (editClipCanMove) {
        if (!singleClipMode) {
            // We need to insert multiple clips, so split the current clip and ...
            SplitAt(t0);
        }
        //else if there is a clip at t0 insert new clip inside it and ...
       
        // ... move everything to the right
        for (const auto& clip : mClips)
        {
            if (clip->GetPlayStartTime() > t0 - (1.0 / mRate))
                clip->Offset(insertDuration);
        }
    }

    if (singleClipMode)
    {
        // Single clip mode
        // wxPrintf("paste: checking for single clip mode!\n");

        WaveClip* insideClip = nullptr;

        for (const auto& clip : mClips)
        {
            if (editClipCanMove)
            {
                if (clip->WithinPlayRegion(t0))
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
                if (clip->WithinPlayRegion(t0) ||
                    TimeToLongSamples(t0) == clip->GetPlayStartSample())
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
                for (const auto& clip : mClips)
                {
                    if (clip->GetPlayStartTime() > insideClip->GetPlayStartTime() &&
                        insideClip->GetPlayEndTime() + insertDuration >
                        clip->GetPlayStartTime())
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
            return;
        }
        // Just fall through and exhibit NEW behaviour
    }

    // Insert NEW clips
    //wxPrintf("paste: multi clip mode!\n");

    if (!editClipCanMove && !IsEmpty(t0, t0 + insertDuration - 1.0 / mRate))
        // Strong-guarantee in case of this path
        // not that it matters.
        throw SimpleMessageBoxException{
           ExceptionType::BadUserAction,
           XO("There is not enough room available to paste the selection"),
           XO("Warning"),
           "Error:_Insufficient_space_in_track"
    };

    for (const auto& clip : other->mClips)
    {
        // AWD Oct. 2009: Don't actually paste in placeholder clips
        if (!clip->GetIsPlaceholder())
        {
            auto newClip =
                std::make_unique<WaveClip>(*clip, mpFactory, true);
            newClip->Resample(mRate);
            newClip->Offset(t0);
            newClip->MarkChanged();
            if (pastingFromTempTrack)
                //Clips from the tracks which aren't bound to any TrackList are 
                //considered to be new entities, thus named using "new" name template
                newClip->SetName(MakeNewClipName());
            else
                newClip->SetName(MakeClipCopyName(clip->GetName()));
            mClips.push_back(std::move(newClip)); // transfer ownership
        }
    }
}

/*! @excsafety{Weak} */
void WaveTrack::Paste(double t0, const Track *src)
{
   if (auto other = dynamic_cast<const WaveTrack*>(src))
      PasteWaveTrack(t0, other);
   else
      // THROW_INCONSISTENCY_EXCEPTION; // ?
      (void)0;// Empty if intentional.   
}

void WaveTrack::Silence(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto start = TimeToLongSamples(t0);
   auto end = TimeToLongSamples(t1);

   for (const auto &clip : mClips)
   {
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();

      if (clipEnd > start && clipStart < end)
      {
         auto offset = std::max(start - clipStart, sampleCount(0));
         // Clip sample region and Get/Put sample region overlap
         auto length = std::min(end, clipEnd) - (clipStart + offset);

         clip->SetSilence(offset, length);
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
         [&](const WaveClipHolder &clip) { return clip->WithinPlayRegion(t); } );

      // use Strong-guarantee
      if (it != end)
         it->get()->InsertSilence(t, len);

      // use No-fail-guarantee
      for (const auto &clip : mClips)
      {
         if (clip->BeforePlayStartTime(t))
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

   for (const auto &clip : mClips)
   {
      double startTime = clip->GetPlayStartTime();
      double endTime = clip->GetPlayEndTime();

      if( endTime < t0 || startTime > t1 )
         continue;

      //simply look for a sequence of zeroes and if the sequence
      //is greater than minimum number, split-DELETE the region

      sampleCount seqStart = -1;
      auto start = clip->TimeToSamples(std::max(.0, t0 - startTime));
      auto end = clip->TimeToSamples(std::min(endTime, t1) - startTime);

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
                     regions.push_back(
                        Region(
                           startTime + clip->SamplesToTime(seqStart),
                           startTime + clip->SamplesToTime(seqEnd)
                        )
                     );
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
   WaveClip* newClip{};

   for (const auto &clip: mClips)
   {
      if (clip->GetPlayStartTime() < t1-(1.0/mRate) &&
          clip->GetPlayEndTime()-(1.0/mRate) > t0) {

         // Put in sorted order
         auto it = clipsToDelete.begin(), end = clipsToDelete.end();
         for (; it != end; ++it)
            if ((*it)->GetPlayStartTime() > clip->GetPlayStartTime())
               break;
         //wxPrintf("Insert clip %.6f at position %d\n", clip->GetStartTime(), i);
         clipsToDelete.insert(it, clip.get());
      }
   }

   //if there are no clips to DELETE, nothing to do
   if( clipsToDelete.size() == 0 )
      return;

   auto t = clipsToDelete[0]->GetPlayStartTime();
   //preserve left trim data if any
   newClip = CreateClip(clipsToDelete[0]->GetSequenceStartTime(),
      clipsToDelete[0]->GetName());
   
   for (const auto &clip : clipsToDelete)
   {
      //wxPrintf("t=%.6f adding clip (offset %.6f, %.6f ... %.6f)\n",
      //       t, clip->GetOffset(), clip->GetStartTime(), clip->GetEndTime());

      if (clip->GetPlayStartTime() - t > (1.0 / mRate)) {
         double addedSilence = (clip->GetPlayStartTime() - t);
         //wxPrintf("Adding %.6f seconds of silence\n");
         auto offset = clip->GetPlayStartTime();
         auto value = clip->GetEnvelope()->GetValue( offset );
         newClip->AppendSilence( addedSilence, value );
         t += addedSilence;
      }

      //wxPrintf("Pasting at %.6f\n", t);
      newClip->Paste(t, clip);

      t = newClip->GetPlayEndTime();

      auto it = FindClip(mClips, clip);
      mClips.erase(it); // deletes the clip
   }
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveTrack::Append(constSamplePtr buffer, sampleFormat format,
   size_t len, unsigned int stride, sampleFormat effectiveFormat)
{
   return RightmostOrNewClip()
      ->Append(buffer, format, len, stride, effectiveFormat);
}

sampleCount WaveTrack::GetBlockStart(sampleCount s) const
{
   for (const auto &clip : mClips)
   {
      const auto startSample = clip->GetPlayStartSample();
      const auto endSample = clip->GetPlayEndSample();
      if (s >= startSample && s < endSample)
      {
          auto blockStartOffset = clip->GetSequence()->GetBlockStart(clip->ToSequenceSamples(s));
          return std::max(startSample, clip->GetSequenceStartSample() + blockStartOffset);
      }
   }

   return -1;
}

size_t WaveTrack::GetBestBlockSize(sampleCount s) const
{
   auto bestBlockSize = GetMaxBlockSize();

   for (const auto &clip : mClips)
   {
      auto startSample = clip->GetPlayStartSample();
      auto endSample = clip->GetPlayEndSample();
      if (s >= startSample && s < endSample)
      {
         bestBlockSize = clip->GetSequence()->GetBestBlockSize(s - clip->GetSequenceStartSample());
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
      maxblocksize =
         Sequence{ mpFactory, SampleFormats{mFormat, mFormat} }
            .GetMaxBlockSize();
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

namespace {
bool IsValidChannel(const int nValue)
{
   return (nValue >= Track::LeftChannel) && (nValue <= Track::MonoChannel);
}
}

bool WaveTrack::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
{
   if (tag == "wavetrack") {
      double dblValue;
      long nValue;

      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == "rate")
         {
            // mRate is an int, but "rate" in the project file is a float.
            if (!value.TryGet(dblValue) ||
                  (dblValue < 1.0) || (dblValue > 1000000.0)) // allow a large range to be read
               return false;

            mRate = lrint(dblValue);
         }
         else if (attr == "offset" && value.TryGet(dblValue))
         {
            // Offset is only relevant for legacy project files. The value
            // is cached until the actual WaveClip containing the legacy
            // track is created.
            mLegacyProjectFileOffset = dblValue;
         }
         else if (this->WritableSampleTrack::HandleXMLAttribute(attr, value))
         {}
         else if (this->Track::HandleCommonXMLAttribute(attr, value))
            ;
         else if (attr == "gain" && value.TryGet(dblValue))
            DoSetGain(dblValue);
         else if (attr == "pan" && value.TryGet(dblValue) &&
                  (dblValue >= -1.0) && (dblValue <= 1.0))
            DoSetPan(dblValue);
         else if (attr == "channel")
         {
            if (!value.TryGet(nValue) ||
                  !IsValidChannel(nValue))
               return false;
            mChannel = static_cast<Track::ChannelType>( nValue );
         }
         else if (attr == "linked" && value.TryGet(nValue))
            SetLinkType(ToLinkType(nValue), false);
         else if (attr == "colorindex" && value.TryGet(nValue))
            // Don't use SetWaveColorIndex as it sets the clips too.
            mWaveColorIndex  = nValue;
         else if (attr == "sampleformat" && value.TryGet(nValue) &&
                  Sequence::IsValidSampleFormat(nValue))
            mFormat = static_cast<sampleFormat>(nValue);
      } // while
      return true;
   }

   return false;
}

void WaveTrack::HandleXMLEndTag(const std::string_view&  WXUNUSED(tag))
{
   // In case we opened a pre-multiclip project, we need to
   // simulate closing the waveclip tag.
   NewestOrNewClip()->HandleXMLEndTag("waveclip");
}

XMLTagHandler *WaveTrack::HandleXMLChild(const std::string_view& tag)
{
   if ( auto pChild = WaveTrackIORegistry::Get()
          .CallObjectAccessor(tag, *this) )
      return pChild;

   //
   // This is legacy code (1.2 and previous) and is not called for NEW projects!
   //
   if (tag == "sequence" || tag == "envelope")
   {
      // This is a legacy project, so set the cached offset
      NewestOrNewClip()->SetSequenceStartTime(mLegacyProjectFileOffset);

      // Legacy project file tracks are imported as one single wave clip
      if (tag == "sequence")
         return NewestOrNewClip()->GetSequence();
      else if (tag == "envelope")
         return NewestOrNewClip()->GetEnvelope();
   }

   // JKC... for 1.1.0, one step better than what we had, but still badly broken.
   // If we see a waveblock at this level, we'd better generate a sequence.
   if (tag == "waveblock")
   {
      // This is a legacy project, so set the cached offset
      NewestOrNewClip()->SetSequenceStartTime(mLegacyProjectFileOffset);
      Sequence *pSeq = NewestOrNewClip()->GetSequence();
      return pSeq;
   }

   //
   // This is for the NEW file format (post-1.2)
   //
   if (tag == "waveclip")
      return CreateClip();

   return nullptr;
}

void WaveTrack::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   xmlFile.StartTag(wxT("wavetrack"));
   this->Track::WriteCommonXMLAttributes( xmlFile );
   xmlFile.WriteAttr(wxT("channel"), mChannel);
   xmlFile.WriteAttr(wxT("linked"), static_cast<int>(GetLinkType()));
   this->WritableSampleTrack::WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("rate"), mRate);
   xmlFile.WriteAttr(wxT("gain"), static_cast<double>(GetGain()));
   xmlFile.WriteAttr(wxT("pan"), static_cast<double>(GetPan()));
   xmlFile.WriteAttr(wxT("colorindex"), mWaveColorIndex );
   xmlFile.WriteAttr(wxT("sampleformat"), static_cast<long>(mFormat) );

   WaveTrackIORegistry::Get().CallWriters(*this, xmlFile);

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
         best = clip->GetPlayStartTime();
      }
      else if (clip->GetPlayStartTime() < best)
         best = clip->GetPlayStartTime();

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
         best = clip->GetPlayEndTime();
      }
      else if (clip->GetPlayEndTime() > best)
         best = clip->GetPlayEndTime();

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
      if (t1 >= clip->GetPlayStartTime() && t0 <= clip->GetPlayEndTime())
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
      if (t1 >= clip->GetPlayStartTime() && t0 <= clip->GetPlayEndTime())
      {
         auto clipStart = clip->TimeToSequenceSamples(std::max(t0, clip->GetPlayStartTime()));
         auto clipEnd = clip->TimeToSequenceSamples(std::min(t1, clip->GetPlayEndTime()));

         float cliprms = clip->GetRMS(t0, t1, mayThrow);

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
      if (start >= clip->GetPlayStartSample() && start+len <= clip->GetPlayEndSample())
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
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy =
            std::min( start+len - clipStart, clip->GetPlaySamplesCount() );
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
   sampleCount start, size_t len, sampleFormat effectiveFormat)
{
   for (const auto &clip: mClips)
   {
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy =
            std::min( start+len - clipStart, clip->GetPlaySamplesCount() );
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
            buffer + startDelta.as_size_t() * SAMPLE_SIZE(format),
            format, inclipDelta, samplesToCopy.as_size_t(), effectiveFormat );
         clip->MarkChanged();
      }
   }
}

sampleFormat WaveTrack::WidestEffectiveFormat() const
{
   auto &clips = GetClips();
   return std::accumulate(clips.begin(), clips.end(), narrowestSampleFormat,
      [](sampleFormat format, const auto &pClip){
         return std::max(format,
            pClip->GetSequence()->GetSampleFormats().Effective());
      });
}

bool WaveTrack::HasTrivialEnvelope() const
{
   auto &clips = GetClips();
   return std::all_of(clips.begin(), clips.end(),
      [](const auto &pClip){ return pClip->GetEnvelope()->IsTrivial(); });
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
      auto dClipStartTime = clip->GetPlayStartTime();
      auto dClipEndTime = clip->GetPlayEndTime();
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
            auto nClipLen = clip->GetPlayEndSample() - clip->GetPlayStartSample();

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

// When the time is both the end of a clip and the start of the next clip, the
// latter clip is returned.
WaveClip* WaveTrack::GetClipAtTime(double time)
{
   
   const auto clips = SortedClipArray();
   auto p = std::find_if(clips.rbegin(), clips.rend(), [&] (WaveClip* const& clip) {
      return time >= clip->GetPlayStartTime() && time <= clip->GetPlayEndTime(); });

   // When two clips are immediately next to each other, the GetPlayEndTime() of the first clip
   // and the GetPlayStartTime() of the second clip may not be exactly equal due to rounding errors.
   // If "time" is the end time of the first of two such clips, and the end time is slightly
   // less than the start time of the second clip, then the first rather than the
   // second clip is found by the above code. So correct this.
   if (p != clips.rend() && p != clips.rbegin() &&
      time == (*p)->GetPlayEndTime() &&
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

WaveClip* WaveTrack::CreateClip(double offset, const wxString& name)
{
   auto clip = std::make_unique<WaveClip>(mpFactory, mFormat, mRate, GetWaveColorIndex());
   clip->SetName(name);
   clip->SetSequenceStartTime(offset);
   mClips.push_back(std::move(clip));

   return mClips.back().get();
}

WaveClip* WaveTrack::NewestOrNewClip()
{
   if (mClips.empty()) {
      return CreateClip(mOffset, MakeNewClipName());
   }
   else
      return mClips.back().get();
}

/*! @excsafety{No-fail} */
WaveClip* WaveTrack::RightmostOrNewClip()
{
   if (mClips.empty()) {
      return CreateClip(mOffset, MakeNewClipName());
   }
   else
   {
      auto it = mClips.begin();
      WaveClip *rightmost = (*it++).get();
      double maxOffset = rightmost->GetPlayStartTime();
      for (auto end = mClips.end(); it != end; ++it)
      {
         WaveClip *clip = it->get();
         double offset = clip->GetPlayStartTime();
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
         if (c->GetPlayStartTime() < clip->GetPlayEndTime() + amount &&
            c->GetPlayEndTime() > clip->GetPlayStartTime() + amount)
         {
            if (!allowedAmount)
               return false; // clips overlap

            if (amount > 0)
            {
               if (c->GetPlayStartTime() - clip->GetPlayEndTime() < *allowedAmount)
                  *allowedAmount = c->GetPlayStartTime() - clip->GetPlayEndTime();
               if (*allowedAmount < 0)
                  *allowedAmount = 0;
            } else
            {
               if (c->GetPlayEndTime() - clip->GetPlayStartTime() > *allowedAmount)
                  *allowedAmount = c->GetPlayEndTime() - clip->GetPlayStartTime();
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
      double d1 = c->GetPlayStartTime() - (clip->GetPlayEndTime()+slideBy);
      double d2 = (clip->GetPlayStartTime()+slideBy) - c->GetPlayEndTime();
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
      if (c->WithinPlayRegion(t))
      {
         t = LongSamplesToTime(TimeToLongSamples(t));
         auto newClip = std::make_unique<WaveClip>( *c, mpFactory, true );
         c->TrimRightTo(t);// put t on a sample
         newClip->TrimLeftTo(t);
         
         // This could invalidate the iterators for the loop!  But we return
         // at once so it's okay
         mClips.push_back(std::move(newClip)); // transfer ownership
         return;
      }
   }
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
            if (clip2->GetPlayStartTime() > clip->GetPlayStartTime() &&
                clip->GetPlayEndTime() + end - start > clip2->GetPlayStartTime())
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
            if (clip2->GetPlayStartTime() > clip->GetPlayStartTime())
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
   clip1->Paste(clip1->GetPlayEndTime(), clip2);
   
   // use No-fail-guarantee for the rest
   // Delete second clip
   auto it = FindClip(mClips, clip2);
   mClips.erase(it);
}

/*! @excsafety{Weak} -- Partial completion may leave clips at differing sample rates!
*/
void WaveTrack::Resample(int rate, BasicUI::ProgressDialog *progress)
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
      { return a->GetPlayStartTime() < b->GetPlayStartTime(); });
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
      ProjectRate::Get( project ),
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

ProjectFormatExtensionsRegistry::Extension smartClipsExtension(
   [](const AudacityProject& project) -> ProjectFormatVersion
   {
      const TrackList& trackList = TrackList::Get(project);

      for (auto wt : trackList.Any<const WaveTrack>())
      {
         for (const auto& clip : wt->GetAllClips())
         {
            if (clip->GetTrimLeft() > 0.0 || clip->GetTrimRight() > 0.0)
               return { 3, 1, 0, 0 };
         }
      }

      return BaseProjectFormatVersion;
   }
);

StringSetting AudioTrackNameSetting{
   L"/GUI/TrackNames/DefaultTrackName",
   // Computed default value depends on chosen language
   []{ return DefaultName.Translation(); }
};

// Bug 825 is essentially that SyncLock requires EditClipsCanMove.
// SyncLock needs rethinking, but meanwhile this function
// fixes the issues of Bug 825 by allowing clips to move when in
// SyncLock.
bool GetEditClipsCanMove()
{
   bool mIsSyncLocked = SyncLockTracks.Read();
   if( mIsSyncLocked )
      return true;
   bool editClipsCanMove;
   return EditClipsCanMove.Read();
}

BoolSetting EditClipsCanMove{
   L"/GUI/EditClipCanMove",         false  };

DEFINE_XML_METHOD_REGISTRY( WaveTrackIORegistry );
