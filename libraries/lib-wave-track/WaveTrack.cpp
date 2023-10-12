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

#include "WideClip.h"
#include "WaveClip.h"

#include <wx/defs.h>
#include <wx/debug.h>
#include <wx/log.h>

#include <algorithm>
#include <float.h>
#include <math.h>
#include <numeric>
#include <optional>
#include <type_traits>

#include "float_cast.h"

#include "AudioSegmentSampleView.h"
#include "Envelope.h"
#include "Sequence.h"

#include "Project.h"
#include "ProjectRate.h"
#include "SampleBlock.h"

#include "BasicUI.h"
#include "Prefs.h"
#include "QualitySettings.h"
#include "SyncLock.h"
#include "TimeWarper.h"


#include "InconsistencyException.h"

#include "ProjectFormatExtensionsRegistry.h"

using std::max;

WaveChannelInterval::~WaveChannelInterval() = default;

bool WaveChannelInterval::Intersects(double t0, double t1) const
{
   return GetNarrowClip().IntersectsPlayRegion(t0, t1);
}

double WaveChannelInterval::Start() const
{
   return GetNarrowClip().GetPlayStartTime();
}

double WaveChannelInterval::End() const
{
   return GetNarrowClip().GetPlayEndTime();
}

AudioSegmentSampleView
WaveChannelInterval::GetSampleView(double t0, double t1, bool mayThrow) const
{
   constexpr auto iChannel = 0u;
   // TODO wide wave tracks: use the real channel number.
   return GetNarrowClip().GetSampleView(iChannel, t0, t1, mayThrow);
}

const Envelope &WaveChannelInterval::GetEnvelope() const
{
   // Always the left clip's envelope
   return *mWideClip.GetEnvelope();
}

sampleCount WaveChannelInterval::GetVisibleSampleCount() const
{
   return GetNarrowClip().GetVisibleSampleCount();
}

int WaveChannelInterval::GetRate() const
{
   return GetNarrowClip().GetRate();
}

double WaveChannelInterval::GetPlayStartTime() const
{
   return GetNarrowClip().GetPlayStartTime();
}

double WaveChannelInterval::GetPlayEndTime() const
{
   return GetNarrowClip().GetPlayEndTime();
}

sampleCount WaveChannelInterval::TimeToSamples(double time) const
{
   return GetNarrowClip().TimeToSamples(time);
}

double WaveChannelInterval::GetStretchRatio() const
{
   return GetNarrowClip().GetStretchRatio();
}

double WaveChannelInterval::GetTrimLeft() const
{
   return GetNarrowClip().GetTrimLeft();
}

double WaveChannelInterval::GetTrimRight() const
{
   return GetNarrowClip().GetTrimRight();
}

bool WaveChannelInterval::GetSamples(samplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, bool mayThrow) const
{
   // Not just a pass-through, but supply the first argument
   // TODO wide wave tracks -- pass miChannel not 0
   return GetNarrowClip().GetSamples(0, buffer, format, start, len, mayThrow);
}

AudioSegmentSampleView WaveChannelInterval::GetSampleView(
   sampleCount start, size_t length, bool mayThrow) const
{
   // Not just a pass-through, but supply the first argument
   // TODO wide wave tracks -- pass miChannel not 0
   return GetNarrowClip().GetSampleView(0, start, length, mayThrow);
}

const Sequence &WaveChannelInterval::GetSequence() const
{
   // TODO wide wave tracks -- use miChannel
   const auto pSequence = GetNarrowClip().GetSequence(0);
   // Assume sufficiently wide clip
   assert(pSequence);
   return *pSequence;
}

constSamplePtr WaveChannelInterval::GetAppendBuffer() const
{
   // TODO wide wave tracks -- use miChannel
   return GetNarrowClip().GetAppendBuffer(0);
}

size_t WaveChannelInterval::GetAppendBufferLen() const
{
   return GetNarrowClip().GetAppendBufferLen();
}

int WaveChannelInterval::GetColourIndex() const
{
   return GetNarrowClip().GetColourIndex();
}

WaveTrack::Interval::Interval(const ChannelGroup &group,
   const std::shared_ptr<WaveClip> &pClip,
   const std::shared_ptr<WaveClip> &pClip1
)  : WideChannelGroupInterval{ group,
      pClip->GetPlayStartTime(), pClip->GetPlayEndTime() }
   , mpClip{ pClip }
   , mpClip1{ pClip1 }
{
}

WaveTrack::Interval::~Interval() = default;

void WaveTrack::Interval::TrimLeftTo(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->TrimLeftTo(t);
}

void WaveTrack::Interval::TrimRightTo(double t)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->TrimRightTo(t);
}

void WaveTrack::Interval::Stretch(double duration, bool toLeft)
{
   for(unsigned channel = 0; channel < NChannels(); ++channel)
      GetClip(channel)->Stretch(duration, toLeft);
}

void WaveTrack::Interval::ApplyStretchRatio(
   const std::function<void(double)>& reportProgress)
{
   const auto channelsCount = NChannels();

   // It is safe to assume that the track has at least one channel
   assert(channelsCount > 0);

   for (unsigned channel = 0; channel < channelsCount; ++channel)
      GetClip(channel)->ApplyStretchRatio(
         [&reportProgress, channel, channelsCount](double progress) {
            reportProgress((channel + progress) / channelsCount);
         });
}

bool WaveTrack::Interval::StretchRatioEquals(double value) const
{
   for (unsigned channel = 0; channel < NChannels(); ++channel)
   {
      if (!GetClip(channel)->StretchRatioEquals(value))
         return false;
   }
   return true;
}

void WaveTrack::Interval::SetName(const wxString& name)
{
   ForEachClip([&](auto& clip) { clip.SetName(name); });
}

const wxString& WaveTrack::Interval::GetName() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common name
   return mpClip->GetName();
}

void WaveTrack::Interval::SetColorIndex(int index)
{
   ForEachClip([&](auto& clip) { clip.SetColourIndex(index); });
}

int WaveTrack::Interval::GetColorIndex() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common color index
   return mpClip->GetColourIndex();
}

void WaveTrack::Interval::SetPlayStartTime(double time)
{
   ForEachClip([&](auto& clip) { clip.SetPlayStartTime(time); });
}

double WaveTrack::Interval::GetPlayStartTime() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common beginning
   return mpClip->GetPlayStartTime();
}

double WaveTrack::Interval::GetPlayEndTime() const
{
   // TODO wide wave tracks:  assuming that all 'narrow' clips share common
   // beginning
   return mpClip->GetPlayEndTime();
}

bool WaveTrack::Interval::IntersectsPlayRegion(double t0, double t1) const
{
   // TODO wide wave tracks:  assuming that all 'narrow' clips share common
   // boundaries
   return mpClip->IntersectsPlayRegion(t0, t1);
}

double WaveTrack::Interval::GetStretchRatio() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common stretch ratio
   return mpClip->GetStretchRatio();
}

sampleCount WaveTrack::Interval::TimeToSamples(double time) const
{
   return mpClip->TimeToSamples(time);
}

double WaveTrack::Interval::SamplesToTime(sampleCount s) const
{
   return mpClip->SamplesToTime(s);
}

double WaveTrack::Interval::GetTrimLeft() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common trims
   return mpClip->GetTrimLeft();
}

double WaveTrack::Interval::GetTrimRight() const
{
   //TODO wide wave tracks:  assuming that all 'narrow' clips share common trims
   return mpClip->GetTrimRight();
}

bool WaveTrack::Interval::IsPlaceholder() const
{
   return mpClip->GetIsPlaceholder();
}

void WaveTrack::Interval::ForEachClip(const std::function<void(WaveClip&)>& op)
{
   for(unsigned channel = 0,
      channelCount = NChannels();
      channel < channelCount; ++channel)
   {
      op(*GetClip(channel));
   }
}

std::shared_ptr<ChannelInterval>
WaveTrack::Interval::DoGetChannel(size_t iChannel)
{
   if (iChannel < NChannels()) {
      // TODO wide wave tracks: there will be only one, wide clip
      const auto pClip = (iChannel == 0 ? mpClip : mpClip1);
      return std::make_shared<WaveChannelInterval>(*mpClip,
         *pClip, iChannel);
   }
   return {};
}

std::shared_ptr<const WaveTrack::Interval>
WaveTrack::GetNextInterval(const Interval& interval, PlaybackDirection searchDirection) const
{
   std::shared_ptr<const Interval> result;
   auto bestMatchTime = searchDirection == PlaybackDirection::forward
      ? std::numeric_limits<double>::max()
      : std::numeric_limits<double>::lowest();

   for(const auto& other : Intervals())
   {
      if((searchDirection == PlaybackDirection::forward &&
         (other->Start() > interval.Start() && other->Start() < bestMatchTime))
         ||
         (searchDirection == PlaybackDirection::backward &&
         (other->Start() < interval.Start() && other->Start() > bestMatchTime)))
      {
         result = other;
         bestMatchTime = other->Start();
      }
   }
   return result;
}

std::shared_ptr<WaveTrack::Interval>
WaveTrack::GetNextInterval(const Interval& interval, PlaybackDirection searchDirection)
{
   return std::const_pointer_cast<Interval>(
      std::as_const(*this).GetNextInterval(interval, searchDirection));
}

namespace {
struct WaveTrackData : ClientData::Cloneable<> {
   WaveTrackData() = default;
   WaveTrackData(const WaveTrackData &);
   WaveTrackData& operator=(const WaveTrackData &) = delete;
   ~WaveTrackData() override;
   std::unique_ptr<ClientData::Cloneable<>> Clone() const override;

   static WaveTrackData &Get(WaveTrack &track);
   static const WaveTrackData &Get(const WaveTrack &track);

   int GetWaveColorIndex() const;
   void SetWaveColorIndex(int index);

   double GetOrigin() const;
   void SetOrigin(double origin);

   sampleFormat GetSampleFormat() const;
   void SetSampleFormat(sampleFormat format);

   float GetGain() const;
   void SetGain(float value);
   float GetPan() const;
   void SetPan(float value);

   int GetRate() const;
   void SetRate(int value);

private:
   //! Atomic because it may be read by worker threads in playback
   std::atomic<float> mGain{ 1.0f };
   //! Atomic because it may be read by worker threads in playback
   std::atomic<float> mPan{ 0.0f };

   int mRate{ 44100 };
   double mOrigin{ 0.0 };
   int mWaveColorIndex{ 0 };
   sampleFormat mFormat { floatSample };
};

static const ChannelGroup::Attachments::RegisteredFactory
waveTrackDataFactory{
   [](auto &) { return std::make_unique<WaveTrackData>(); } };

//! Copy can't be generated by default because of atomic members
WaveTrackData::WaveTrackData(const WaveTrackData &other) {
   SetGain(other.GetGain());
   SetPan(other.GetPan());
   mRate = other.mRate;
   mOrigin = other.mOrigin;
   mWaveColorIndex = other.mWaveColorIndex;
   mFormat = other.mFormat;
}

WaveTrackData::~WaveTrackData() = default;

std::unique_ptr<ClientData::Cloneable<>> WaveTrackData::Clone() const {
   return std::make_unique<WaveTrackData>(*this);
}

WaveTrackData &WaveTrackData::Get(WaveTrack &track) {
   return track.GetGroupData().Attachments
      ::Get<WaveTrackData>(waveTrackDataFactory);
}

const WaveTrackData &WaveTrackData::Get(const WaveTrack &track)
{
   return Get(const_cast<WaveTrack &>(track));
}

int WaveTrackData::GetWaveColorIndex() const
{
   return mWaveColorIndex;
}

void WaveTrackData::SetWaveColorIndex(int index)
{
   mWaveColorIndex = index;
}

double WaveTrackData::GetOrigin() const
{
   return mOrigin;
}
void WaveTrackData::SetOrigin(double origin)
{
   mOrigin = origin;
}

sampleFormat WaveTrackData::GetSampleFormat() const
{
   return mFormat;
}

void WaveTrackData::SetSampleFormat(sampleFormat format)
{
   mFormat = format;
}

float WaveTrackData::GetGain() const
{
   return mGain.load(std::memory_order_relaxed);
}

void WaveTrackData::SetGain(float value)
{
   mGain.store(value, std::memory_order_relaxed);
}

float WaveTrackData::GetPan() const
{
   return mPan.load(std::memory_order_relaxed);
}

void WaveTrackData::SetPan(float value)
{
   mPan.store(value, std::memory_order_relaxed);
}

int WaveTrackData::GetRate() const
{
   return mRate;
}

void WaveTrackData::SetRate(int value)
{
   mRate = value;
}

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

static auto DefaultName = XO("Audio");

WaveChannel::~WaveChannel() = default;

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

TrackListHolder WaveTrackFactory::Create(size_t nChannels)
{
   return Create(nChannels, QualitySettings::SampleFormatChoice(), mRate.GetRate());
}

TrackListHolder WaveTrackFactory::Create(size_t nChannels, sampleFormat format, double rate)
{
   auto channels = std::vector<std::shared_ptr<Track>>{ };
   std::generate_n(
      std::back_inserter(channels),
      nChannels,
      [&] { return Create(format, rate); }
   );
   if(nChannels == 2)
      return TrackList::Temporary(nullptr, channels[0], channels[1]);
   return TrackList::Temporary(nullptr, channels);
}

TrackListHolder WaveTrackFactory::Create(size_t nChannels, const WaveTrack& proto)
{
   auto channels = std::vector<std::shared_ptr<Track>>{};
   std::generate_n(
      std::back_inserter(channels),
      nChannels,
      [&]{ return proto.EmptyCopy(mpFactory, false); }
   );
   if(nChannels == 2)
      return TrackList::Temporary(nullptr, channels[0], channels[1]);
   return TrackList::Temporary(nullptr, channels);
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
   : mpFactory(pFactory)
{
   mLegacyProjectFileOffset = 0;

   WaveTrackData::Get(*this).SetSampleFormat(format);
   DoSetRate(static_cast<int>(rate));
}

WaveTrack::WaveTrack(const WaveTrack &orig, ProtectedCreationArg &&a)
   : WritableSampleTrack(orig, std::move(a))
   , mpFactory( orig.mpFactory )
{
   mLegacyProjectFileOffset = 0;
   for (const auto &clip : orig.mClips)
      InsertClip(std::make_shared<WaveClip>(*clip, mpFactory, true));
}

size_t WaveTrack::GetWidth() const
{
   return 1;
}

size_t WaveTrack::NChannels() const
{
   if (IsLeader() && GetOwner()) {
      auto result = TrackList::NChannels(*this);
      assert(result > 0);
      return result;
   }
   else
      return 1;
}

AudioGraph::ChannelType WaveTrack::GetChannelType() const
{
   if (TrackList::NChannels(*this) == 1)
      return AudioGraph::MonoChannel;
   else if (IsLeader())
      return AudioGraph::LeftChannel;
   else
      // TODO: more-than-two-channels
      return AudioGraph::RightChannel;
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack &orig)
{
   WritableSampleTrack::Init(orig);
   mpFactory = orig.mpFactory;
}

void WaveTrack::Reinit(const WaveTrack &orig)
{
   assert(IsLeader());
   assert(orig.IsLeader());
   assert(NChannels() == orig.NChannels());
   const auto channels = TrackList::Channels(this);
   auto iter = TrackList::Channels(&orig).begin();
   for (const auto pChannel : channels)
      pChannel->Init(**iter++);
}

WaveTrack::~WaveTrack()
{
}

/*! @excsafety{No-fail} */
void WaveTrack::MoveTo(double origin)
{
   double delta = origin - GetStartTime();
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this)) {
      for (const auto &clip : pChannel->mClips)
         // assume No-fail-guarantee
         clip->ShiftBy(delta);
   }
   WaveTrackData::Get(*this).SetOrigin(origin);
}

void WaveTrack::DoOnProjectTempoChange(
   const std::optional<double>& oldTempo, double newTempo)
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this))
      for (const auto& clip : pChannel->mClips)
         clip->OnProjectTempoChange(oldTempo, newTempo);
}

TrackListHolder
WaveTrack::DuplicateWithOtherTempo(double newTempo, WaveTrack*& leader) const
{
   const auto srcCopyList = Duplicate();
   leader = *srcCopyList->Any<WaveTrack>().begin();
   leader->OnProjectTempoChange(newTempo);
   return srcCopyList;
}

bool WaveTrack::LinkConsistencyFix(bool doFix)
{
   assert(!doFix || IsLeader());
   auto err = !WritableSampleTrack::LinkConsistencyFix(doFix);
   const auto linkType = GetLinkType();
   if (linkType != LinkType::None) {
      auto next = *TrackList::Channels(this).first.advance(1);
      if (next == nullptr) {
         //next track is absent or not a wave track, fix and report error
         if (doFix) {
            wxLogWarning(L"Right track %s is expected to be a WaveTrack."
               "\n Removing link from left wave track %s.",
               next->GetName(), GetName());
            SetLinkType(LinkType::None);
         }
         err = true;
      }
      else if (doFix) {
         // non-error upgrades happen here
         if (!AreAligned(SortedClipArray(), next->SortedClipArray()) ||
             !RateConsistencyCheck() || !FormatConsistencyCheck())
         {
            SetLinkType(LinkType::None);
         }
         else
         {
            SetLinkType(LinkType::Aligned);
            // Be sure to lose any right channel group data that might
            // have been made during during deserialization of the channel
            // before joining it
            next->DestroyGroupData();
         }
      }
   }
   if (doFix) {
      // More non-error upgrading
      // Set the common channel group rate from the leader's rate
      if (mLegacyRate > 0)
      {
         SetRate(mLegacyRate);
         mLegacyRate = 0;
         WaveTrackData::Get(*this).SetSampleFormat(mLegacyFormat);
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

Track::Holder WaveTrack::PasteInto(
   AudacityProject &project, TrackList &list) const
{
   assert(IsLeader());
   auto &trackFactory = WaveTrackFactory::Get(project);
   auto &pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
   Track::Holder pFirstTrack;
   const WaveTrack *pFirstChannel{};
   for (const auto pChannel : TrackList::Channels(this)) {
      auto pNewTrack = pChannel->EmptyCopy(pSampleBlockFactory);
      list.Add(pNewTrack);
      assert(pNewTrack->IsLeader() == pChannel->IsLeader());
      if (!pFirstTrack) {
         pFirstTrack = pNewTrack;
         pFirstChannel = pChannel;
      }
   }
   pFirstTrack->Paste(0.0, *pFirstChannel);
   return pFirstTrack;
}

size_t WaveTrack::NIntervals() const
{
   return mClips.size();
}

std::shared_ptr<WideChannelGroupInterval>
WaveTrack::DoGetInterval(size_t iInterval)
{
   if (iInterval < NIntervals()) {
      WaveClipHolder pClip = mClips[iInterval],
         pClip1;
      // TODO wide wave tracks
      // This assumed correspondence of clips may be wrong if they misalign
      if (auto right = ChannelGroup::GetChannel<WaveTrack>(1)
         ; right && iInterval < right->mClips.size()
      )
         pClip1 = right->mClips[iInterval];
      return std::make_shared<Interval>(*this, pClip, pClip1);
   }
   return {};
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

std::shared_ptr<::Channel> WaveTrack::DoGetChannel(size_t iChannel)
{
   auto nChannels = NChannels();
   if (iChannel >= nChannels)
      return {};
   auto pTrack = (iChannel == 0)
      ? this
      // TODO: more-than-two-channels
      : *TrackList::Channels(this).rbegin();
   // Use aliasing constructor of std::shared_ptr
   ::Channel *alias = pTrack;
   return { pTrack->shared_from_this(), alias };
}

ChannelGroup &WaveTrack::DoGetChannelGroup() const
{
   const ChannelGroup &group = *this;
   return const_cast<ChannelGroup&>(group);
}

ChannelGroup &WaveTrack::ReallyDoGetChannelGroup() const
{
   const Track *pTrack = this;
   if (const auto pOwner = GetHolder())
      pTrack = *pOwner->Find(this);
   const ChannelGroup &group = *pTrack;
   return const_cast<ChannelGroup&>(group);
}

TrackListHolder WaveTrack::Clone() const
{
   assert(IsLeader());
   auto result = TrackList::Temporary(nullptr);
   const auto cloneOne = [&](const WaveTrack *pChannel){
      const auto pTrack =
         std::make_shared<WaveTrack>(*pChannel, ProtectedCreationArg{});
      pTrack->Init(*pChannel);
      result->Add(pTrack);
   };
   if (GetOwner())
      for (const auto pChannel : TrackList::Channels(this))
         cloneOne(pChannel);
   else
      cloneOne(this);
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
   return WaveTrackData::Get(*this).GetRate();
}

void WaveTrack::SetRate(double newRate)
{
   assert( newRate > 0 );
   newRate = std::max( 1.0, newRate );
   DoSetRate(newRate);

   for(const auto& channel : Channels())
      channel->GetTrack().SetClipRates(newRate);
}

void WaveTrack::DoSetRate(double newRate)
{
   auto &data = WaveTrackData::Get(*this);
   data.SetRate(static_cast<int>(newRate));
}

void WaveTrack::SetClipRates(double newRate)
{
   for (const auto &clip : mClips)
      clip->SetRate(static_cast<int>(newRate));
}

float WaveTrack::GetGain() const
{
   return WaveTrackData::Get(*this).GetGain();
}

void WaveTrack::DoSetGain(float value)
{
   WaveTrackData::Get(*this).SetGain(value);
}

void WaveTrack::SetGain(float newGain)
{
   if (GetGain() != newGain) {
      DoSetGain(newGain);
      Notify(true);
   }
}

float WaveTrack::GetPan() const
{
   return WaveTrackData::Get(*this).GetPan();
}

void WaveTrack::DoSetPan(float value)
{
   WaveTrackData::Get(*this).SetPan(value);
}

void WaveTrack::SetPan(float newPan)
{
   if (newPan > 1.0)
      newPan = 1.0;
   else if (newPan < -1.0)
      newPan = -1.0;

   if ( GetPan() != newPan ) {
      DoSetPan(newPan);
      Notify(true);
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

int WaveTrack::GetWaveColorIndex() const
{
   return WaveTrackData::Get(*this).GetWaveColorIndex();
}

/*! @excsafety{Strong} */
void WaveTrack::SetWaveColorIndex(int colorIndex)
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this)) {
      for (const auto &clip : pChannel->mClips)
         clip->SetColourIndex(colorIndex);
   }
   WaveTrackData::Get(*this).SetWaveColorIndex(colorIndex);
}

sampleCount WaveTrack::GetVisibleSampleCount() const
{
    sampleCount result{ 0 };

    for (const auto& clip : mClips)
        result += clip->GetVisibleSampleCount();

    return result;
}

sampleCount WaveTrack::GetSequenceSamplesCount() const
{
   assert(IsLeader());
   sampleCount result{ 0 };

   for (const auto pChannel : TrackList::Channels(this))
      for (const auto& clip : pChannel->mClips)
         result += clip->GetSequenceSamplesCount();

   return result;
}

size_t WaveTrack::CountBlocks() const
{
   assert(IsLeader());
   size_t result{};
   for (const auto pChannel : TrackList::Channels(this)) {
      for (auto& clip : pChannel->GetClips())
         result += clip->GetWidth() * clip->GetSequenceBlockArray(0)->size();
   }
   return result;
}

sampleFormat WaveTrack::GetSampleFormat() const
{
   return WaveTrackData::Get(*this).GetSampleFormat();
}

/*! @excsafety{Weak} -- Might complete on only some clips */
void WaveTrack::ConvertToSampleFormat(sampleFormat format,
   const std::function<void(size_t)> & progressReport)
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this)) {
      for (const auto& clip : pChannel->mClips)
         clip->ConvertToSampleFormat(format, progressReport);
   }
   WaveTrackData::Get(*this).SetSampleFormat(format);
}


bool WaveTrack::IsEmpty(double t0, double t1) const
{
   if (t0 > t1)
      return true;

   //wxPrintf("Searching for overlap in %.6f...%.6f\n", t0, t1);
   for (const auto &clip : mClips)
   {
      if (clip->IntersectsPlayRegion(t0, t1)) {
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

TrackListHolder WaveTrack::Cut(double t0, double t1)
{
   assert(IsLeader());
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto result = Copy(t0, t1);
   Clear(t0, t1);
   return result;
}

/*! @excsafety{Strong} */
TrackListHolder WaveTrack::SplitCut(double t0, double t1)
{
   assert(IsLeader());
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   // SplitCut is the same as 'Copy', then 'SplitDelete'
   auto result = Copy(t0, t1);
   SplitDelete(t0, t1);
   return result;
}

//Trim trims within a clip, rather than trimming everything.
//If a bound is outside a clip, it trims everything.
/*! @excsafety{Weak} */
void WaveTrack::Trim (double t0, double t1)
{
   assert(IsLeader());
   bool inside0 = false;
   bool inside1 = false;

   const auto range = TrackList::Channels(this);
   for (auto pChannel : range) {
      for (const auto &clip : pChannel->mClips) {
         if (t1 > clip->GetPlayStartTime() && t1 < clip->GetPlayEndTime()) {
            clip->SetTrimRight(
               clip->GetTrimRight() + clip->GetPlayEndTime() - t1);
            inside1 = true;
         }

         if (t0 > clip->GetPlayStartTime() && t0 < clip->GetPlayEndTime()) {
            clip->SetTrimLeft(
               clip->GetTrimLeft() + t0 - clip->GetPlayStartTime());
            inside0 = true;
         }
      }
   }

   //if inside0 is false, then the left selector was between
   //clips, so DELETE everything to its left.
   if (const auto endTime = GetEndTime()
      ; !inside1 && t1 < endTime
   )
      Clear(t1, endTime);

   if (const auto startTime = GetStartTime()
      ; !inside0 && t0 > startTime
   )
      SplitDelete(startTime, t0);
}

WaveTrack::Holder WaveTrack::EmptyCopy(
   const SampleBlockFactoryPtr &pFactory, bool keepLink) const
{
   const auto rate = GetRate();
   auto result = std::make_shared<WaveTrack>(pFactory, GetSampleFormat(), rate);
   result->Init(*this);
   // The previous line might have destroyed the rate information stored in
   // channel group data.  The copy is not yet in a TrackList.  Reassign rate
   // in case the track needs to make WaveClips before it is properly joined
   // with the opposite channel in a TrackList.
   // TODO wide wave tracks -- all of the comment above will be irrelevant!
   result->DoSetRate(rate);
   result->mpFactory = pFactory ? pFactory : mpFactory;
   if (!keepLink)
      result->SetLinkType(LinkType::None);
   return result;
}

TrackListHolder WaveTrack::WideEmptyCopy(
   const SampleBlockFactoryPtr &pFactory, bool keepLink) const
{
   assert(IsLeader());
   auto result = TrackList::Temporary(nullptr);
   for (const auto pChannel : TrackList::Channels(this)) {
      const auto pNewTrack =
         result->Add(pChannel->EmptyCopy(pFactory, keepLink));
      assert(!keepLink || pNewTrack->IsLeader() == pChannel->IsLeader());
   }
   return result;
}

TrackListHolder WaveTrack::MonoToStereo()
{
   assert(!GetOwner());

   auto result = Duplicate();
   result->MakeMultiChannelTrack(**result->begin(), 2);

   return result;
}

TrackListHolder WaveTrack::Copy(double t0, double t1, bool forClipboard) const
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto list = TrackList::Create(nullptr);
   for (const auto pChannel : TrackList::Channels(this))
      list->Add(CopyOne(*pChannel, t0, t1, forClipboard));
   return list;
}

auto WaveTrack::CopyOne(
   const WaveTrack &track, double t0, double t1, bool forClipboard) -> Holder
{
   const auto &pFactory = track.mpFactory;
   auto result = track.EmptyCopy();
   WaveTrack *newTrack = result.get();

   // PRL:  Why shouldn't cutlines be copied and pasted too?  I don't know,
   // but that was the old behavior.  But this function is also used by the
   // Duplicate command and I changed its behavior in that case.

   for (const auto &clip : track.mClips) {
      if(clip->IsEmpty())
         continue;

      if (t0 <= clip->GetPlayStartTime() && t1 >= clip->GetPlayEndTime()) {
         // Whole clip is in copy region
         //wxPrintf("copy: clip %i is in copy region\n", (int)clip);

         newTrack->InsertClip(
            std::make_shared<WaveClip>(*clip, pFactory, !forClipboard));
         WaveClip *const newClip = newTrack->mClips.back().get();
         newClip->ShiftBy(-t0);
      }
      else if (clip->CountSamples(t0, t1) >= 1) {
         // Clip is affected by command
         //wxPrintf("copy: clip %i is affected by command\n", (int)clip);

         auto newClip = std::make_shared<WaveClip>(
            *clip, pFactory, !forClipboard, t0, t1);
         newClip->SetName(clip->GetName());

         newClip->ShiftBy(-t0);
         if (newClip->GetPlayStartTime() < 0)
            newClip->SetPlayStartTime(0);

         newTrack->InsertClip(std::move(newClip)); // transfer ownership
      }
   }

   // AWD, Oct 2009: If the selection ends in whitespace, create a
   // placeholder clip representing that whitespace
   // PRL:  Only if we want the track for pasting into other tracks.  Not if
   // it goes directly into a project as in the Duplicate command.
   if (forClipboard &&
       newTrack->GetEndTime() + 1.0 / newTrack->GetRate() < t1 - t0) {
      // TODO wide wave tracks -- match clip width of newTrack
      auto placeholder = std::make_shared<WaveClip>(1, pFactory,
         newTrack->GetSampleFormat(),
         static_cast<int>(newTrack->GetRate()),
         0 /*colourindex*/);
      placeholder->SetIsPlaceholder(true);
      placeholder->InsertSilence(0, (t1 - t0) - newTrack->GetEndTime());
      placeholder->ShiftBy(newTrack->GetEndTime());
      newTrack->InsertClip(std::move(placeholder)); // transfer ownership
   }
   return newTrack->SharedPointer<WaveTrack>();
}

/*! @excsafety{Strong} */
void WaveTrack::Clear(double t0, double t1)
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this))
      pChannel->HandleClear(t0, t1, false, false);
}

/*! @excsafety{Strong} */
void WaveTrack::ClearAndAddCutLine(double t0, double t1)
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this))
      pChannel->HandleClear(t0, t1, true, false);
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
void WaveTrack::ClearAndPaste(
   double t0,                      // Start of time to clear
   double t1,                      // End of time to clear
   const WaveTrack& src,           // What to paste
   bool preserve,                  // Whether to reinsert splits/cuts
   bool merge,                     // Whether to remove 'extra' splits
   const TimeWarper* effectWarper, // How does time change
   bool clearByTrimming)
{
   // Get a modifiable copy of `src` because it may come from another project
   // with different tempo, making boundary queries incorrect.
   const auto& tempo = GetProjectTempo();
   if (!tempo.has_value())
      THROW_INCONSISTENCY_EXCEPTION;
   WaveTrack* copy;
   const auto copyHolder = src.DuplicateWithOtherTempo(*tempo, copy);
   ClearAndPasteAtSameTempo(
      t0, t1, *copy, preserve, merge, effectWarper, clearByTrimming);
}

void WaveTrack::ClearAndPasteAtSameTempo(
   double t0, double t1, const WaveTrack& src, bool preserve, bool merge,
   const TimeWarper* effectWarper, bool clearByTrimming)
{
   const auto srcNChannels = src.NChannels();
   assert(IsLeader());
   assert(src.IsLeader());
   assert(srcNChannels == 1 || srcNChannels == NChannels());
   assert(
      GetProjectTempo().has_value() &&
      GetProjectTempo() == src.GetProjectTempo());

   t0 = SnapToSample(t0);
   t1 = SnapToSample(t1);

   const auto startTime = src.GetStartTime();
   const auto endTime = src.GetEndTime();
   double dur = std::min(t1 - t0, endTime);

   // If duration is 0, then it's just a plain paste
   if (dur == 0.0) {
      // use Weak-guarantee
      PasteWaveTrack(t0, src, merge);
      return;
   }

   auto iter = TrackList::Channels(&src).begin();
   const auto myChannels = TrackList::Channels(this);
   for (const auto pChannel : myChannels) {
      ClearAndPasteOne(
         *pChannel, t0, t1, startTime, endTime, **iter, preserve, merge,
         effectWarper, clearByTrimming);
      if (srcNChannels > 1)
         ++iter;
   }
}

void WaveTrack::ClearAndPasteOne(
   WaveTrack& track, double t0, double t1, const double startTime,
   const double endTime, const WaveTrack& src, bool preserve, bool merge,
   const TimeWarper* effectWarper, bool clearByTrimming)
{
   const auto pFactory = track.mpFactory;

   std::vector<SplitInfo> splits;
   WaveClipHolders cuts;

   //helper routine, that finds SplitInfo by time value,
   //or creates a new one if no one exists yet
   auto get_split = [&](double time) {
      auto it = std::find_if(splits.begin(), splits.end(),
         [time](const SplitInfo& split) { return split.time == time; });
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

   const auto roundTime = [&track](double t){
      return track.SnapToSample(t);
   };

   // Align to a sample
   t0 = roundTime(t0);
   t1 = roundTime(t1);

   // Save the cut/split lines whether preserving or not since merging
   // needs to know if a clip boundary is being crossed since Paste()
   // will add split lines around the pasted clip if so.
   for (const auto &clip : track.mClips) {
      double st;

      // Remember clip boundaries as locations to split
      // we need to copy clips, trims and names, because the original ones
      // could be changed later during Clear/Paste routines
      st = roundTime(clip->GetPlayStartTime());
      if (st >= t0 && st <= t1) {
         auto it = get_split(st);
         if (clip->GetTrimLeft() != 0) {
            //keep only hidden left part
            it->right = std::make_shared<WaveClip>(*clip, pFactory, false);
            it->right->SetTrimLeft(.0);
            it->right->ClearRight(clip->GetPlayStartTime());
         }
         it->rightClipName = clip->GetName();
      }

      st = roundTime(clip->GetPlayEndTime());
      if (st >= t0 && st <= t1) {
         auto it = get_split(st);
         if (clip->GetTrimRight() != 0) {
            //keep only hidden right part
            it->left = std::make_shared<WaveClip>(*clip, pFactory, false);
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
         const double cs = roundTime(
            clip->GetSequenceStartTime() + cut->GetSequenceStartTime());

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

   const auto tolerance = 2.0 / track.GetRate();

   // The split option to `HandleClear` will trim rather than clear `[t0, t1]`.
   const auto split = clearByTrimming;

   // Shift clip at t1 to t0, so that it appends itself to what we'll be pasting
   // after the clear, and we get neither the "There is not enough room" message
   // nor empty space.
   constexpr auto shiftClipAtT1ToT0 = true;

   // Now, clear the selection
   track.HandleClear(t0, t1, false, split, shiftClipAtT1ToT0);

   // And paste in the new data
   PasteOne(track, t0, src, startTime, endTime, !split);

   // First, merge the new clip(s) in with the existing clips
   if (merge && splits.size() > 0) {
      {
         // Now t1 represents the absolute end of the pasted data.
         t1 = t0 + endTime;

         // Get a sorted array of the clips
         auto clips = track.SortedClipArray();

         // Scan the sorted clips for the first clip whose start time
         // exceeds the pasted regions end time.
         {
            WaveClip *prev = nullptr;
            for (const auto clip : clips) {
               // Merge this clip and the previous clip if the end time
               // falls within it and this isn't the first clip in the track.
               if (fabs(t1 - clip->GetPlayStartTime()) < tolerance) {
                  if (prev && clip->HasEqualStretchRatio(*prev))
                     track.MergeOneClipPair(track.GetClipIndex(prev),
                        track.GetClipIndex(clip));
                  break;
               }
               prev = clip;
            }
         }
      }

      {
         // Refill the array since clips have changed.
         auto clips = track.SortedClipArray();

         // Scan the sorted clips to look for the start of the pasted
         // region.
         WaveClip *prev = nullptr;
         for (const auto clip : clips) {
            if (prev) {
               // It must be that clip is what was pasted and it begins where
               // prev ends.
               // use Weak-guarantee
               if (clip->HasEqualStretchRatio(*prev))
                  track.MergeOneClipPair(
                     track.GetClipIndex(prev), track.GetClipIndex(clip));
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
      auto attachLeft = [](WaveClip& target, WaveClip& src) {
         // What this lambda does is restoring the left hidden data of `target`
         // that was cleared by `HandleClear`. Hence, `target` has no left
         // hidden data at this stage.
         assert(target.GetTrimLeft() == 0);
         if (target.GetTrimLeft() != 0)
            return;

         // `src` was created by copy from `target`, so they have equal width
         // and stretch ratio.
         assert(target.GetWidth() == src.GetWidth());
         assert(target.GetStretchRatio() == src.GetStretchRatio());

         auto trim = src.GetPlayEndTime() - src.GetPlayStartTime();
         auto success = target.Paste(target.GetPlayStartTime(), src);
         assert(success); // because of precondition above
         target.SetTrimLeft(trim);
         //Play start time needs to be adjusted after
         //prepending data to the sequence
         target.ShiftBy(-trim);
      };

      auto attachRight = [](WaveClip &target, WaveClip &src)
      {
         // See `attachLeft` for rationale behind these asserts.
         assert(target.GetTrimRight() == 0);
         if (target.GetTrimRight() != 0)
            return;
         assert(target.GetWidth() == src.GetWidth());
         assert(target.GetStretchRatio() == src.GetStretchRatio());

         auto trim = src.GetPlayEndTime() - src.GetPlayStartTime();
         auto success = target.Paste(target.GetPlayEndTime(), src);
         assert(success); // because of precondition above
         target.SetTrimRight(trim);
      };

      // Restore the split lines and trims, transforming the position appropriately
      for (const auto& split: splits) {
         auto at = roundTime(warper->Warp(split.time));
         for (const auto& clip : track.GetClips()) {
            // Clips in split began as copies of a clip in the track,
            // therefore have the same width, satisfying preconditions to
            // attach
            if (clip->SplitsPlayRegion(at))//strictly inside
            {
               auto newClip =
                  std::make_shared<WaveClip>(*clip, pFactory, true);

               clip->ClearRight(at);
               newClip->ClearLeft(at);
               if (split.left)
                  // clip was cleared right
                  attachRight(*clip, *split.left);
               if (split.right)
                  // new clip was cleared left
                  attachLeft(*newClip, *split.right);
               bool success = track.AddClip(std::move(newClip));
               assert(success); // copied clip has same width and factory
               break;
            }
            else if (clip->GetPlayStartSample() ==
               track.TimeToLongSamples(at) && split.right) {
               // Satisfy the precondition of attachLeft first!
               const auto trim = clip->GetTrimLeft();
               const auto seqStartTime = clip->GetSequenceStartTime();
               clip->Clear(seqStartTime, seqStartTime + trim);
               // This clearing, although only removing the hidden part, moved
               // the clip leftwards. We don't want this in this case.
               clip->ShiftBy(trim);
               attachLeft(*clip, *split.right);
               break;
            }
            else if (clip->GetPlayEndSample() ==
               track.TimeToLongSamples(at) && split.left) {
               // Satisfy the precondition of attachRight first!
               clip->Clear(
                  clip->GetPlayEndTime(), clip->GetSequenceEndTime());
               attachRight(*clip, *split.left);
               break;
            }
         }
      }

      //Restore clip names
      for (const auto& split : splits)
      {
         auto s = track.TimeToLongSamples(warper->Warp(split.time));
         for (auto& clip : track.GetClips()) {
            if (split.rightClipName.has_value() && clip->GetPlayStartSample() == s)
               clip->SetName(*split.rightClipName);
            else if (split.leftClipName.has_value() && clip->GetPlayEndSample() == s)
               clip->SetName(*split.leftClipName);
         }
      }

      // Restore the saved cut lines, also transforming if time altered
      for (const auto &clip : track.mClips) {
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

/*! @excsafety{Strong} */
void WaveTrack::SplitDelete(double t0, double t1)
{
   assert(IsLeader());
   bool addCutLines = false;
   bool split = true;
   for (const auto pChannel : TrackList::Channels(this))
      pChannel->HandleClear(t0, t1, addCutLines, split);
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
   assert(clip);
   if (clip->GetSequence(0)->GetFactory() != this->mpFactory)
      return false;

   if (clip->GetWidth() != GetWidth())
      return false;

   // Uncomment the following line after we correct the problem of zero-length clips
   //if (CanInsertClip(clip))
   InsertClip(clip); // transfer ownership

   return true;
}

/*! @excsafety{Strong} */
void WaveTrack::HandleClear(
   double t0, double t1, bool addCutLines, bool split, bool shiftClipAtT1ToT0)
{
   // For debugging, use an ASSERT so that we stop
   // closer to the problem.
   wxASSERT( t1 >= t0 );
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   t0 = SnapToSample(t0);
   t1 = SnapToSample(t1);

   WaveClipPointers clipsToDelete;
   WaveClipHolders clipsToAdd;

   // We only add cut lines when deleting in the middle of a single clip
   // The cut line code is not really prepared to handle other situations
   if (addCutLines)
   {
      for (const auto &clip : mClips)
      {
         if (clip->PartlyWithinPlayRegion(t0, t1))
         {
            addCutLines = false;
            break;
         }
      }
   }

   for (const auto &clip : mClips)
   {
      if (clip->CoversEntirePlayRegion(t0, t1))
      {
         // Whole clip must be deleted - remember this
         clipsToDelete.push_back(clip.get());
      }
      else if (clip->IntersectsPlayRegion(t0, t1))
      {
         // Clip data is affected by command
         if (addCutLines)
         {
            // Don't modify this clip in place, because we want a strong
            // guarantee, and might modify another clip
            clipsToDelete.push_back( clip.get() );
            auto newClip =
               std::make_shared<WaveClip>(*clip, mpFactory, true);
            newClip->ClearAndAddCutLine( t0, t1 );
            clipsToAdd.push_back( std::move( newClip ) );
         }
         else
         {
            if (split) {
               // Three cases:

               if (clip->BeforePlayRegion(t0)) {
                  // Delete from the left edge

                  // Don't modify this clip in place, because we want a strong
                  // guarantee, and might modify another clip
                  clipsToDelete.push_back( clip.get() );
                  auto newClip =
                     std::make_shared<WaveClip>(*clip, mpFactory, true);
                  newClip->TrimLeft(t1 - clip->GetPlayStartTime());
                  clipsToAdd.push_back( std::move( newClip ) );
               }
               else if (clip->AfterPlayRegion(t1)) {
                  // Delete to right edge

                  // Don't modify this clip in place, because we want a strong
                  // guarantee, and might modify another clip
                  clipsToDelete.push_back( clip.get() );
                  auto newClip =
                     std::make_shared<WaveClip>(*clip, mpFactory, true);
                  newClip->TrimRight(clip->GetPlayEndTime() - t0);

                  clipsToAdd.push_back( std::move( newClip ) );
               }
               else {
                  // Delete in the middle of the clip...we actually create two
                  // NEW clips out of the left and right halves...

                  auto leftClip =
                     std::make_shared<WaveClip>(*clip, mpFactory, true);
                  leftClip->TrimRight(clip->GetPlayEndTime() - t0);
                  clipsToAdd.push_back(std::move(leftClip));

                  auto rightClip =
                     std::make_shared<WaveClip>(*clip, mpFactory, true);
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
               auto newClip =
                  std::make_shared<WaveClip>(*clip, mpFactory, true);

               // clip->Clear keeps points < t0 and >= t1 via Envelope::CollapseRegion
               newClip->Clear(t0,t1);

               clipsToAdd.push_back( std::move( newClip ) );
            }
         }
      }
   }

   // Only now, change the contents of this track
   // use No-fail-guarantee for the rest

   const auto moveClipsLeft = !split && GetEditClipsCanMove();
   if (moveClipsLeft)
      // Clip is "behind" the region -- offset it unless we're splitting
      // or we're using the "don't move other clips" mode
      for (const auto& clip : mClips)
         if (clip->AtOrBeforePlayRegion(t1))
            clip->ShiftBy(-(t1 - t0));

   for (const auto &clip: clipsToDelete)
   {
      auto myIt = FindClip(mClips, clip);
      if (myIt != mClips.end())
         mClips.erase(myIt); // deletes the clip!
      else
         wxASSERT(false);
   }

   for (auto &clip: clipsToAdd)
      InsertClip(std::move(clip)); // transfer ownership

   if (!moveClipsLeft && shiftClipAtT1ToT0)
      if (const auto clip = GetClipAtTime(t1);
          clip && clip->GetPlayStartTime() == t1)
         clip->ShiftBy(-(t1 - t0));
}

void WaveTrack::SyncLockAdjust(double oldT1, double newT1)
{
   assert(IsLeader());
   const auto endTime = GetEndTime();
   if (newT1 > oldT1 &&
      // JKC: This is a rare case where using >= rather than > on a float matters.
      // GetEndTime() looks through the clips and may give us EXACTLY the same
      // value as T1, when T1 was set to be at the end of one of those clips.
      oldT1 >= endTime)
         return;
   const auto channels = TrackList::Channels(this);
   if (newT1 > oldT1) {
      // Insert space within the track

      // If track is empty at oldT1 insert whitespace; otherwise, silence
      if (IsEmpty(oldT1, oldT1)) {
         // Check if clips can move
         if (EditClipsCanMove.Read()) {
            const auto offset = newT1 - oldT1;
            const auto rate = GetRate();
            for (const auto pChannel : channels)
               for (const auto& clip : pChannel->mClips)
                  if (clip->GetPlayStartTime() > oldT1 - (1.0 / rate))
                     clip->ShiftBy(offset);
         }
         return;
      }
      else {
         // AWD: Could just use InsertSilence() on its own here, but it doesn't
         // follow EditClipCanMove rules (Paste() does it right)
         const auto duration = newT1 - oldT1;
         for (const auto pChannel : channels) {
            auto tmp = std::make_shared<WaveTrack>(
               mpFactory, GetSampleFormat(), GetRate());
            // tmpList exists only to fix assertion crashes in usage of tmp
            auto tmpList = TrackList::Temporary(nullptr, tmp, nullptr);
            assert(tmp->IsLeader()); // It is not yet owned by a TrackList
            tmp->InsertSilence(0.0, duration);
            tmp->FlushOne();
            PasteOne(*pChannel, oldT1, *tmp, 0.0, duration);
         }
      }
   }
   else if (newT1 < oldT1)
      Clear(newT1, oldT1);
}

void WaveTrack::PasteWaveTrack(double t0, const WaveTrack& other, bool merge)
{
   // Get a modifiable copy of `src` because it may come from another project
   // with different tempo, making boundary queries incorrect.
   const auto& tempo = GetProjectTempo();
   if (!tempo.has_value())
      THROW_INCONSISTENCY_EXCEPTION;
   WaveTrack* copy;
   const auto copyHolder = other.DuplicateWithOtherTempo(*tempo, copy);
   PasteWaveTrackAtSameTempo(t0, *copy, merge);
}

void WaveTrack::PasteWaveTrackAtSameTempo(
   double t0, const WaveTrack& other, bool merge)
{
   assert(IsLeader());
   const auto otherNChannels = other.NChannels();
   assert(otherNChannels == 1 || otherNChannels == NChannels());
   assert(
      GetProjectTempo().has_value() &&
      GetProjectTempo() == other.GetProjectTempo());
   const auto startTime = other.GetStartTime();
   const auto endTime = other.GetEndTime();
   auto iter = TrackList::Channels(&other).begin();
   for (const auto pChannel : TrackList::Channels(this)) {
      PasteOne(*pChannel, t0, **iter, startTime, endTime, merge);
      if (otherNChannels > 1)
         ++iter;
   }
}

void WaveTrack::PasteOne(
   WaveTrack& track, double t0, const WaveTrack& other, const double startTime,
   const double insertDuration, bool merge)
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

    if (other.GetNumClips() == 0)
        return;

    t0 = track.SnapToSample(t0);

    //wxPrintf("paste: we have at least one clip\n");

    const auto clipAtT0 = track.GetClipAtTime(t0);
    const auto otherFirstClip = other.GetLeftmostClip();
    const auto otherLastClip = other.GetRightmostClip();
    const auto stretchRatiosMatch =
       !clipAtT0 || (clipAtT0->HasEqualStretchRatio(*otherFirstClip) &&
                     clipAtT0->HasEqualStretchRatio(*otherLastClip));

    // `singleClipMode` will try to merge. Only allow this if clips on both ends
    // of the selection have equal stretch ratio.
    const bool singleClipMode =
       other.GetNumClips() == 1 &&
       std::abs(startTime) < track.LongSamplesToTime(1) * 0.5 &&
       stretchRatiosMatch && merge;

    const auto rate = track.GetRate();
    if (insertDuration != 0 && insertDuration < 1.0 / rate)
        // PRL:  I added this check to avoid violations of preconditions in other WaveClip and Sequence
        // methods, but allow the value 0 so I don't subvert the purpose of commit
        // 739422ba70ceb4be0bb1829b6feb0c5401de641e which causes append-recording always to make
        // a new clip.
        return;

    //wxPrintf("Check if we need to make room for the pasted data\n");

    auto pastingFromTempTrack = !other.GetOwner();
    bool editClipCanMove = GetEditClipsCanMove();

    const SimpleMessageBoxException notEnoughSpaceException {
       ExceptionType::BadUserAction,
       XO("There is not enough room available to paste the selection"),
       XO("Warning"), "Error:_Insufficient_space_in_track"
    };

    // Make room for the pasted data
    if (editClipCanMove) {
        if (!singleClipMode) {
            // We need to insert multiple clips, so split the current clip and ...
            track.SplitAt(t0);
        }
        //else if there is a clip at t0 insert new clip inside it and ...

        // ... move everything to the right
        for (const auto& clip : track.mClips)
            if (clip->GetPlayStartTime() > t0 - (1.0 / rate))
                clip->ShiftBy(insertDuration);
    }
    else
    {
       if (!merge)
          track.SplitAt(t0);
       const auto clipAtT0 = track.GetClipAtTime(t0);
       const auto t = clipAtT0 ? clipAtT0->GetPlayEndTime() : t0;
       if (!track.IsEmpty(t, t + insertDuration))
          throw notEnoughSpaceException;
       if (clipAtT0 && clipAtT0->GetPlayStartTime() == t0)
          clipAtT0->ShiftBy(insertDuration);
    }

    if (singleClipMode) {
        // Single clip mode
        // wxPrintf("paste: checking for single clip mode!\n");

        WaveClip* insideClip = nullptr;
        for (const auto& clip : track.mClips) {
            if (editClipCanMove) {
                if (clip->WithinPlayRegion(t0)) {
                    //wxPrintf("t0=%.6f: inside clip is %.6f ... %.6f\n",
                    //       t0, clip->GetStartTime(), clip->GetEndTime());
                    insideClip = clip.get();
                    break;
                }
            }
            else {
                // If clips are immovable we also allow prepending to clips
                if (clip->WithinPlayRegion(t0) ||
                    track.TimeToLongSamples(t0) == clip->GetPlayStartSample())
                {
                    insideClip = clip.get();
                    break;
                }
            }
        }

        if (insideClip) {
            // Exhibit traditional behaviour
            //wxPrintf("paste: traditional behaviour\n");
            if (!editClipCanMove) {
                // We did not move other clips out of the way already, so
                // check if we can paste without having to move other clips
                for (const auto& clip : track.mClips) {
                    if (clip->GetPlayStartTime() > insideClip->GetPlayStartTime() &&
                        insideClip->GetPlayEndTime() + insertDuration >
                        clip->GetPlayStartTime())
                        // Strong-guarantee in case of this path
                        // not that it matters.
                        throw notEnoughSpaceException;
                }
            }
            if (auto *pClip = other.GetClipByIndex(0)) {
               // This branch only gets executed in `singleClipMode` - we've
               // already made sure that stretch ratios are equal, satisfying
               // `WaveClip::Paste`'s precondition.
               bool success = insideClip->Paste(t0, *pClip);
               // TODO wide wave tracks -- prove success, or propagate failure,
               // or we might throw a MessageBoxException
               // (which would require a change in base class Track)
               // for now it would be quiet failure if clip widths mismatched
               // Can't yet assert(success);
            }
            return;
        }
        // Just fall through and exhibit NEW behaviour
    }

    // Insert NEW clips
    //wxPrintf("paste: multi clip mode!\n");

    if (!editClipCanMove &&
        !track.IsEmpty(t0, t0 + insertDuration - 1.0 / rate))
        // Strong-guarantee in case of this path
        // not that it matters.
        throw notEnoughSpaceException;

    for (const auto& clip : other.mClips) {
        // AWD Oct. 2009: Don't actually paste in placeholder clips
        if (!clip->GetIsPlaceholder()) {
            auto newClip =
                std::make_shared<WaveClip>(*clip, track.mpFactory, true);
            newClip->Resample(rate);
            newClip->ShiftBy(t0);
            newClip->MarkChanged();
            if (pastingFromTempTrack)
                //Clips from the tracks which aren't bound to any TrackList are
                //considered to be new entities, thus named using "new" name template
                newClip->SetName(track.MakeNewClipName());
            else
                newClip->SetName(track.MakeClipCopyName(clip->GetName()));
            track.InsertClip(std::move(newClip)); // transfer ownership
        }
    }
}

bool WaveTrack::RateConsistencyCheck() const
{
   assert(IsLeader());

   // The channels and all clips in them should have the same sample rate.
   std::optional<double> oRate;
   auto channels = TrackList::Channels(this);
   return std::all_of(channels.begin(), channels.end(),
      [&](const WaveTrack *pTrack){
         if (!pTrack)
            return false;

         const auto rate = pTrack->mLegacyRate;
         if (!oRate)
            oRate = rate;
         else if (*oRate != rate)
            return false;
         return true;
      });
}

bool WaveTrack::FormatConsistencyCheck() const
{
   assert(IsLeader());

   const auto channels = TrackList::Channels(this);
   return std::all_of(channels.begin(), channels.end(),
      [&](const WaveTrack *pTrack){
         return pTrack && pTrack->mLegacyFormat == mLegacyFormat;
      });
}

void WaveTrack::InsertClip(WaveClipHolder clip)
{
   assert(clip->GetIsPlaceholder() || !clip->IsEmpty());
   if(!clip->GetIsPlaceholder() && clip->IsEmpty())
      return;

   const auto& tempo = GetProjectTempo();
   if (tempo.has_value())
      clip->OnProjectTempoChange(std::nullopt, *tempo);
   mClips.push_back(std::move(clip));
}

void WaveTrack::ApplyStretchRatio(
   std::optional<TimeInterval> interval, ProgressReporter reportProgress)
{
   assert(IsLeader());
   // Assert that the interval is reasonable, but this function will be no-op
   // anyway if not
   assert(!interval.has_value() ||
          interval->first <= interval->second);
   if (GetNumClips() == 0)
      return;
   const auto startTime =
      interval ? std::max(SnapToSample(interval->first), GetStartTime()) :
                 GetStartTime();
   const auto endTime =
      interval ? std::min(SnapToSample(interval->second), GetEndTime()) :
                 GetEndTime();
   if (startTime >= endTime)
      return;
   const auto channels = TrackList::Channels(this);
   BasicUI::SplitProgress(
      channels.begin(), channels.end(),
      [&](WaveTrack* pChannel, ProgressReporter child) {
         pChannel->ApplyStretchRatioOne(startTime, endTime, child);
      },
      reportProgress);
}

void WaveTrack::ApplyStretchRatioOne(
   double t0, double t1, const ProgressReporter& reportProgress)
{
   if (auto clipAtT0 = GetClipAtTime(t0); clipAtT0 &&
                                          clipAtT0->SplitsPlayRegion(t0) &&
                                          !clipAtT0->StretchRatioEquals(1))
      SplitAt(t0);
   if (auto clipAtT1 = GetClipAtTime(t1); clipAtT1 &&
                                          clipAtT1->SplitsPlayRegion(t1) &&
                                          !clipAtT1->StretchRatioEquals(1))
      SplitAt(t1);
   std::vector<WaveClip*> intersectedClips;
   auto clip = GetClipAtTime(t0);
   while (clip && clip->GetPlayStartTime() < t1)
   {
      intersectedClips.push_back(clip);
      clip = GetNextClip(*clip, PlaybackDirection::forward);
   }
   BasicUI::SplitProgress(
      intersectedClips.begin(), intersectedClips.end(),
      [&](WaveClip* clip, ProgressReporter reportClipProgress) {
         clip->ApplyStretchRatio(reportClipProgress);
      },
      reportProgress);
}

/*! @excsafety{Weak} */
void WaveTrack::Paste(double t0, const Track &src)
{
   assert(IsLeader()); // pre of Track::Paste
   if (const auto other = dynamic_cast<const WaveTrack*>(&src))
   {
      // Currently `Paste` isn't used by code that wants the newer "don't merge
      // when copy/pasting" behaviour ...
      constexpr auto merge = true;
      PasteWaveTrack(t0, *other, merge);
   }
   else
      // THROW_INCONSISTENCY_EXCEPTION; // ?
      (void)0;// Empty if intentional.
}

void WaveTrack::Silence(double t0, double t1, ProgressReporter reportProgress)
{
   assert(IsLeader());
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   ApplyStretchRatio({ { t0, t1 } }, std::move(reportProgress));

   auto start = TimeToLongSamples(t0);
   auto end = TimeToLongSamples(t1);

   for (const auto pChannel : TrackList::Channels(this)) {
      for (const auto &clip : pChannel->mClips) {
         auto clipStart = clip->GetPlayStartSample();
         auto clipEnd = clip->GetPlayEndSample();
         if (clipEnd > start && clipStart < end) {
            auto offset = std::max(start - clipStart, sampleCount(0));
            // Clip sample region and Get/Put sample region overlap
            auto length = std::min(end, clipEnd) - (clipStart + offset);
            clip->SetSilence(offset, length);
         }
      }
   }
}

/*! @excsafety{Strong} */
void WaveTrack::InsertSilence(double t, double len)
{
   assert(IsLeader());
   // Nothing to do, if length is zero.
   // Fixes Bug 1626
   if (len == 0)
      return;
   if (len <= 0)
      THROW_INCONSISTENCY_EXCEPTION;

   for (const auto pChannel : TrackList::Channels(this)) {
      auto &clips = pChannel->mClips;
      if (clips.empty()) {
         // Special case if there is no clip yet
         // TODO wide wave tracks -- match clip width
         auto clip = std::make_shared<WaveClip>(1,
            mpFactory, GetSampleFormat(), GetRate(), this->GetWaveColorIndex());
         clip->InsertSilence(0, len);
         // use No-fail-guarantee
         pChannel->InsertClip(move(clip));
      }
      else
      {
         // Assume at most one clip contains t
         const auto end = clips.end();
         const auto it = std::find_if(clips.begin(), end,
            [&](const WaveClipHolder &clip) { return clip->SplitsPlayRegion(t); } );

         // use Strong-guarantee
         if (it != end)
            it->get()->InsertSilence(t, len);

         // use No-fail-guarantee
         for (const auto &clip : clips)
            if (clip->BeforePlayRegion(t))
               clip->ShiftBy(len);
      }
   }
}

//Performs the opposite of Join
//Analyses selected region for possible Joined clips and disjoins them
/*! @excsafety{Weak} */
void WaveTrack::Disjoin(double t0, double t1)
{
   assert(IsLeader());
   auto minSamples = TimeToLongSamples(WAVETRACK_MERGE_POINT_TOLERANCE);
   const size_t maxAtOnce = 1048576;
   std::vector<float> buffer;
   std::vector<samplePtr> buffers;
   Regions regions;

   const size_t width = NChannels();

   for (const auto &interval : Intervals()) {
      double startTime = interval->Start();
      double endTime = interval->End();

      if (endTime < t0 || startTime > t1)
         continue;

      // Assume all clips will have the same width
      if (buffer.empty()) {
         buffer.resize(maxAtOnce * width);
         buffers.resize(width);
         auto pBuffer = buffer.data();
         for (size_t ii = 0; ii < width; ++ii, pBuffer += maxAtOnce)
            buffers[ii] = reinterpret_cast<samplePtr>(pBuffer);
      }

      const auto allZeroesAt = [&](size_t i) {
         auto pData = buffer.data() + i;
         for (size_t ii = 0; ii < width; ++ii, pData += maxAtOnce) {
            if (*pData != 0.0)
               return false;
         }
         return true;
      };

      // simply look for a sequence of zeroes (across all channels) and if the
      // sequence is longer than the minimum number, split-delete the region

      sampleCount seqStart = -1;
      auto start = interval->TimeToSamples(std::max(.0, t0 - startTime));
      auto end = interval->TimeToSamples(std::min(endTime, t1) - startTime);

      auto len = (end - start);
      for (decltype(len) done = 0; done < len; done += maxAtOnce) {
         auto numSamples = limitSampleBufferSize(maxAtOnce, len - done);

         auto bufferIt = buffers.begin();

         for (auto channel : interval->Channels())
            channel->GetSamples(
               *bufferIt++, floatSample, start + done, numSamples);

         for (decltype(numSamples) i = 0; i < numSamples; ++i) {
            auto curSamplePos = start + done + i;

            //start a NEW sequence
            if (seqStart == -1 && allZeroesAt(i))
               seqStart = curSamplePos;
            else if (curSamplePos == end - 1 || !allZeroesAt(i)) {
               if (seqStart != -1) {
                  decltype(end) seqEnd;

                  //consider the end case, where selection ends in zeroes
                  if (curSamplePos == end - 1 && allZeroesAt(i))
                     seqEnd = end;
                  else
                     seqEnd = curSamplePos;
                  if (seqEnd - seqStart + 1 > minSamples) {
                     regions.push_back(
                        Region(
                           startTime + interval->SamplesToTime(seqStart),
                           startTime + interval->SamplesToTime(seqEnd)
                        )
                     );
                  }
                  seqStart = -1;
               }
            }
         } // samples
      } // blocks
   } // finding regions

   for (const auto &region : regions)
      SplitDelete(region.start, region.end);
}

/*! @excsafety{Weak} */
void WaveTrack::Join(
   double t0, double t1, const ProgressReporter& reportProgress)
{
   assert(IsLeader());
   // Merge all WaveClips overlapping selection into one
   const auto intervals = Intervals();
   std::vector<std::shared_ptr<Interval>> intervalsToJoin;
   for (auto interval : intervals)
      if (interval->IntersectsPlayRegion(t0, t1))
         // TODO after this loop's iteration executes, the object referred to by
         // `interval` would get destroyed if `intervalsToJoin` were a vector of
         // raw pointers - why is that ?
         intervalsToJoin.push_back(interval);
   if (intervalsToJoin.size() < 2u)
      return;
   if (std::any_of(
          intervalsToJoin.begin() + 1, intervalsToJoin.end(),
          [first =
              intervalsToJoin[0]->GetStretchRatio()](const auto& interval) {
             return first != interval->GetStretchRatio();
          }))
      BasicUI::SplitProgress(
         intervalsToJoin.begin(), intervalsToJoin.end(),
         [&](
            const std::shared_ptr<Interval>& interval, ProgressReporter child) {
            interval->ApplyStretchRatio(child);
         },
         reportProgress);

   for (const auto pChannel : TrackList::Channels(this))
      JoinOne(*pChannel, t0, t1);
}

void WaveTrack::JoinOne(
   WaveTrack& track, double t0, double t1)
{
   WaveClipPointers clipsToDelete;
   WaveClip* newClip{};

   const auto rate = track.GetRate();
   auto &clips = track.mClips;
   for (const auto &clip: clips) {
      if (clip->IntersectsPlayRegion(t0, t1)) {
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
   if (clipsToDelete.empty())
      return;

   auto t = clipsToDelete[0]->GetPlayStartTime();
   //preserve left trim data if any
   newClip = track.CreateClip(clipsToDelete[0]->GetSequenceStartTime(),
      clipsToDelete[0]->GetName());

   for (auto clip : clipsToDelete) {
      // wxPrintf("t=%.6f adding clip (offset %.6f, %.6f ... %.6f)\n",
      //       t, clip->GetOffset(), clip->GetStartTime(),
      //       clip->GetEndTime());

      if (clip->GetPlayStartTime() - t > (1.0 / rate))
      {
         double addedSilence = (clip->GetPlayStartTime() - t);
         // wxPrintf("Adding %.6f seconds of silence\n");
         auto offset = clip->GetPlayStartTime();
         auto value = clip->GetEnvelope()->GetValue(offset);
         newClip->AppendSilence(addedSilence, value);
         t += addedSilence;
      }

      // wxPrintf("Pasting at %.6f\n", t);
      bool success = newClip->Paste(t, *clip);
      assert(success); // promise of CreateClip

      t = newClip->GetPlayEndTime();

      auto it = FindClip(clips, clip);
      clips.erase(it); // deletes the clip
   }
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveChannel::AppendBuffer(constSamplePtr buffer, sampleFormat format,
   size_t len, unsigned stride, sampleFormat effectiveFormat)
{
   return GetTrack().Append(buffer, format, len, stride, effectiveFormat);
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveChannel::Append(constSamplePtr buffer, sampleFormat format,
   size_t len)
{
   return GetTrack().Append(buffer, format, len, 1, widestSampleFormat);
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveTrack::Append(constSamplePtr buffer, sampleFormat format,
   size_t len, unsigned int stride, sampleFormat effectiveFormat,
   size_t iChannel)
{
   // TODO wide wave tracks -- there will be only one clip, and its `Append`
   // (or an overload) must take iChannel
   auto pTrack = this;
   if (GetOwner() && iChannel == 1)
      pTrack = *TrackList::Channels(this).rbegin();
   constSamplePtr buffers[]{ buffer };
   return pTrack->RightmostOrNewClip()
      ->Append(buffers, format, len, stride, effectiveFormat);
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
         // ignore extra channels (this function will soon be removed)
         bestBlockSize = clip->GetSequence(0)
            ->GetBestBlockSize(s - clip->GetSequenceStartSample());
         break;
      }
   }

   return bestBlockSize;
}

size_t WaveTrack::GetMaxBlockSize() const
{
   decltype(GetMaxBlockSize()) maxblocksize = 0;
   for (const auto &clip : mClips)
      for (size_t ii = 0, width = clip->GetWidth(); ii < width; ++ii)
         maxblocksize = std::max(maxblocksize,
            clip->GetSequence(ii)->GetMaxBlockSize());

   if (maxblocksize == 0)
   {
      // We really need the maximum block size, so create a
      // temporary sequence to get it.
      maxblocksize =
         Sequence{ mpFactory, SampleFormats{GetSampleFormat(), GetSampleFormat()} }
            .GetMaxBlockSize();
   }

   wxASSERT(maxblocksize > 0);

   return maxblocksize;
}

size_t WaveTrack::GetIdealBlockSize()
{
   // ignore extra channels (this function will soon be removed)
   return NewestOrNewClip()->GetSequence(0)->GetIdealBlockSize();
}

// TODO restore a proper exception safety guarantee; comment below is false
// because failure might happen after only one channel is done
/*! @excsafety{Mixed} */
/*! @excsafety{No-fail} -- The rightmost clip will be in a flushed state. */
/*! @excsafety{Partial}
-- Some initial portion (maybe none) of the append buffer of the rightmost
clip gets appended; no previously saved contents are lost. */
void WaveTrack::Flush()
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this))
      pChannel->FlushOne();
}

void WaveTrack::SetLegacyFormat(sampleFormat format)
{
   mLegacyFormat = format;
}

void WaveTrack::CopyClipEnvelopes()
{
   const auto channels = TrackList::Channels(this);
   if (channels.size() != 2)
      return;
   // Assume correspondence of clips
   const auto left = *channels.begin();
   auto it = begin(left->mClips),
      last = end(left->mClips);
   const auto right = *channels.rbegin();
   auto it2 = begin(right->mClips),
      last2 = end(right->mClips);
   for (; it != last; ++it, ++it2) {
      if (it2 == last2) {
         assert(false);
         break;
      }
      (*it2)->SetEnvelope(std::make_unique<Envelope>(*(*it)->GetEnvelope()));
   }
}

/*! @excsafety{Mixed} */
/*! @excsafety{No-fail} -- The rightmost clip will be in a flushed state. */
/*! @excsafety{Partial}
-- Some initial portion (maybe none) of the append buffer of the rightmost
clip gets appended; no previously saved contents are lost. */
void WaveTrack::FlushOne()
{
   // After appending, presumably.  Do this to the clip that gets appended.
   RightmostOrNewClip()->Flush();
}

bool WaveTrack::IsLeader() const
{
   return Track::IsLeader();
}

const ChannelGroup *WaveTrack::FindChannelGroup() const
{
   return this;
}

bool WaveTrack::GetMute() const
{
   return PlayableTrack::GetMute();
}

bool WaveTrack::GetSolo() const
{
   return PlayableTrack::GetSolo();
}

bool WaveTrack::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
{
   if (tag == "wavetrack") {
      double dblValue;
      long nValue;

      for (const auto& pair : attrs)
      {
         const auto& attr = pair.first;
         const auto& value = pair.second;

         if (attr == "rate")
         {
            // mRate is an int, but "rate" in the project file is a float.
            if (!value.TryGet(dblValue) ||
                  (dblValue < 1.0) || (dblValue > 1000000.0)) // allow a large range to be read
               return false;

            // Defer the setting of rate until LinkConsistencyFix
            mLegacyRate = lrint(dblValue);
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
         else if (attr == "linked" && value.TryGet(nValue))
            SetLinkType(ToLinkType(nValue), false);
         else if (attr == "colorindex" && value.TryGet(nValue))
            // Don't use SetWaveColorIndex as it sets the clips too.
            WaveTrackData::Get(*this).SetWaveColorIndex(nValue);
         else if (attr == "sampleformat" && value.TryGet(nValue) &&
                  Sequence::IsValidSampleFormat(nValue))
         {
            //Remember sample format until consistency check is performed.
            SetLegacyFormat(static_cast<sampleFormat>(nValue));
         }
      } // while
      return true;
   }

   return false;
}

void WaveTrack::HandleXMLEndTag(const std::string_view&  WXUNUSED(tag))
{
#if 0
   // In case we opened a pre-multiclip project, we need to
   // simulate closing the waveclip tag.
   NewestOrNewClip()->HandleXMLEndTag("waveclip");
#else
   // File compatibility breaks have intervened long since, and the line above
   // would now have undesirable side effects
#endif
   // Check for zero-length clips and remove them
   for (auto it = mClips.begin(); it != mClips.end();)
      if ((*it)->IsEmpty())
         it = mClips.erase(it);
      else
         ++it;
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
         return NewestOrNewClip()->GetSequence(0);
      else if (tag == "envelope")
         return NewestOrNewClip()->GetEnvelope();
   }

   // JKC... for 1.1.0, one step better than what we had, but still badly broken.
   // If we see a waveblock at this level, we'd better generate a sequence.
   if (tag == "waveblock")
   {
      // This is a legacy project, so set the cached offset
      NewestOrNewClip()->SetSequenceStartTime(mLegacyProjectFileOffset);
      Sequence *pSeq = NewestOrNewClip()->GetSequence(0);
      return pSeq;
   }

   //
   // This is for the NEW file format (post-1.2)
   //
   if (tag == "waveclip")
   {
      // Make clips (which don't serialize the rate) consistent with channel rate,
      // though the consistency check of channels with each other remains to do.
      // Not all `WaveTrackData` fields are properly initialized by now,
      // use deserialization helpers.
      auto clip = std::make_shared<WaveClip>(1,
         mpFactory, mLegacyFormat, mLegacyRate, GetWaveColorIndex());
      const auto xmlHandler = clip.get();
      mClips.push_back(std::move(clip));
      return xmlHandler;
   }

   return nullptr;
}

void WaveTrack::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   assert(IsLeader());
   const auto channels = TrackList::Channels(this);
   size_t iChannel = 0,
      nChannels = channels.size();
   for (const auto pChannel : channels)
      WriteOneXML(*pChannel, xmlFile, iChannel++, nChannels);
}

void WaveTrack::WriteOneXML(const WaveTrack &track, XMLWriter &xmlFile,
   size_t iChannel, size_t nChannels)
// may throw
{
   xmlFile.StartTag(wxT("wavetrack"));
   track.Track::WriteCommonXMLAttributes(xmlFile);

   // Write the "channel" attribute so earlier versions can interpret stereo
   // tracks, but this version doesn't read it
   {
      enum ChannelType {
         LeftChannel = 0,
         RightChannel = 1,
         MonoChannel = 2
      };
      const auto channelType = (nChannels == 0)
         ? MonoChannel
         : (iChannel == 0)
            ? LeftChannel
            : RightChannel;
      xmlFile.WriteAttr(wxT("channel"), channelType);
   }

   xmlFile.WriteAttr(wxT("linked"), static_cast<int>(track.GetLinkType()));
   track.WritableSampleTrack::WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("rate"), track.GetRate());

   // Some values don't vary independently in channels but have been written
   // redundantly for each channel.  Keep doing this in 3.4 and later in case
   // a project is opened in an earlier version.
   xmlFile.WriteAttr(wxT("gain"), static_cast<double>(track.GetGain()));
   xmlFile.WriteAttr(wxT("pan"), static_cast<double>(track.GetPan()));
   xmlFile.WriteAttr(wxT("colorindex"), track.GetWaveColorIndex());

   xmlFile.WriteAttr(wxT("sampleformat"), static_cast<long>(track.GetSampleFormat()));

   WaveTrackIORegistry::Get().CallWriters(track, xmlFile);

   for (const auto &clip : track.mClips)
      clip->WriteXML(xmlFile);

   xmlFile.EndTag(wxT("wavetrack"));
}

std::optional<TranslatableString> WaveTrack::GetErrorOpening() const
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this))
      for (const auto &clip : pChannel->mClips)
         for (size_t ii = 0, width = clip->GetWidth(); ii < width; ++ii)
            if (clip->GetSequence(ii)->GetErrorOpening())
               return XO("A track has a corrupted sample sequence.");

   return {};
}

bool WaveTrack::CloseLock() noexcept
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this))
      for (const auto &clip : pChannel->mClips)
         clip->CloseLock();

   return true;
}

const WaveClip* WaveTrack::GetLeftmostClip() const {
   if (mClips.empty())
      return nullptr;
   return std::min_element(
             mClips.begin(), mClips.end(),
             [](const auto& a, const auto b) {
                return a->GetPlayStartTime() < b->GetPlayStartTime();
             })
      ->get();
}

const WaveClip* WaveTrack::GetRightmostClip() const {
   if (mClips.empty())
      return nullptr;
   return std::max_element(
             mClips.begin(), mClips.end(),
             [](const auto& a, const auto b) {
                return a->GetPlayEndTime() < b->GetPlayEndTime();
             })
      ->get();
}

ClipConstHolders WaveTrack::GetClipInterfaces() const
{
  // We're constructing possibly wide clips here, and for this we need to have
  // access to the other channel-tracks.
  assert(IsLeader());
  const auto pOwner = GetOwner();
  ClipConstHolders wideClips;
  wideClips.reserve(mClips.size());
  for (auto clipIndex = 0u; clipIndex < mClips.size(); ++clipIndex)
  {
     const auto leftClip = mClips[clipIndex];
     WaveClipHolder rightClip;
     if (NChannels() == 2u && pOwner)
     {
        const auto& rightClips =
           (*TrackList::Channels(this).rbegin())->mClips;
        // This is known to have potential for failure for stereo tracks with
        // misaligned left/right clips - see
        // https://github.com/audacity/audacity/issues/4791.
        // If what you are trying to do is something else and this fails,
        // please report.
        assert(clipIndex < rightClips.size());
        if (clipIndex < rightClips.size())
           rightClip = rightClips[clipIndex];
     }
     wideClips.emplace_back(
        std::make_shared<WideClip>(leftClip, std::move(rightClip)));
  }
   return wideClips;
}

double WaveTrack::GetStartTime() const
{
   return ChannelGroup::GetStartTime();
}

double WaveTrack::GetEndTime() const
{
   return ChannelGroup::GetEndTime();
}

//
// Getting/setting samples.  The sample counts here are
// expressed relative to t=0.0 at the track's sample rate.
//

std::pair<float, float> WaveChannel::GetMinMax(
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

   for (const auto &clip: GetTrack().mClips)
   {
      if (t1 >= clip->GetPlayStartTime() && t0 <= clip->GetPlayEndTime())
      {
         clipFound = true;
         // TODO wide wave tracks -- choose correct channel
         auto clipResults = clip->GetMinMax(0, t0, t1, mayThrow);
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

float WaveChannel::GetRMS(double t0, double t1, bool mayThrow) const
{
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return 0.f;
   }

   if (t0 == t1)
      return 0.f;

   double sumsq = 0.0;
   double duration = 0;

   for (const auto &clip: GetTrack().mClips)
   {
      // If t1 == clip->GetStartTime() or t0 == clip->GetEndTime(), then the clip
      // is not inside the selection, so we don't want it.
      // if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      if (t1 >= clip->GetPlayStartTime() && t0 <= clip->GetPlayEndTime())
      {
         const auto clipStart = std::max(t0, clip->GetPlayStartTime());
         const auto clipEnd = std::min(t1, clip->GetPlayEndTime());

         // TODO wide wave tracks -- choose correct channel
         float cliprms = clip->GetRMS(0, t0, t1, mayThrow);

         sumsq += cliprms * cliprms * (clipEnd - clipStart);
         duration += (clipEnd - clipStart);
      }
   }
   return duration > 0 ? sqrt(sumsq / duration) : 0.0;
}

bool WaveTrack::DoGet(size_t iChannel, size_t nBuffers,
   const samplePtr buffers[], sampleFormat format,
   sampleCount start, size_t len, bool backwards, fillFormat fill,
   bool mayThrow, sampleCount* pNumWithinClips) const
{
   const auto nChannels = NChannels();
   assert(iChannel + nBuffers <= nChannels); // precondition
   const auto pOwner = GetOwner();
   if (!pOwner) {
      //! an un-owned track should have reported one channel only
      assert(nChannels == 1);
      nBuffers = std::min<size_t>(nBuffers, 1);
   }
   std::optional<TrackIter<const WaveTrack>> iter;
   auto pTrack = this;
   if (pOwner) {
      const auto ppLeader = TrackList::Channels(this).first;
      iter.emplace(ppLeader.advance(IsLeader() ? iChannel : 1));
      pTrack = **iter;
   }
   return std::all_of(buffers, buffers + nBuffers, [&](samplePtr buffer) {
      const auto result = pTrack->GetOne(
         buffer, format, start, len, backwards, fill, mayThrow,
         pNumWithinClips);
      if (iter)
         pTrack = *(++ *iter);
      return result;
   });
}

namespace {
void RoundToNearestClipSample(const WaveTrack& track, double& t)
{
   const auto clip = track.GetClipAtTime(t);
   if (!clip)
      return;
   t = clip->SamplesToTime(clip->TimeToSamples(t - clip->GetPlayStartTime())) +
       clip->GetPlayStartTime();
}
}

std::pair<size_t, size_t> WaveTrack::GetFloatsCenteredAroundTime(
   double t, size_t iChannel, float* buffer, size_t numSideSamples,
   bool mayThrow) const
{
   const auto numSamplesReadLeft = GetFloatsFromTime(
      t, iChannel, buffer, numSideSamples, mayThrow, PlaybackDirection::backward);
   const auto numSamplesReadRight = GetFloatsFromTime(
      t, iChannel, buffer + numSideSamples, numSideSamples + 1, mayThrow,
      PlaybackDirection::forward);
   return { numSideSamples - numSamplesReadLeft,
            numSideSamples + numSamplesReadRight };
}

namespace
{
template <typename FloatType>
using BufferCharType = std::conditional_t<
   std::is_const_v<std::remove_pointer_t<FloatType>>, constSamplePtr,
   samplePtr>;

template <typename BufferType> struct SampleAccessArgs
{
   const BufferCharType<BufferType> offsetBuffer;
   const sampleCount start;
   const size_t len;
};

template <typename BufferType>
SampleAccessArgs<BufferType> GetSampleAccessArgs(
   const WaveClip& clip, double startOrEndTime /*absolute*/, BufferType buffer,
   size_t totalToRead, size_t alreadyRead, bool forward)
{
   assert(totalToRead >= alreadyRead);
   const auto remainingToRead = totalToRead - alreadyRead;
   const auto sampsInClip = clip.GetVisibleSampleCount();
   const auto sampsPerSec = clip.GetRate() / clip.GetStretchRatio();
   if (forward)
   {
      const auto startTime =
         std::max(startOrEndTime - clip.GetPlayStartTime(), 0.);
      const sampleCount startSamp { std::round(startTime * sampsPerSec) };
      if (startSamp >= sampsInClip)
         return { nullptr, sampleCount { 0u }, 0u };
      const auto len =
         limitSampleBufferSize(remainingToRead, sampsInClip - startSamp);
      return { reinterpret_cast<BufferCharType<BufferType>>(
                  buffer + alreadyRead),
               startSamp, len };
   }
   else
   {
      const auto endTime = std::min(
         startOrEndTime - clip.GetPlayStartTime(), clip.GetPlayDuration());
      const sampleCount endSamp { std::round(endTime * sampsPerSec) };
      const auto startSamp =
         std::max(endSamp - remainingToRead, sampleCount { 0 });
      // `len` cannot be greater than `remainingToRead`, itself a `size_t` ->
      // safe cast.
      const auto len = (endSamp - startSamp).as_size_t();
      if (len == 0 || startSamp >= sampsInClip)
         return { nullptr, sampleCount { 0u }, 0u };
      const auto bufferEnd = buffer + remainingToRead;
      return { reinterpret_cast<BufferCharType<BufferType>>(bufferEnd - len),
               startSamp, len };
   }
}
} // namespace

size_t WaveTrack::GetFloatsFromTime(
   double t, size_t iChannel, float* buffer, size_t numSamples, bool mayThrow,
   PlaybackDirection direction) const
{
   RoundToNearestClipSample(*this, t);
   auto clip = GetClipAtTime(t);
   auto numSamplesRead = 0u;
   const auto forward = direction == PlaybackDirection::forward;
   while (clip)
   {
      const auto args = GetSampleAccessArgs(
         *clip, t, buffer, numSamples, numSamplesRead, forward);
      if (!clip->GetSamples(
             iChannel, args.offsetBuffer, floatSample, args.start, args.len,
             mayThrow))
         return 0u;
      numSamplesRead += args.len;
      if (numSamplesRead >= numSamples)
         break;
      clip = GetAdjacentClip(*clip, direction);
   }
   return numSamplesRead;
}

bool WaveTrack::GetFloatAtTime(
   double t, size_t iChannel, float& value, bool mayThrow) const
{
   const auto clip = GetClipAtTime(t);
   if (!clip)
      return false;
   clip->GetFloatAtTime(
      t - clip->GetPlayStartTime(), iChannel, value, mayThrow);
   return true;
}

void WaveTrack::SetFloatsCenteredAroundTime(
   double t, size_t iChannel, const float* buffer, size_t numSideSamples,
   sampleFormat effectiveFormat)
{
   SetFloatsFromTime(
      t, iChannel, buffer, numSideSamples, effectiveFormat,
      PlaybackDirection::backward);
   SetFloatsFromTime(
      t, iChannel, buffer + numSideSamples, numSideSamples + 1, effectiveFormat,
      PlaybackDirection::forward);
}

void WaveTrack::SetFloatsFromTime(
   double t, size_t iChannel, const float* buffer, size_t numSamples,
   sampleFormat effectiveFormat, PlaybackDirection direction)
{
   RoundToNearestClipSample(*this, t);
   auto clip = GetClipAtTime(t);
   auto numSamplesWritten = 0u;
   const auto forward = direction == PlaybackDirection::forward;
   while (clip)
   {
      const auto args = GetSampleAccessArgs(
         *clip, t, buffer, numSamples, numSamplesWritten, forward);
      if (args.len > 0u)
      {
         clip->SetSamples(
            iChannel, args.offsetBuffer, floatSample, args.start, args.len,
            effectiveFormat);
         numSamplesWritten += args.len;
         if (numSamplesWritten >= numSamples)
            break;
      }
      clip = GetAdjacentClip(*clip, direction);
   }
}

void WaveTrack::SetFloatAtTime(
   double t, size_t iChannel, float value, sampleFormat effectiveFormat)
{
   SetFloatsCenteredAroundTime(t, iChannel, &value, 0u, effectiveFormat);
}

void WaveTrack::SetFloatsWithinTimeRange(
   double t0, double t1, size_t iChannel,
   const std::function<float(double sampleTime)>& producer,
   sampleFormat effectiveFormat)
{
   if (t0 >= t1)
      return;
   const auto sortedClips = SortedClipArray();
   if (sortedClips.empty())
      return;
   t0 = std::max(t0, (*sortedClips.begin())->GetPlayStartTime());
   t1 = std::min(t1, (*sortedClips.rbegin())->GetPlayEndTime());
   auto clip = GetClipAtTime(t0);
   while (clip) {
      const auto clipStartTime = clip->GetPlayStartTime();
      const auto clipEndTime = clip->GetPlayEndTime();
      const auto sampsPerSec = clip->GetRate() / clip->GetStretchRatio();
      const auto roundedT0 =
         std::round((t0 - clipStartTime) * sampsPerSec) / sampsPerSec +
         clipStartTime;
      const auto roundedT1 =
         std::round((t1 - clipStartTime) * sampsPerSec) / sampsPerSec +
         clipStartTime;
      if (clipStartTime > roundedT1)
         break;
      const auto tt0 = std::max(clipStartTime, roundedT0);
      const auto tt1 = std::min(clipEndTime, roundedT1);
      const size_t numSamples = (tt1 - tt0) * sampsPerSec + .5;
      std::vector<float> values(numSamples);
      for (auto i = 0u; i < numSamples; ++i)
         values[i] = producer(tt0 + clip->SamplesToTime(i));
      clip->SetFloatsFromTime(
         tt0 - clipStartTime, iChannel, values.data(), numSamples,
         effectiveFormat);
      clip = GetNextClip(*clip, PlaybackDirection::forward);
   }
}

bool WaveTrack::GetOne(
   samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
   bool backwards, fillFormat fill, bool mayThrow,
   sampleCount* pNumWithinClips) const
{
   if (backwards)
      start -= len;
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
      if (fill == FillFormat::fillZero)
         ClearSamples(buffer, format, 0, len);
      // but we don't have to.
      else if (fill == FillFormat::fillTwo)
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
         // Yes, exact comparison
         if (clip->GetStretchRatio() != 1.0)
            return false;

         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy =
            std::min( start+len - clipStart, clip->GetVisibleSampleCount() );
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

         if (!clip->GetSamples(0,
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
   if (result == true && backwards)
      ReverseSamples(buffer, format, 0, len);
   return result;
}

ChannelGroupSampleView
WaveTrack::GetSampleView(double t0, double t1, bool mayThrow) const
{
   assert(IsLeader());
   ChannelGroupSampleView result;
   for (const auto& channel : Channels()) {
      result.push_back(channel->GetSampleView(t0, t1, mayThrow));
   }
   return result;
}

ChannelSampleView
WaveChannel::GetSampleView(double t0, double t1, bool mayThrow) const
{
   std::vector<std::shared_ptr<const WaveChannelInterval>>
      intersectingIntervals;
   for (const auto& interval : Intervals())
      if (interval->Intersects(t0, t1))
         intersectingIntervals.push_back(interval);
   if (intersectingIntervals.empty())
      return { AudioSegmentSampleView {
         (TimeToLongSamples(t1) - TimeToLongSamples(t0)).as_size_t() } };
   std::sort(
      intersectingIntervals.begin(), intersectingIntervals.end(),
      [](const auto& a, const auto& b) { return a->Start() < b->Start(); });
   std::vector<AudioSegmentSampleView> segments;
   segments.reserve(2 * intersectingIntervals.size() + 1);
   for (auto i = 0u; i < intersectingIntervals.size();++i)
   {
      const auto& interval = intersectingIntervals[i];
      const auto intervalStartTime = interval->Start();
      if (t0 < intervalStartTime)
      {
         const auto numSamples = TimeToLongSamples(intervalStartTime - t0);
         segments.push_back(AudioSegmentSampleView{numSamples.as_size_t()});
         t0 = intervalStartTime;
      }
      const auto intervalT0 = t0 - intervalStartTime;
      const auto intervalT1 = std::min(t1, interval->End()) - intervalStartTime;
      if(intervalT1 > intervalT0)
      {
         auto newSegment =
            interval->GetSampleView(intervalT0, intervalT1, mayThrow);
         t0 += intervalT1 - intervalT0;
         segments.push_back(std::move(newSegment));
      }
      if (t0 == t1)
         break;
   }
   if (t0 < t1)
      segments.push_back(AudioSegmentSampleView {
         (TimeToLongSamples(t1) - TimeToLongSamples(t0)).as_size_t() });
   return segments;
}

/*! @excsafety{Weak} */
bool WaveChannel::Set(constSamplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, sampleFormat effectiveFormat)
{
   for (const auto &clip: GetTrack().mClips)
   {
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();

      if (clipEnd > start && clipStart < start+len)
      {
         // Test as also in WaveTrack::GetOne()
         if (clip->GetStretchRatio() != 1.0)
            return false;

         // Clip sample region and Get/Put sample region overlap
         auto samplesToCopy =
            std::min( start+len - clipStart, clip->GetVisibleSampleCount() );
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

         clip->SetSamples(0,
            buffer + startDelta.as_size_t() * SAMPLE_SIZE(format),
            format, inclipDelta, samplesToCopy.as_size_t(), effectiveFormat );
         clip->MarkChanged();
      }
   }
   return true;
}

sampleFormat WaveTrack::WidestEffectiveFormat() const
{
   auto result = narrowestSampleFormat;
   const auto accumulate = [&](const WaveTrack &track) {
      for (const auto &pClip : track.GetClips())
         for (size_t ii = 0, width = pClip->GetWidth(); ii < width; ++ii)
            result = std::max(result,
               pClip->GetSequence(ii)->GetSampleFormats().Effective());
   };
   if (auto pOwner = GetOwner()) {
      for (auto channel : TrackList::Channels(this))
         accumulate(*channel);
   }
   else
      accumulate(*this);
   return result;
}

bool WaveTrack::HasTrivialEnvelope() const
{
   auto pTrack = this;
   if (GetOwner())
      // Substitute the leader track
      pTrack = *TrackList::Channels(this).begin();
   auto &clips = pTrack->GetClips();
   return std::all_of(clips.begin(), clips.end(),
      [](const auto &pClip){ return pClip->GetEnvelope()->IsTrivial(); });
}

void WaveTrack::GetEnvelopeValues(
   double* buffer, size_t bufferLen, double t0, bool backwards) const
{
   auto pTrack = this;
   if (GetOwner())
      // Substitute the leader track
      pTrack = *TrackList::Channels(this).begin();

   if (backwards)
      t0 -= bufferLen / GetRate();
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
   const auto rate = GetRate();
   auto tstep = 1.0 / rate;
   double endTime = t0 + tstep * bufferLen;
   for (const auto &clip: pTrack->mClips)
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
            auto nDiff = (sampleCount)floor((dClipStartTime - rt0) * rate + 0.5);
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
   if (backwards)
      std::reverse(buffer, buffer + bufferLen);
}

const WaveClip* WaveTrack::GetAdjacentClip(
   const WaveClip& clip, PlaybackDirection direction) const
{
   const auto neighbour = GetNextClip(clip, direction);
   if (!neighbour)
      return nullptr;
   else if (direction == PlaybackDirection::forward)
      return std::abs(clip.GetPlayEndTime() - neighbour->GetPlayStartTime()) <
                   1e-9 ?
                neighbour :
                nullptr;
   else
      return std::abs(clip.GetPlayStartTime() - neighbour->GetPlayEndTime()) <
                   1e-9 ?
                neighbour :
                nullptr;
}

WaveClip*
WaveTrack::GetAdjacentClip(const WaveClip& clip, PlaybackDirection direction)
{
   return const_cast<WaveClip*>(
      std::as_const(*this).GetAdjacentClip(clip, direction));
}

// When the time is both the end of a clip and the start of the next clip, the
// latter clip is returned.
WaveClip* WaveTrack::GetClipAtTime(double time)
{
   return const_cast<WaveClip*>(std::as_const(*this).GetClipAtTime(time));
}

const WaveClip* WaveTrack::GetNextClip(
   const WaveClip& clip, PlaybackDirection direction) const
{
   const auto clips = SortedClipArray();
   const auto p = std::find(clips.begin(), clips.end(), &clip);
   if (p == clips.end())
      return nullptr;
   else if (direction == PlaybackDirection::forward)
      return p == clips.end() - 1 ? nullptr : *(p + 1);
   else
      return p == clips.begin() ? nullptr : *(p - 1);
}

WaveClipConstHolders WaveTrack::GetClipsIntersecting(double t0, double t1) const
{
   assert(t0 <= t1);
   WaveClipConstHolders intersectingClips;
   for (const auto& clip : mClips)
      if (clip->IntersectsPlayRegion(t0, t1))
         intersectingClips.push_back(clip);
   return intersectingClips;
}

WaveClip*
WaveTrack::GetNextClip(const WaveClip& clip, PlaybackDirection direction)
{
   return const_cast<WaveClip*>(
      std::as_const(*this).GetNextClip(clip, direction));
}

const WaveClip* WaveTrack::GetClipAtTime(double time) const
{
   const auto clips = SortedClipArray();
   auto p = std::find_if(
      clips.rbegin(), clips.rend(), [&](const WaveClip* const& clip) {
         return clip->WithinPlayRegion(time);
      });

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
   auto pTrack = this;
   if (GetOwner())
      // Substitute the leader track
      pTrack = *TrackList::Channels(this).begin();
   WaveClip* clip = pTrack->GetClipAtTime(time);
   if (clip)
      return clip->GetEnvelope();
   else
      return NULL;
}

void WaveTrack::CreateWideClip(double offset, const wxString& name)
{
   assert(IsLeader());
   for(auto channel : TrackList::Channels(this))
      channel->CreateClip(offset, name);
}

WaveClip* WaveTrack::CreateClip(double offset, const wxString& name)
{
   // TODO wide wave tracks -- choose clip width correctly for the track
   auto clip = std::make_shared<WaveClip>(1,
      mpFactory, GetSampleFormat(), GetRate(), GetWaveColorIndex());
   clip->SetName(name);
   clip->SetSequenceStartTime(offset);

   const auto& tempo = GetProjectTempo();
   if (tempo.has_value())
      clip->OnProjectTempoChange(std::nullopt, *tempo);
   mClips.push_back(std::move(clip));

   auto result = mClips.back().get();
   // TODO wide wave tracks -- for now assertion is correct because widths are
   // always 1
   assert(result->GetWidth() == GetWidth());
   return result;
}

WaveClip* WaveTrack::NewestOrNewClip()
{
   if (mClips.empty()) {
      return CreateClip(WaveTrackData::Get(*this).GetOrigin(), MakeNewClipName());
   }
   else
      return mClips.back().get();
}

/*! @excsafety{No-fail} */
WaveClip* WaveTrack::RightmostOrNewClip()
{
   if (mClips.empty()) {
      return CreateClip(WaveTrackData::Get(*this).GetOrigin(), MakeNewClipName());
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

int WaveTrack::GetNumClips(double t0, double t1) const
{
   const auto clips = SortedClipArray();
   // Find first position where the comparison is false
   const auto firstIn = std::lower_bound(clips.begin(), clips.end(), t0,
      [](const auto& clip, double t0) {
         return clip->GetPlayEndTime() <= t0;
      });
   // Find first position where the comparison is false
   const auto firstOut = std::lower_bound(firstIn, clips.end(), t1,
      [](const auto& clip, double t1) {
         return clip->GetPlayStartTime() < t1;
      });
   return std::distance(firstIn, firstOut);
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
   const WaveClip& candidateClip, double& slideBy, double tolerance) const
{
   if (mClips.empty())
      return true;
   // Find clip in this that overlaps most with `clip`:
   const auto candidateClipStartTime = candidateClip.GetPlayStartTime();
   const auto candidateClipEndTime = candidateClip.GetPlayEndTime();
   const auto t0 = SnapToSample(candidateClipStartTime + slideBy);
   const auto t1 = SnapToSample(candidateClipEndTime + slideBy);
   std::vector<double> overlaps;
   std::transform(
      mClips.begin(), mClips.end(), std::back_inserter(overlaps),
      [&](const auto& pClip) {
         return pClip->IntersectsPlayRegion(t0, t1) ?
                   std::min(pClip->GetPlayEndTime(), t1) -
                      std::max(pClip->GetPlayStartTime(), t0) :
                   0.0;
      });
   const auto maxOverlap = std::max_element(overlaps.begin(), overlaps.end());
   if (*maxOverlap > tolerance)
      return false;
   const auto overlappedClip =
      mClips[std::distance(overlaps.begin(), maxOverlap)];
   const auto requiredOffset =  slideBy +
             *maxOverlap * (overlappedClip->GetPlayStartTime() < t0 ? 1 : -1);
   // Brute-force check to see if there's another clip that'd be in the way.
   if (std::any_of(
          mClips.begin(), mClips.end(),
          [&](const auto& pClip)
          {
             const auto result = pClip->IntersectsPlayRegion(
                SnapToSample(candidateClipStartTime + requiredOffset),
                SnapToSample(candidateClipEndTime + requiredOffset));
             return result;
          }))
      return false;
   slideBy = requiredOffset;
   return true;
}

/*! @excsafety{Weak} */
void WaveTrack::Split(double t0, double t1)
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this)) {
      pChannel->SplitAt(t0);
      if (t0 != t1)
         pChannel->SplitAt(t1);
   }
}

/*! @excsafety{Weak} */
void WaveTrack::SplitAt(double t)
{
   for (const auto &c : mClips)
   {
      if (c->SplitsPlayRegion(t))
      {
         t = SnapToSample(t);
         auto newClip = std::make_shared<WaveClip>(*c, mpFactory, true);
         c->TrimRightTo(t);// put t on a sample
         newClip->TrimLeftTo(t);

         // This could invalidate the iterators for the loop!  But we return
         // at once so it's okay
         InsertClip(std::move(newClip)); // transfer ownership
         return;
      }
   }
}

// Expand cut line (that is, re-insert audio, then DELETE audio saved in cut line)
// Can't promise strong exception safety for a pair of tracks together
void WaveTrack::ExpandCutLine(double cutLinePosition, double* cutlineStart,
                              double* cutlineEnd)
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this)) {
      pChannel->ExpandOneCutLine(cutLinePosition, cutlineStart, cutlineEnd);
      // Assign the out parameters at most once
      cutlineStart = cutlineEnd = nullptr;
   }
}

/*! @excsafety{Strong} */
void WaveTrack::ExpandOneCutLine(double cutLinePosition,
   double* cutlineStart, double* cutlineEnd)
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
               clip2->ShiftBy(end - start);
         }
      }
   }
}

bool WaveTrack::RemoveCutLine(double cutLinePosition)
{
   assert(IsLeader());

   bool removed = false;
   for (const auto pChannel : TrackList::Channels(this))
      for (const auto &clip : pChannel->mClips)
         if (clip->RemoveCutLine(cutLinePosition)) {
            removed = true;
            break;
         }

   return removed;
}

// Can't promise strong exception safety for a pair of tracks together
bool WaveTrack::MergeClips(int clipidx1, int clipidx2)
{
   const auto channels = TrackList::Channels(this);
   return std::all_of(channels.begin(), channels.end(),
      [&](const auto pChannel){
         return pChannel->MergeOneClipPair(clipidx1, clipidx2); });
}

/*! @excsafety{Strong} */
bool WaveTrack::MergeOneClipPair(int clipidx1, int clipidx2)
{
   WaveClip* clip1 = GetClipByIndex(clipidx1);
   WaveClip* clip2 = GetClipByIndex(clipidx2);

   if (!clip1 || !clip2) // Could happen if one track of a linked pair had a split and the other didn't.
      return false; // Don't throw, just do nothing.

   const auto stretchRatiosEqual = clip1->HasEqualStretchRatio(*clip2);
   if (!stretchRatiosEqual)
      return false;

   // Append data from second clip to first clip
   // use Strong-guarantee
   bool success = clip1->Paste(clip1->GetPlayEndTime(), *clip2);
   assert(success);  // assuming clips of the same track must have same width

   // use No-fail-guarantee for the rest
   // Delete second clip
   auto it = FindClip(mClips, clip2);
   mClips.erase(it);

   return true;
}

/*! @excsafety{Weak} -- Partial completion may leave clips at differing sample rates!
*/
void WaveTrack::Resample(int rate, BasicUI::ProgressDialog *progress)
{
   for (const auto pChannel : TrackList::Channels(this)) {
      for (const auto &clip : pChannel->mClips)
         clip->Resample(rate, progress);
   }
   DoSetRate(rate);
}

bool WaveTrack::Reverse(sampleCount start, sampleCount len,
   const ProgressReport &progress)
{
   size_t count = 0;
   const auto range = TrackList::Channels(this);
   const auto myProgress = [&](double fraction){
      return progress((count + fraction) / range.size());
   };
   for (const auto pChannel : range) {
      if (!ReverseOne(*pChannel, start, len, myProgress))
         return false;
      ++count;
   }
   return true;
}

bool WaveTrack::ReverseOne(WaveTrack &track,
   sampleCount start, sampleCount len,
   const ProgressReport &progress)
{
   bool rValue = true; // return value

   // start, end, len refer to the selected reverse region
   auto end = start + len;

   // STEP 1:
   // If a reverse selection begins and/or ends at the inside of a clip
   // perform a split at the start and/or end of the reverse selection
   const auto &clips = track.GetClips();
   // Beware, the array grows as we loop over it.  Use integer subscripts, not
   // iterators.
   for (size_t ii = 0; ii < clips.size(); ++ii) {
      const auto &clip = clips[ii].get();
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();
      if (clipStart < start && clipEnd > start && clipEnd <= end) {
         // the reverse selection begins at the inside of a clip
         double splitTime = track.LongSamplesToTime(start);
         track.SplitAt(splitTime);
      }
      else if (clipStart >= start && clipStart < end && clipEnd > end) {
         // the reverse selection ends at the inside of a clip
         double splitTime = track.LongSamplesToTime(end);
         track.SplitAt(splitTime);
      }
      else if (clipStart < start && clipEnd > end) {
         // the selection begins AND ends at the inside of a clip
         double splitTime = track.LongSamplesToTime(start);
         track.SplitAt(splitTime);
         splitTime = track.LongSamplesToTime(end);
         track.SplitAt(splitTime);
      }
   }

   //STEP 2:
   // Individually reverse each clip inside the selected region
   // and apply the appropriate offset after detaching them from the track

   bool checkedFirstClip = false;

   // used in calculating the offset of clips to rearrange
   // holds the new end position of the current clip
   auto currentEnd = end;

   // holds the reversed clips
   WaveClipHolders revClips;
   // holds the clips that appear after the reverse selection region
   WaveClipHolders otherClips;
   auto clipArray = track.SortedClipArray();
   for (size_t i = 0; i < clipArray.size(); ++i) {
      WaveClip *clip = clipArray[i];
      auto clipStart = clip->GetPlayStartSample();
      auto clipEnd = clip->GetPlayEndSample();

      if (clipStart >= start && clipEnd <= end) {
         // if the clip is inside the selected region
         // this is used to check if the selected region begins with a
         // whitespace.  If yes then clipStart (of the first clip) and start are
         // not the same.  Adjust currentEnd accordingly and set endMerge to
         // false
         if (!checkedFirstClip && clipStart > start) {
            checkedFirstClip = true;
            if (i > 0) {
               if (clipArray[i - 1]->GetPlayEndSample() <= start)
                  currentEnd -= (clipStart - start);
            }
            else
               currentEnd -= (clipStart - start);
         }

         auto revStart = std::max(clipStart, start);
         auto revEnd = std::min(end, clipEnd);
         auto revLen = revEnd - revStart;
         if (revEnd >= revStart) {
            // reverse the clip
            if (!ReverseOneClip(track, revStart, revLen, start, end, progress))
            {
               rValue = false;
               break;
            }

            // calculate the offset required
            auto clipOffsetStart = currentEnd - (clipEnd - clipStart);
            double offsetStartTime = track.LongSamplesToTime(clipOffsetStart);
            if (i + 1 < clipArray.size()) {
               // update currentEnd if there is a clip to process next
               auto nextClipStart = clipArray[i + 1]->GetPlayStartSample();
               currentEnd = currentEnd -
                  (clipEnd - clipStart) - (nextClipStart - clipEnd);
            }

            // detach the clip from track
            revClips.push_back(track.RemoveAndReturnClip(clip));
            // align time to a sample and set offset
            revClips.back()->SetPlayStartTime(
               track.SnapToSample(offsetStartTime));
         }
      }
      else if (clipStart >= end) {
         // clip is after the selection region
         // simply remove and append to otherClips
         otherClips.push_back(track.RemoveAndReturnClip(clip));
      }
   }

   // STEP 3: Append the clips from
   // revClips and otherClips back to the track
   // the last clip of revClips is appended to the track first
   // PRL:  I don't think that matters, the sequence of storage of clips in the
   // track is not elsewhere assumed to be by time
   for (auto it = revClips.rbegin(), revEnd = revClips.rend();
        rValue && it != revEnd; ++it)
      rValue = track.AddClip(*it);

   if (!rValue)
      return false;

   for (auto &clip : otherClips)
      if (!(rValue = track.AddClip(clip)))
          break;

   return rValue;
}

bool WaveTrack::ReverseOneClip(WaveTrack &track,
   sampleCount start, sampleCount len,
   sampleCount originalStart, sampleCount originalEnd,
   const ProgressReport &report)
{
   bool rc = true;
   // keep track of two blocks whose data we will swap
   auto first = start;

   auto blockSize = track.GetMaxBlockSize();
   Floats buffer1{ blockSize };
   const auto pBuffer1 = buffer1.get();
   Floats buffer2{ blockSize };
   const auto pBuffer2 = buffer2.get();

   auto originalLen = originalEnd - originalStart;

   while (len > 1) {
      auto block =
         limitSampleBufferSize(track.GetBestBlockSize(first), len / 2);
      auto second = first + (len - block);

      track.GetFloats(buffer1.get(), first, block);
      std::reverse(pBuffer1, pBuffer1 + block);
      track.GetFloats(buffer2.get(), second, block);
      std::reverse(pBuffer2, pBuffer2 + block);
      // Don't dither on later rendering if only reversing samples
      const bool success =
         track.Set((samplePtr)buffer2.get(), floatSample, first, block,
            narrowestSampleFormat)
         &&
         track.Set((samplePtr)buffer1.get(), floatSample, second, block,
            narrowestSampleFormat);
      if (!success)
         return false;

      len -= 2 * block;
      first += block;

      if (!report(
         2 * (first - originalStart).as_double() / originalLen.as_double()
      )) {
         rc = false;
         break;
      }
   }

   return rc;
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

bool WaveTrack::HasHiddenData() const
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this))
      for (const auto& clip : pChannel->GetClips())
         if (clip->GetTrimLeft() != 0 || clip->GetTrimRight() != 0)
            return true;
   return false;
}

void WaveTrack::DiscardTrimmed()
{
   assert(IsLeader());
   for (const auto pChannel : TrackList::Channels(this)) {
      for (auto clip : pChannel->GetClips()) {
         if (clip->GetTrimLeft() != 0) {
            auto t0 = clip->GetPlayStartTime();
            clip->SetTrimLeft(0);
            clip->ClearLeft(t0);
         }
         if (clip->GetTrimRight() != 0) {
            auto t1 = clip->GetPlayEndTime();
            clip->SetTrimRight(0);
            clip->ClearRight(t1);
         }
      }
   }
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

void VisitBlocks(TrackList &tracks, BlockVisitor visitor,
   SampleBlockIDSet *pIDs)
{
   for (auto wt : tracks.Any<const WaveTrack>())
      for (const auto pChannel : TrackList::Channels(wt))
         // Scan all clips within current track
         for (const auto &clip : pChannel->GetAllClips())
            // Scan all sample blocks within current clip
            for (size_t ii = 0, width = clip->GetWidth(); ii < width; ++ii) {
               auto blocks = clip->GetSequenceBlockArray(ii);
               for (const auto &block : *blocks) {
                  auto &pBlock = block.sb;
                  if (pBlock) {
                     if (pIDs && !pIDs->insert(pBlock->GetBlockID()).second)
                        continue;
                     if (visitor)
                        visitor(*pBlock);
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

namespace {
// If any clips have hidden data, don't allow older versions to open the
// project.  Otherwise overlapping clips might result.
ProjectFormatExtensionsRegistry::Extension smartClipsExtension(
   [](const AudacityProject& project) -> ProjectFormatVersion {
      const TrackList& trackList = TrackList::Get(project);
      for (auto wt : trackList.Any<const WaveTrack>())
         for (const auto pChannel : TrackList::Channels(wt))
            for (const auto& clip : pChannel->GetAllClips())
               if (clip->GetTrimLeft() > 0.0 || clip->GetTrimRight() > 0.0)
                  return { 3, 1, 0, 0 };
      return BaseProjectFormatVersion;
   }
);

// If any clips have any stretch, don't allow older versions to open the
// project.  Otherwise overlapping clips might result.
ProjectFormatExtensionsRegistry::Extension stretchedClipsExtension(
   [](const AudacityProject& project) -> ProjectFormatVersion {
      const TrackList& trackList = TrackList::Get(project);
      for (auto wt : trackList.Any<const WaveTrack>())
         for (const auto pChannel : TrackList::Channels(wt))
            for (const auto& clip : pChannel->GetAllClips())
               if (clip->GetStretchRatio() != 1.0)
                  return { 3, 4, 0, 0 };
      return BaseProjectFormatVersion;
   }
);
}

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
