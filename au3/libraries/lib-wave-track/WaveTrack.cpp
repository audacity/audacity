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

#include <algorithm>
#include <float.h>
#include <math.h>
#include <numeric>
#include <optional>
#include <type_traits>
#include <unordered_set>

#include "float_cast.h"

#include "AudioSegmentSampleView.h"
#include "ChannelAttachments.h"
#include "ClipTimeAndPitchSource.h"
#include "Envelope.h"
#include "Sequence.h"
#include "StaffPadTimeAndPitch.h"

#include "TempoChange.h"
#include "Project.h"
#include "ProjectRate.h"
#include "SampleBlock.h"

#include "BasicUI.h"
#include "Prefs.h"
#include "QualitySettings.h"
#include "SyncLock.h"
#include "TimeWarper.h"

#include "InconsistencyException.h"

#include <cmath>

using std::max;

/*!
 * @post result: `result->GetStretchRatio() == 1`
 */
namespace {
WaveTrack::IntervalHolder GetRenderedCopy(
    const WaveTrack::IntervalHolder& pInterval,
    const std::function<void(double)>& reportProgress,
    const SampleBlockFactoryPtr& factory, sampleFormat format)
{
    auto& interval = *pInterval;
    using Interval = WaveTrack::Interval;
    if (!interval.HasPitchOrSpeed()) {
        return pInterval;
    }

    const auto dst = WaveClip::NewShared(interval.NChannels(), factory, format, interval.GetRate());

    const auto originalPlayStartTime = interval.GetPlayStartTime();
    const auto originalPlayEndTime = interval.GetPlayEndTime();
    const auto stretchRatio = interval.GetStretchRatio();

    auto success = false;
    Finally Do { [&] {
            if (!success) {
                interval.TrimLeftTo(originalPlayStartTime);
                interval.TrimRightTo(originalPlayEndTime);
            }
        } };

    // Leave 1 second of raw, unstretched audio before and after visible region
    // to give the algorithm a chance to be in a steady state when reaching the
    // play boundaries.
    const auto tmpPlayStartTime
        =std::max(interval.GetSequenceStartTime(), originalPlayStartTime - stretchRatio);
    const auto tmpPlayEndTime
        =std::min(interval.GetSequenceEndTime(), originalPlayEndTime + stretchRatio);
    interval.TrimLeftTo(tmpPlayStartTime);
    interval.TrimRightTo(tmpPlayEndTime);

    constexpr auto sourceDurationToDiscard = 0.;
    constexpr auto blockSize = 1024;
    const auto numChannels = interval.NChannels();
    ClipTimeAndPitchSource stretcherSource { interval, sourceDurationToDiscard,
                                             PlaybackDirection::forward };
    TimeAndPitchInterface::Parameters params;
    params.timeRatio = stretchRatio;
    params.pitchRatio = std::pow(2., interval.GetCentShift() / 1200.);
    params.preserveFormants
        =interval.GetPitchAndSpeedPreset() == PitchAndSpeedPreset::OptimizeForVoice;
    StaffPadTimeAndPitch stretcher { interval.GetRate(), numChannels,
                                     stretcherSource, std::move(params) };

    // Post-rendering sample counts, i.e., stretched units
    const auto totalNumOutSamples
        =sampleCount { interval.GetVisibleSampleCount().as_double()
                       * stretchRatio };

    sampleCount numOutSamples { 0 };
    AudioContainer container(blockSize, numChannels);

    while (numOutSamples < totalNumOutSamples)
    {
        const auto numSamplesToGet
            =limitSampleBufferSize(blockSize, totalNumOutSamples - numOutSamples);
        stretcher.GetSamples(container.Get(), numSamplesToGet);
        constSamplePtr data[2];
        data[0] = reinterpret_cast<constSamplePtr>(container.Get()[0]);
        if (interval.NChannels() == 2) {
            data[1] = reinterpret_cast<constSamplePtr>(container.Get()[1]);
        }
        dst->Append(data, floatSample, numSamplesToGet, 1, widestSampleFormat);
        numOutSamples += numSamplesToGet;
        if (reportProgress) {
            reportProgress(
                numOutSamples.as_double() / totalNumOutSamples.as_double());
        }
    }
    dst->Flush();

    // Now we're all like `this` except unstretched. We can clear leading and
    // trailing, stretching transient parts.
    dst->SetPlayStartTime(tmpPlayStartTime);
    dst->ClearLeft(originalPlayStartTime);
    dst->ClearRight(originalPlayEndTime);

    // We don't preserve cutlines but the relevant part of the envelope.
    auto dstEnvelope = std::make_unique<Envelope>(interval.GetEnvelope());
    const auto samplePeriod = 1. / interval.GetRate();
    dstEnvelope->CollapseRegion(
        originalPlayEndTime, interval.GetSequenceEndTime() + samplePeriod, samplePeriod);
    dstEnvelope->CollapseRegion(0, originalPlayStartTime, samplePeriod);
    dstEnvelope->SetOffset(originalPlayStartTime);
    dst->SetEnvelope(move(dstEnvelope));

    success = true;

    assert(!dst->HasPitchOrSpeed());
    return dst;
}
}

std::shared_ptr<const WaveTrack::Interval>
WaveTrack::GetNextInterval(const Interval& interval, PlaybackDirection searchDirection) const
{
    std::shared_ptr<const Interval> result;
    auto bestMatchTime = searchDirection == PlaybackDirection::forward
                         ? std::numeric_limits<double>::max()
                         : std::numeric_limits<double>::lowest();

    for (const auto& other : Intervals()) {
        if ((searchDirection == PlaybackDirection::forward
             && (other->Start() > interval.Start() && other->Start() < bestMatchTime))
            ||
            (searchDirection == PlaybackDirection::backward
             && (other->Start() < interval.Start() && other->Start() > bestMatchTime))) {
            result = other;
            bestMatchTime = other->Start();
        }
    }
    return result;
}

WaveTrack::IntervalHolder WaveTrack::GetNextInterval(
    const Interval& interval, PlaybackDirection searchDirection)
{
    return std::const_pointer_cast<Interval>(
        std::as_const(*this).GetNextInterval(interval, searchDirection));
}

WaveTrack::IntervalHolder WaveTrack::GetIntervalAtTime(double t)
{
    IntervalHolder result;
    for (const auto& interval : Intervals()) {
        if (interval->WithinPlayRegion(t)) {
            return interval;
        }
    }
    return nullptr;
}

namespace {
struct WaveTrackData : ClientData::Cloneable<> {
    WaveTrackData() = default;
    WaveTrackData(const WaveTrackData&);
    WaveTrackData& operator=(const WaveTrackData&) = delete;
    ~WaveTrackData() override;
    std::unique_ptr<ClientData::Cloneable<> > Clone() const override;

    static WaveTrackData& Get(WaveTrack& track);
    static const WaveTrackData& Get(const WaveTrack& track);

    double GetOrigin() const;
    void SetOrigin(double origin);

    sampleFormat GetSampleFormat() const;
    void SetSampleFormat(sampleFormat format);

    float GetVolume() const;
    void SetVolume(float value);
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
    sampleFormat mFormat { floatSample };
};

static const ChannelGroup::Attachments::RegisteredFactory
    waveTrackDataFactory{
    [](auto&) { return std::make_unique<WaveTrackData>(); } };

//! Copy can't be generated by default because of atomic members
WaveTrackData::WaveTrackData(const WaveTrackData& other)
{
    SetVolume(other.GetVolume());
    SetPan(other.GetPan());
    mRate = other.mRate;
    mOrigin = other.mOrigin;
    mFormat = other.mFormat;
}

WaveTrackData::~WaveTrackData() = default;

std::unique_ptr<ClientData::Cloneable<> > WaveTrackData::Clone() const
{
    return std::make_unique<WaveTrackData>(*this);
}

WaveTrackData& WaveTrackData::Get(WaveTrack& track)
{
    return track.Attachments::Get<WaveTrackData>(waveTrackDataFactory);
}

const WaveTrackData& WaveTrackData::Get(const WaveTrack& track)
{
    return Get(const_cast<WaveTrack&>(track));
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

float WaveTrackData::GetVolume() const
{
    return mGain.load(std::memory_order_relaxed);
}

void WaveTrackData::SetVolume(float value)
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

namespace {
bool AreAligned(const WaveTrack::IntervalConstHolders& a,
                const WaveTrack::IntervalConstHolders& b)
{
    if (a.size() != b.size()) {
        return false;
    }

    const auto compare = [](const auto& a, const auto& b) {
        // clips are aligned if both sequence start/end
        // points and play start/end points of the first clip match
        // the corresponding points of the other clip
        return a->GetPlayStartTime() == b->GetPlayStartTime()
               && a->GetSequenceStartTime() == b->GetSequenceStartTime()
               && a->GetPlayEndTime() == b->GetPlayEndTime()
               && a->GetSequenceEndTime() == b->GetSequenceEndTime();
    };

    return std::mismatch(a.begin(), a.end(), b.begin(), compare).first == a.end();
}
}

//Handles possible future file values
Track::LinkType ToLinkType(int value)
{
    if (value < 0) {
        return Track::LinkType::None;
    } else if (value > 3) {
        return Track::LinkType::Group;
    }
    return static_cast<Track::LinkType>(value);
}
}

double WaveTrack::ProjectNyquistFrequency(const AudacityProject& project)
{
    auto& tracks = TrackList::Get(project);
    return std::max(ProjectRate::Get(project).GetRate(),
                    tracks.Any<const WaveTrack>().max(&WaveTrack::GetRate))
           / 2.0;
}

static auto DefaultName = XO("Audio");

WaveChannel::WaveChannel(WaveTrack& owner)
    : mOwner{owner}
{
}

WaveChannel::~WaveChannel() = default;

wxString WaveTrack::GetDefaultAudioTrackNamePreference()
{
    const auto name = AudioTrackNameSetting.ReadWithDefault(L"");

    if (name.empty() || (name == DefaultName.MSGID())) {
        // When nothing was specified,
        // the default-default is whatever translation of...
        /* i18n-hint: The default name for an audio track. */
        return DefaultName.Translation();
    } else {
        return name;
    }
}

static ProjectFileIORegistry::ObjectReaderEntry readerEntry{
    "wavetrack",
    WaveTrack::New
};

std::shared_ptr<WaveTrack> WaveTrackFactory::Create()
{
    return Create(QualitySettings::SampleFormatChoice(), mRate.GetRate());
}

std::shared_ptr<WaveTrack> WaveTrackFactory::DoCreate(size_t nChannels,
                                                      sampleFormat format, double rate)
{
    auto result = std::make_shared<WaveTrack>(
        WaveTrack::CreateToken {}, mpFactory, format, rate);
    // Set the number of channels correctly before building all channel
    // attachments
    if (nChannels > 1) {
        result->CreateRight();
    }
    // Only after make_shared returns, can weak_from_this be used, which
    // attached object factories may need
    result->AttachedTrackObjects::BuildAll();
    return result;
}

std::shared_ptr<WaveTrack> WaveTrackFactory::Create(sampleFormat format, double rate)
{
    return DoCreate(1, format, rate);
}

WaveTrack::Holder WaveTrackFactory::Create(size_t nChannels)
{
    assert(nChannels > 0);
    assert(nChannels <= 2);
    return Create(nChannels, QualitySettings::SampleFormatChoice(), mRate.GetRate());
}

TrackListHolder WaveTrackFactory::CreateMany(size_t nChannels)
{
    return CreateMany(nChannels,
                      QualitySettings::SampleFormatChoice(), mRate.GetRate());
}

void WaveTrack::MergeChannelAttachments(WaveTrack&& other)
{
    this->AttachedTrackObjects::ForCorresponding(other,
                                                 [this](TrackAttachment* pLeft, TrackAttachment* pRight){
        // Precondition of callback from ClientData::Site
        assert(pLeft && pRight);
        const auto pLeftAttachments
            =dynamic_cast<ChannelAttachmentsBase*>(pLeft);
        const auto pRightAttachments
            =dynamic_cast<ChannelAttachmentsBase*>(pRight);
        // They should have come from the same factory of channel attachments
        assert((pLeftAttachments == nullptr) == (pRightAttachments == nullptr));
        if (pLeftAttachments) {
            // First fixup the back-pointers from channel views to their track
            pRightAttachments->Reparent(shared_from_this());
            // Then "steal" them
            pLeftAttachments->MakeStereo(shared_from_this(),
                                         std::move(*pRightAttachments));
        }
    });
}

//! Erase all attachments for a given index
void WaveTrack::EraseChannelAttachments(size_t ii)
{
    this->AttachedTrackObjects::ForEach(
        [this, ii](TrackAttachment& attachment){
        if (const auto pAttachments
                =dynamic_cast<ChannelAttachmentsBase*>(&attachment)) {
            pAttachments->Erase(shared_from_this(), ii);
        }
    });
}

WaveTrack::Holder WaveTrackFactory::Create(size_t nChannels, sampleFormat format, double rate)
{
    assert(nChannels > 0);
    assert(nChannels <= 2);
    return CreateMany(nChannels, format, rate)->DetachFirst()
           ->SharedPointer<WaveTrack>();
}

TrackListHolder WaveTrackFactory::CreateMany(size_t nChannels, sampleFormat format, double rate)
{
    // There are some cases where more than two channels are requested
    if (nChannels == 2) {
        return TrackList::Temporary(nullptr, DoCreate(nChannels, format, rate));
    }
    auto result = TrackList::Temporary(nullptr);
    while (nChannels--) {
        result->Add(DoCreate(1, format, rate));
    }
    return result;
}

WaveTrack::Holder WaveTrackFactory::Create(size_t nChannels, const WaveTrack& proto)
{
    return proto.EmptyCopy(nChannels, mpFactory);
}

WaveTrack* WaveTrack::New(AudacityProject& project)
{
    auto& trackFactory = WaveTrackFactory::Get(project);
    auto& tracks = TrackList::Get(project);
    auto result = tracks.Add(trackFactory.Create());
    return result;
}

WaveTrack::WaveTrack(CreateToken&&, const SampleBlockFactoryPtr& pFactory,
                     sampleFormat format, double rate)
    : mpFactory(pFactory)
    , mChannel(*this)
{
    WaveTrackData::Get(*this).SetSampleFormat(format);
    DoSetRate(static_cast<int>(rate));
}

auto WaveTrack::Create(
    const SampleBlockFactoryPtr& pFactory, sampleFormat format, double rate)
-> Holder
{
    auto result
        =std::make_shared<WaveTrack>(CreateToken {}, pFactory, format, rate);
    // Only after make_shared returns, can weak_from_this be used, which
    // attached object factories may need
    // (but this is anyway just the factory for unit test purposes)
    result->AttachedTrackObjects::BuildAll();
    return result;
}

void WaveTrack::CopyClips(WaveClipHolders& clips,
                          SampleBlockFactoryPtr pFactory, const WaveClipHolders& orig, bool backup)
{
    for (const auto& clip : orig) {
        InsertClip(
            clips, WaveClip::NewSharedFrom(*clip, pFactory, true, backup), false,
            backup, false);
    }
}

size_t WaveChannel::NChannels() const
{
    return 1;
}

size_t WaveTrack::NChannels() const
{
    return mRightChannel.has_value() ? 2 : 1;
}

AudioGraph::ChannelType WaveChannel::GetChannelType() const
{
    if (GetTrack().Channels().size() == 1) {
        return AudioGraph::MonoChannel;
    } else if (GetChannelIndex() == 0) {
        return AudioGraph::LeftChannel;
    } else {
        // TODO: more-than-two-channels
        return AudioGraph::RightChannel;
    }
}

AudioGraph::ChannelType WaveTrack::GetChannelType() const
{
    // Not quite meaningful but preserving old behavior
    return (*Channels().begin())->WaveChannel::GetChannelType();
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack& orig)
{
    WritableSampleTrack::Init(orig);
    mpFactory = orig.mpFactory;
}

WaveTrack::~WaveTrack()
{
}

/*! @excsafety{No-fail} */
void WaveTrack::MoveTo(double origin)
{
    double delta = origin - GetStartTime();
    for (const auto& pInterval : Intervals()) {
        // assume No-fail-guarantee
        pInterval->ShiftBy(delta);
    }
    WaveTrackData::Get(*this).SetOrigin(origin);
}

/*! @excsafety{No-fail} */
void WaveTrack::ShiftBy(double t0, double delta)
{
    for (const auto& pInterval : Intervals()) { // assume No-fail-guarantee
        if (pInterval->Start() >= t0) {
            pInterval->ShiftBy(delta);
        }
    }
    const auto origin = WaveTrackData::Get(*this).GetOrigin();
    if (t0 <= origin) {
        const auto offset = t0 >= 0 ? delta : t0 + delta;
        WaveTrackData::Get(*this).SetOrigin(origin + offset);
    }
}

auto WaveTrack::DuplicateWithOtherTempo(double newTempo) const -> Holder
{
    const auto srcCopy = Duplicate();
    ::DoProjectTempoChange(*srcCopy, newTempo);
    return std::static_pointer_cast<WaveTrack>(srcCopy);
}

bool WaveTrack::LinkConsistencyFix(const bool doFix)
{
    // This implies satisfaction of the precondition of SetRate()
    assert(!doFix || IsLeader());

    const auto removeZeroClips = [](WaveClipHolders& clips) {
        // Check for zero-length clips and remove them
        for (auto it = clips.begin(); it != clips.end();) {
            if ((*it)->IsEmpty()) {
                it = clips.erase(it);
            } else {
                ++it;
            }
        }
    };

    auto err = !WritableSampleTrack::LinkConsistencyFix(doFix);

    auto linkType = GetLinkType();
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
        } else if (doFix) {
            // non-error upgrades happen here
            if (!AreAligned(SortedClipArray(), next->SortedClipArray())
                || !RateConsistencyCheck() || !FormatConsistencyCheck()) {
                SetLinkType(linkType = LinkType::None);
            } else {
                SetLinkType(linkType = LinkType::Aligned);
                //clean up zero clips only after alignment check has completed
                //this can't break alignment as there should be a "twin"
                //in the right channel which will also be removed, otherwise
                //track will be unlinked because AreAligned returned false
                removeZeroClips(NarrowClips());
                removeZeroClips(next->NarrowClips());
            }
        }
    }
    if (doFix) {
        // More non-error upgrading
        // Set the common channel group rate from the unzipped leader's rate
        if (mLegacyRate > 0) {
            WaveTrack* next{};
            if (linkType != LinkType::None) {
                next = *TrackList::Channels(this).first.advance(1);
            }
            SetRate(mLegacyRate);
            mLegacyRate = 0;
            if (next && next->mLegacyRate > 0) {
                next->SetRate(next->mLegacyRate);
                next->mLegacyRate = 0;
            }
            if (mLegacyFormat != undefinedSample) {
                WaveTrackData::Get(*this).SetSampleFormat(mLegacyFormat);
            }
            if (next && next->mLegacyFormat != undefinedSample) {
                WaveTrackData::Get(*next).SetSampleFormat(mLegacyFormat);
            }
        }
        if (linkType == LinkType::None) {
            // Did not visit the other call to removeZeroClips, do it now
            removeZeroClips(NarrowClips());
        } else {
            // Make a real wide wave track from two deserialized narrow tracks
            ZipClips();
        }
    }
    return !err;
}

static const Track::TypeInfo& typeInfo()
{
    static const Track::TypeInfo info{
        { "wave", "wave", XO("Wave Track") },
        true, &WritableSampleTrack::ClassTypeInfo() };
    return info;
}

auto WaveTrack::GetTypeInfo() const -> const TypeInfo&
{
    return typeInfo();
}

auto WaveTrack::ClassTypeInfo() -> const TypeInfo&
{
    return typeInfo();
}

Track::Holder WaveTrack::PasteInto(
    AudacityProject& project, TrackList& list) const
{
    auto& trackFactory = WaveTrackFactory::Get(project);
    auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
    auto pFirstTrack = EmptyCopy(pSampleBlockFactory);
    list.Add(pFirstTrack->SharedPointer());
    pFirstTrack->Paste(0.0, *this);
    return pFirstTrack->SharedPointer();
}

size_t WaveTrack::NIntervals() const
{
    return NarrowClips().size();
}

auto WaveTrack::GetClip(size_t iInterval) -> IntervalHolder
{
    return std::static_pointer_cast<Interval>(DoGetInterval(iInterval));
}

auto WaveTrack::GetClip(size_t iInterval) const -> IntervalConstHolder
{
    return const_cast<WaveTrack&>(*this).GetClip(iInterval);
}

std::shared_ptr<WideChannelGroupInterval>
WaveTrack::DoGetInterval(size_t iInterval)
{
    if (iInterval < NIntervals()) {
        return mClips[iInterval];
    }
    return {};
}

bool WaveTrack::HasClipNamed(const wxString& name) const
{
    auto clips = Intervals();
    return std::any_of(clips.begin(), clips.end(),
                       [&](const auto& pClip){ return pClip->GetName() == name; });
}

std::shared_ptr<::Channel> WaveTrack::DoGetChannel(size_t iChannel)
{
    auto nChannels = NChannels();
    if (iChannel >= nChannels) {
        return {};
    }
    // TODO: more-than-two-channels
    ::Channel& aliased = (iChannel == 0)
                         ? mChannel
                         : *mRightChannel;
    // Use aliasing constructor of std::shared_ptr
    return { shared_from_this(), &aliased };
}

ChannelGroup& WaveChannel::DoGetChannelGroup() const
{
    return mOwner;
}

std::shared_ptr<WaveClipChannel>
WaveChannel::GetInterval(size_t iInterval)
{
    return
        ::Channel::GetInterval<WaveClipChannel>(iInterval);
}

std::shared_ptr<const WaveClipChannel>
WaveChannel::GetInterval(size_t iInterval) const
{
    return
        ::Channel::GetInterval<const WaveClipChannel>(iInterval);
}

IteratorRange<Channel::IntervalIterator<WaveClipChannel> >
WaveChannel::Intervals() { return ::Channel::Intervals<WaveClipChannel>(); }

IteratorRange<Channel::IntervalIterator<const WaveClipChannel> >
WaveChannel::Intervals() const
{
    return ::Channel::Intervals<const WaveClipChannel>();
}

WaveClipHolders& WaveTrack::NarrowClips()
{
    return mClips;
}

const WaveClipHolders& WaveTrack::NarrowClips() const
{
    return mClips;
}

Track::Holder WaveTrack::Clone(bool backup) const
{
    auto newTrack = EmptyCopy(NChannels());
    if (backup) {
        //Init-time rate and format should be preserved as initialization
        //phase is not yet completed for that track.
        newTrack->mLegacyFormat = mLegacyFormat;
        newTrack->mLegacyRate = mLegacyRate;
    }
    newTrack->CopyClips(newTrack->mClips,
                        newTrack->mpFactory, this->mClips, backup);
    return newTrack;
}

wxString WaveTrack::MakeClipCopyName(const wxString& originalName) const
{
    auto name = originalName;
    for (auto i = 1;; ++i) {
        if (!HasClipNamed(name)) {
            return name;
        }
        //i18n-hint Template for clip name generation on copy-paste
        name = XC("%s.%i", "clip name template").Format(originalName, i).Translation();
    }
}

wxString WaveTrack::MakeNewClipName() const
{
    for (auto i = 1;; ++i) {
        //i18n-hint Template for clip name generation on inserting new empty clip
        auto name = XC("%s.%i", "clip name template").Format(GetName(), i).Translation();
        if (!HasClipNamed(name)) {
            return name;
        }
    }
}

double WaveChannel::GetRate() const
{
    return GetTrack().GetRate();
}

double WaveTrack::GetRate() const
{
    return WaveTrackData::Get(*this).GetRate();
}

void WaveTrack::SetRate(double newRate)
{
    assert(newRate > 0);
    newRate = std::max(1.0, newRate);
    DoSetRate(newRate);

    for (const auto& clip : Intervals()) {
        clip->SetRate(newRate);
    }
}

void WaveTrack::DoSetRate(double newRate)
{
    auto& data = WaveTrackData::Get(*this);
    data.SetRate(static_cast<int>(newRate));
}

float WaveTrack::GetVolume() const
{
    return WaveTrackData::Get(*this).GetVolume();
}

void WaveTrack::DoSetVolume(float value)
{
    WaveTrackData::Get(*this).SetVolume(value);
}

void WaveTrack::SetVolume(float newVolume)
{
    if (GetVolume() != newVolume) {
        DoSetVolume(newVolume);
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
    if (newPan > 1.0) {
        newPan = 1.0;
    } else if (newPan < -1.0) {
        newPan = -1.0;
    }

    if (GetPan() != newPan) {
        DoSetPan(newPan);
        Notify(true);
    }
}

float WaveChannel::GetChannelVolume(int channel) const
{
    return GetTrack().GetChannelVolume(channel);
}

float WaveTrack::GetChannelVolume(int channel) const
{
    // channel is not necessarily less than the channel group width but
    // a mono track might pan differently according to that
    float left = 1.0;
    float right = 1.0;

    const auto pan = GetPan();
    if (pan < 0) {
        right = (pan + 1.0);
    } else if (pan > 0) {
        left = 1.0 - pan;
    }

    const auto volume = GetVolume();
    if ((channel % 2) == 0) {
        return left * volume;
    } else {
        return right * volume;
    }
}

sampleCount WaveTrack::GetVisibleSampleCount() const
{
    sampleCount result{ 0 };

    for (const auto& clip : Intervals()) {
        result += clip->GetVisibleSampleCount();
    }

    return result;
}

sampleFormat WaveTrack::GetSampleFormat() const
{
    return WaveTrackData::Get(*this).GetSampleFormat();
}

/*! @excsafety{Weak} -- Might complete on only some clips */
void WaveTrack::ConvertToSampleFormat(sampleFormat format,
                                      const std::function<void(size_t)>& progressReport)
{
    for (const auto& pClip : Intervals()) {
        pClip->ConvertToSampleFormat(format, progressReport);
    }
    WaveTrackData::Get(*this).SetSampleFormat(format);
}

bool WaveTrack::IsEmpty(double t0, double t1) const
{
    if (t0 > t1) {
        return true;
    }

    //wxPrintf("Searching for overlap in %.6f...%.6f\n", t0, t1);
    for (const auto& clip : Intervals()) {
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

Track::Holder WaveTrack::Cut(double t0, double t1)
{
    if (t1 < t0) {
        THROW_INCONSISTENCY_EXCEPTION;
    }

    auto result = Copy(t0, t1);
    Clear(t0, t1);
    return result;
}

/*! @excsafety{Strong} */
auto WaveTrack::SplitCut(double t0, double t1) -> Holder
{
    if (t1 < t0) {
        THROW_INCONSISTENCY_EXCEPTION;
    }

    // SplitCut is the same as 'Copy', then 'SplitDelete'
    auto result = Copy(t0, t1);
    SplitDelete(t0, t1);
    return std::static_pointer_cast<WaveTrack>(result);
}

//Trim trims within a clip, rather than trimming everything.
//If a bound is outside a clip, it trims everything.
/*! @excsafety{Weak} */
void WaveTrack::Trim(double t0, double t1)
{
    bool inside0 = false;
    bool inside1 = false;

    for (const auto& clip : Intervals()) {
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

    //if inside0 is false, then the left selector was between
    //clips, so DELETE everything to its left.
    if (const auto endTime = GetEndTime()
        ; !inside1 && t1 < endTime
        ) {
        Clear(t1, endTime);
    }

    if (const auto startTime = GetStartTime()
        ; !inside0 && t0 > startTime
        ) {
        SplitDelete(startTime, t0);
    }
}

WaveTrack::Holder WaveTrack::EmptyCopy(size_t nChannels,
                                       const SampleBlockFactoryPtr& pFactory) const
{
    const auto rate = GetRate();
    auto result = std::make_shared<WaveTrack>(CreateToken {},
                                              pFactory, GetSampleFormat(), rate);
    if (nChannels > 1) {
        result->CreateRight();
    }
    result->Init(*this);
    // Copy state rather than BuildAll()
    Track::CopyAttachments(*result, *this, true /* deep copy */);
    // The previous line might have destroyed the rate information stored in
    // channel group data.  The copy is not yet in a TrackList.  Reassign rate
    // in case the track needs to make WaveClips before it is properly joined
    // with the opposite channel in a TrackList.
    result->DoSetRate(rate);
    result->mpFactory = pFactory ? pFactory : mpFactory;
    WaveTrackData::Get(*result).SetOrigin(0);
    return result;
}

auto WaveTrack::EmptyCopy(
    const SampleBlockFactoryPtr& pFactory) const -> Holder
{
    return EmptyCopy(NChannels(), pFactory);
}

void WaveTrack::MakeMono()
{
    mRightChannel.reset();
    for (auto& pClip : mClips) {
        pClip->DiscardRightChannel();
    }
    EraseChannelAttachments(1);
}

auto WaveTrack::MonoToStereo() -> Holder
{
    assert(!GetOwner());
    MakeMono();

    // Make temporary new mono track
    auto newTrack = Duplicate();

    // Make a list
    auto result = TrackList::Temporary(nullptr, shared_from_this());
    result->Add(newTrack);
    // Destroy the temporary track, widening this track to stereo
    ZipClips();

    return std::static_pointer_cast<WaveTrack>(result->DetachFirst());
}

auto WaveTrack::SplitChannels() -> std::vector<Holder>
{
    std::vector<Holder> result{ SharedPointer<WaveTrack>() };
    if (NChannels() == 2) {
        auto pOwner = GetOwner();
        assert(pOwner); // pre
        auto pNewTrack = result.emplace_back(EmptyCopy(1));
        for (auto& pClip : mClips) {
            pNewTrack->mClips.emplace_back(pClip->SplitChannels());
        }
        this->mRightChannel.reset();
        TrackList::AssignUniqueId(pNewTrack);
        auto iter = pOwner->Find(this);
        pOwner->Insert(*++iter, pNewTrack);
        // Fix up the channel attachments to avoid waste of space
        result[0]->EraseChannelAttachments(1);
        result[1]->EraseChannelAttachments(0);
    }
    return result;
}

void WaveTrack::SwapChannels()
{
    assert(NChannels() == 2);
    for (const auto& pClip: mClips) {
        pClip->SwapChannels();
    }
    this->AttachedTrackObjects::ForEach([this](TrackAttachment& attachment){
        if (const auto pAttachments
                =dynamic_cast<ChannelAttachmentsBase*>(&attachment)) {
            pAttachments->SwapChannels(shared_from_this());
        }
    });
}

Track::Holder WaveTrack::Copy(double t0, double t1, bool forClipboard) const
{
    if (t1 < t0) {
        THROW_INCONSISTENCY_EXCEPTION;
    }

    // If for the clipboard, then this is definitely not a backup action.
    // MH: And if it isn't ?..
    const auto backup = !forClipboard;
    auto newTrack = EmptyCopy(NChannels());
    for (const auto pClip : Intervals()) {
        // PRL:  Why shouldn't cutlines be copied and pasted too?  I don't know,
        // but that was the old behavior.  But this function is also used by the
        // Duplicate command and I changed its behavior in that case.
        if (pClip->IsEmpty()) {
            continue;
        } else if (t0 <= pClip->GetPlayStartTime() && t1 >= pClip->GetPlayEndTime()) {
            newTrack->CopyWholeClip(*pClip, t0, forClipboard, backup);
        } else if (pClip->CountSamples(t0, t1) >= 1) {
            newTrack->CopyPartOfClip(*pClip, t0, t1, forClipboard, backup);
        }
    }
    newTrack->FinishCopy(t0, t1, forClipboard);
    return newTrack;
}

void WaveTrack::CopyWholeClip(
    const Interval& clip, double t0, bool forClipboard, bool backup)
{
    const auto& pFactory = GetSampleBlockFactory();
    const auto newClip = WaveClip::NewSharedFrom(clip, pFactory, !forClipboard, backup);
    InsertInterval(newClip, false, false);
    newClip->ShiftBy(-t0);
}

void WaveTrack::CopyPartOfClip(
    const Interval& clip, double t0, double t1, bool forClipboard, bool backup)
{
    const auto& pFactory = GetSampleBlockFactory();
    auto newClip = WaveClip::NewSharedFromRange(
        clip, pFactory, !forClipboard, backup, t0, t1);
    newClip->SetName(clip.GetName());
    newClip->ShiftBy(-t0);
    if (newClip->GetPlayStartTime() < 0) {
        newClip->SetPlayStartTime(0);
    }
    InsertInterval(std::move(newClip), false, false);
}

void WaveTrack::FinishCopy(
    double t0, double t1, bool forClipboard)
{
    // AWD, Oct 2009: If the selection ends in whitespace, create a
    // placeholder clip representing that whitespace
    // PRL:  Only if we want the track for pasting into other tracks.  Not if
    // it goes directly into a project as in the Duplicate command.
    if (forClipboard && GetEndTime() + 1.0 / GetRate() < t1 - t0) {
        auto placeholder = CreateClip();
        placeholder->SetIsPlaceholder(true);
        placeholder->InsertSilence(0, (t1 - t0) - GetEndTime());
        placeholder->ShiftBy(GetEndTime());
        InsertInterval(move(placeholder), true, false);
    }
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
    WaveTrack::IntervalHolder left;
    //Contains trimmed data, which should be re-appended to
    //the clip to the right from the boundary, may be null
    WaveTrack::IntervalHolder right;
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
    double t0,                     // Start of time to clear
    double t1,                     // End of time to clear
    const WaveTrack& src,          // What to paste
    bool preserve,                 // Whether to reinsert splits/cuts
    bool merge,                    // Whether to remove 'extra' splits
    const TimeWarper* effectWarper, // How does time change
    bool clearByTrimming)
{
    // Get a modifiable copy of `src` because it may come from another project
    // with different tempo, making boundary queries incorrect.
    const auto& tempo = GetProjectTempo(*this);
    if (!tempo.has_value()) {
        THROW_INCONSISTENCY_EXCEPTION;
    }
    const auto copyHolder = src.DuplicateWithOtherTempo(*tempo);
    ClearAndPasteAtSameTempo(
        t0, t1, *copyHolder, preserve, merge, effectWarper, clearByTrimming);
}

void WaveTrack::ClearAndPasteAtSameTempo(
    double t0, double t1, const WaveTrack& src, bool preserve, bool merge,
    const TimeWarper* effectWarper, bool clearByTrimming)
{
    const auto srcNChannels = src.NChannels();
    assert(srcNChannels == NChannels());
    assert(
        GetProjectTempo(*this).has_value()
        && GetProjectTempo(*this) == GetProjectTempo(src));

    t0 = SnapToSample(t0);
    t1 = SnapToSample(t1);

    const auto endTime = src.GetEndTime();
    double dur = std::min(t1 - t0, endTime);

    // If duration is 0, then it's just a plain paste
    if (dur == 0.0) {
        // use Weak-guarantee
        PasteWaveTrack(t0, src, merge);
        return;
    }

    auto& track = *this;

    std::vector<SplitInfo> splits;
    IntervalHolders cuts;

    //helper routine, that finds SplitInfo by time value,
    //or creates a new one if no one exists yet
    auto get_split = [&](double time) {
        auto it = std::find_if(splits.begin(), splits.end(),
                               [time](const SplitInfo& split) { return split.time == time; });
        if (it == splits.end()) {
            it = splits.insert(
                splits.end(),
                { time, nullptr, nullptr, std::nullopt, std::nullopt }
                );
        }
        return it;
    };

    // If provided time warper was NULL, use a default one that does nothing
    IdentityTimeWarper localWarper;
    const TimeWarper* warper = (effectWarper ? effectWarper : &localWarper);

    const auto roundTime = [&track](double t){
        return track.SnapToSample(t);
    };

    // Align to a sample
    t0 = roundTime(t0);
    t1 = roundTime(t1);

    // Save the cut/split lines whether preserving or not since merging
    // needs to know if a clip boundary is being crossed since Paste()
    // will add split lines around the pasted clip if so.
    for (const auto&& clip : track.Intervals()) {
        double st;

        // Remember clip boundaries as locations to split
        // we need to copy clips, trims and names, because the original ones
        // could be changed later during Clear/Paste routines
        st = roundTime(clip->GetPlayStartTime());
        if (st >= t0 && st <= t1) {
            auto it = get_split(st);
            if (clip->GetTrimLeft() != 0) {
                //keep only hidden left part
                it->right = track.CopyClip(*clip, false);
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
                it->left = track.CopyClip(*clip, false);
                it->left->SetTrimRight(.0);
                it->left->ClearLeft(clip->GetPlayEndTime());
            }
            it->leftClipName = clip->GetName();
        }

        // Search for cut lines
        auto cutlines = clip->GetCutLines();
        for (auto& cut : cutlines) {
            const auto unrounded
                =clip->GetSequenceStartTime() + cut->GetSequenceStartTime();
            const double cs = roundTime(unrounded);

            // Remember cut point
            if (cs >= t0 && cs <= t1) {
                // Remember the absolute offset and add to our cuts array.
                cut->SetSequenceStartTime(cs);
                bool removed = clip->RemoveCutLine(unrounded);
                assert(removed);
                cuts.push_back(move(cut));
            }
        }
    }

    const auto tolerance = 2.0 / track.GetRate();

    // This is not a split-cut operation.
    constexpr auto split = false;

    // Now, clear the selection
    track.HandleClear(t0, t1, false, split, clearByTrimming);

    // And paste in the new data
    track.PasteWaveTrackAtSameTempo(t0, src, merge);

    // First, merge the new clip(s) in with the existing clips
    if (merge && splits.size() > 0) {
        {
            // Now t1 represents the absolute end of the pasted data.
            t1 = t0 + endTime;

            // Get a sorted array of the clips
            auto clips = track.SortedIntervalArray();

            // Scan the sorted clips for the first clip whose start time
            // exceeds the pasted regions end time.
            IntervalHolder prev;
            for (const auto clip : clips) {
                // Merge this clip and the previous clip if the end time
                // falls within it and this isn't the first clip in the track.
                if (fabs(t1 - clip->GetPlayStartTime()) < tolerance) {
                    if (prev && clip->HasEqualPitchAndSpeed(*prev)) {
                        track.MergeClips(
                            track.GetClipIndex(*prev), track.GetClipIndex(*clip));
                    }
                    break;
                }
                prev = clip;
            }
        }

        {
            // Refill the array since clips have changed.
            auto clips = track.SortedIntervalArray();

            // Scan the sorted clips to look for the start of the pasted
            // region.
            IntervalHolder prev;
            for (const auto clip : clips) {
                if (prev) {
                    // It must be that clip is what was pasted and it begins where
                    // prev ends.
                    // use Weak-guarantee
                    if (clip->HasEqualPitchAndSpeed(*prev)) {
                        track.MergeClips(
                            track.GetClipIndex(*prev), track.GetClipIndex(*clip));
                    }
                    break;
                }
                if (fabs(t0 - clip->GetPlayEndTime()) < tolerance) {
                    // Merge this clip and the next clip if the start time
                    // falls within it and this isn't the last clip in the track.
                    prev = clip;
                } else {
                    prev = nullptr;
                }
            }
        }
    }

    // Restore cut/split lines
    if (preserve) {
        auto attachLeft = [](Interval& target, Interval& src) {
            // What this lambda does is restoring the left hidden data of `target`
            // that was cleared by `HandleClear`. Hence, `target` has no left
            // hidden data at this stage.
            assert(target.GetTrimLeft() == 0);
            if (target.GetTrimLeft() != 0) {
                return;
            }

            // `src` was created by copy from `target`, so they have equal width,
            // pitch and speed.
            assert(target.NChannels() == src.NChannels());
            assert(target.HasEqualPitchAndSpeed(src));

            auto trim = src.GetPlayEndTime() - src.GetPlayStartTime();
            auto success = target.Paste(target.GetPlayStartTime(), src);
            assert(success); // because of precondition above
            target.SetTrimLeft(trim);
            //Play start time needs to be adjusted after
            //prepending data to the sequence
            target.ShiftBy(-trim);
        };

        auto attachRight = [](Interval& target, Interval& src)
        {
            // See `attachLeft` for rationale behind these asserts.
            assert(target.GetTrimRight() == 0);
            if (target.GetTrimRight() != 0) {
                return;
            }
            assert(target.NChannels() == src.NChannels());
            assert(target.HasEqualPitchAndSpeed(src));

            auto trim = src.GetPlayEndTime() - src.GetPlayStartTime();
            auto success = target.Paste(target.GetPlayEndTime(), src);
            assert(success); // because of precondition above
            target.SetTrimRight(trim);
        };

        // Restore the split lines and trims, transforming the position appropriately
        for (const auto& split: splits) {
            auto at = roundTime(warper->Warp(split.time));
            for (const auto&& clip : track.Intervals()) {
                // Clips in split began as copies of a clip in the track,
                // therefore have the same width, satisfying preconditions to
                // attach
                if (clip->SplitsPlayRegion(at)) {//strictly inside
                    auto newClip = CopyClip(*clip, true);
                    clip->ClearRight(at);
                    newClip->ClearLeft(at);
                    if (split.left) {
                        // clip was cleared right
                        attachRight(*clip, *split.left);
                    }
                    if (split.right) {
                        // new clip was cleared left
                        attachLeft(*newClip, *split.right);
                    }
                    track.InsertInterval(move(newClip), false);
                    break;
                } else if (clip->GetPlayStartSample()
                           == track.TimeToLongSamples(at) && split.right) {
                    // Satisfy the precondition of attachLeft first!
                    const auto trim = clip->GetTrimLeft();
                    const auto seqStartTime = clip->GetSequenceStartTime();
                    clip->Clear(seqStartTime, seqStartTime + trim);
                    // This clearing, although only removing the hidden part, moved
                    // the clip leftwards. We don't want this in this case.
                    clip->ShiftBy(trim);
                    attachLeft(*clip, *split.right);
                    break;
                } else if (clip->GetPlayEndSample()
                           == track.TimeToLongSamples(at) && split.left) {
                    // Satisfy the precondition of attachRight first!
                    clip->Clear(
                        clip->GetPlayEndTime(), clip->GetSequenceEndTime());
                    attachRight(*clip, *split.left);
                    break;
                }
            }
        }

        //Restore clip names
        for (const auto& split : splits) {
            auto s = track.TimeToLongSamples(warper->Warp(split.time));
            for (auto&& clip : track.Intervals()) {
                if (split.rightClipName.has_value() && clip->GetPlayStartSample() == s) {
                    clip->SetName(*split.rightClipName);
                } else if (split.leftClipName.has_value() && clip->GetPlayEndSample() == s) {
                    clip->SetName(*split.leftClipName);
                }
            }
        }

        // Restore the saved cut lines, also transforming if time altered
        for (const auto&& clip : track.Intervals()) {
            const double st = clip->GetPlayStartTime();
            const double et = clip->GetPlayEndTime();

            // Scan the cuts for any that live within this clip
            for (auto& cut : cuts) {
                if (!cut) {
                    continue;
                }

                //cutlines in this array were orphaned previously
                double cs = cut->GetSequenceStartTime();

                // Offset the cut from the start of the clip and add it to
                // this clips cutlines.
                if (cs >= st && cs <= et) {
                    cut->SetSequenceStartTime(warper->Warp(cs) - st);
                    clip->AddCutLine(cut);
                    cut = {};
                }
            }
        }
    }
}

/*! @excsafety{Strong} */
void WaveTrack::SplitDelete(double t0, double t1)
{
    constexpr bool addCutLines = false;
    constexpr bool split = true;
    HandleClear(t0, t1, addCutLines, split);
}

std::ptrdiff_t WaveTrack::FindClip(const Interval& clip)
{
    auto clips = Intervals();
    const auto begin = clips.begin();
    const auto pred = [&](auto pClip){ return pClip.get() == &clip; };
    auto iter = std::find_if(begin, clips.end(), pred);
    return std::distance(begin, iter);
}

void WaveTrack::RemoveClip(std::ptrdiff_t distance)
{
    auto& clips = NarrowClips();
    if (distance < clips.size()) {
        clips.erase(clips.begin() + distance);
    }
}

/*! @excsafety{Strong} */
void WaveTrack::HandleClear(double t0, double t1, bool addCutLines,
                            const bool split, const bool clearByTrimming)
{
    // For debugging, use an ASSERT so that we stop
    // closer to the problem.
    wxASSERT(t1 >= t0);
    if (t1 < t0) {
        THROW_INCONSISTENCY_EXCEPTION;
    }

    t0 = SnapToSample(t0);
    t1 = SnapToSample(t1);

    IntervalHolders clipsToDelete;
    IntervalHolders clipsToAdd;

    // We only add cut lines when deleting in the middle of a single clip
    // The cut line code is not really prepared to handle other situations
    if (addCutLines) {
        for (const auto& clip : Intervals()) {
            if (clip->PartlyWithinPlayRegion(t0, t1)) {
                addCutLines = false;
                break;
            }
        }
    }

    for (const auto& clip : Intervals()) {
        if (clip->CoversEntirePlayRegion(t0, t1)) {
            // Whole clip must be deleted - remember this
            clipsToDelete.push_back(clip);
        } else if (clip->IntersectsPlayRegion(t0, t1)) {
            // Clip data is affected by command
            if (addCutLines) {
                // Don't modify this clip in place, because we want a strong
                // guarantee, and might modify another clip
                clipsToDelete.push_back(clip);
                auto newClip = CopyClip(*clip, true);
                newClip->ClearAndAddCutLine(t0, t1);
                clipsToAdd.push_back(move(newClip));
            } else {
                if (split || clearByTrimming) {
                    // Three cases:

                    if (clip->BeforePlayRegion(t0)) {
                        // Delete from the left edge

                        // Don't modify this clip in place, because we want a strong
                        // guarantee, and might modify another clip
                        clipsToDelete.push_back(clip);
                        auto newClip = CopyClip(*clip, true);
                        newClip->TrimLeft(t1 - clip->GetPlayStartTime());
                        if (!split) {
                            // If this is not a split-cut, where things are left in
                            // place, we need to reposition the clip.
                            newClip->ShiftBy(t0 - t1);
                        }
                        clipsToAdd.push_back(move(newClip));
                    } else if (clip->AfterPlayRegion(t1)) {
                        // Delete to right edge

                        // Don't modify this clip in place, because we want a strong
                        // guarantee, and might modify another clip
                        clipsToDelete.push_back(clip);
                        auto newClip = CopyClip(*clip, true);
                        newClip->TrimRight(clip->GetPlayEndTime() - t0);

                        clipsToAdd.push_back(move(newClip));
                    } else {
                        // Delete in the middle of the clip...we actually create two
                        // NEW clips out of the left and right halves...

                        auto leftClip = CopyClip(*clip, true);
                        leftClip->TrimRight(clip->GetPlayEndTime() - t0);
                        clipsToAdd.push_back(move(leftClip));

                        auto rightClip = CopyClip(*clip, true);
                        rightClip->TrimLeft(t1 - clip->GetPlayStartTime());
                        if (!split) {
                            // If this is not a split-cut, where things are left in
                            // place, we need to reposition the clip.
                            rightClip->ShiftBy(t0 - t1);
                        }
                        clipsToAdd.push_back(move(rightClip));

                        clipsToDelete.push_back(clip);
                    }
                } else {
                    // (We are not doing a split cut)

                    // Don't modify this clip in place, because we want a strong
                    // guarantee, and might modify another clip
                    clipsToDelete.push_back(clip);
                    auto newClip = CopyClip(*clip, true);

                    // clip->Clear keeps points < t0 and >= t1 via Envelope::CollapseRegion
                    newClip->Clear(t0, t1);

                    clipsToAdd.push_back(move(newClip));
                }
            }
        }
    }

    // Only now, change the contents of this track
    // use No-fail-guarantee for the rest

    for (const auto& clip: clipsToDelete) {
        RemoveInterval(clip);
    }

    const auto moveClipsLeft = !split && GetEditClipsCanMove();
    if (moveClipsLeft) {
        // Clip is "behind" the region -- offset it unless we're splitting
        // or we're using the "don't move other clips" mode
        for (const auto& clip : Intervals()) {
            if (clip->AtOrBeforePlayRegion(t1)) {
                clip->ShiftBy(-(t1 - t0));
            }
        }
    }

    for (auto& clip: clipsToAdd) {
        InsertInterval(move(clip), false);
    }
}

void WaveTrack::SyncLockAdjust(double oldT1, double newT1)
{
    const auto endTime = GetEndTime();
    if (newT1 > oldT1
        &&// JKC: This is a rare case where using >= rather than > on a float matters.
          // GetEndTime() looks through the clips and may give us EXACTLY the same
          // value as T1, when T1 was set to be at the end of one of those clips.
        oldT1 >= endTime) {
        return;
    }
    if (newT1 > oldT1) {
        // Insert space within the track

        // If track is empty at oldT1 insert whitespace; otherwise, silence
        if (IsEmpty(oldT1, oldT1)) {
            // Check if clips can move
            if (EditClipsCanMove.Read()) {
                const auto offset = newT1 - oldT1;
                const auto rate = GetRate();
                for (const auto& clip : Intervals()) {
                    if (clip->GetPlayStartTime() > oldT1 - (1.0 / rate)) {
                        clip->ShiftBy(offset);
                    }
                }
            }
            return;
        } else {
            // AWD: Could just use InsertSilence() on its own here, but it doesn't
            // follow EditClipCanMove rules (Paste() does it right)
            const auto duration = newT1 - oldT1;
            auto tmp = EmptyCopy(mpFactory);
            tmp->InsertSilence(0.0, duration);
            tmp->Flush();
            Paste(oldT1, *tmp);
        }
    } else if (newT1 < oldT1) {
        Clear(newT1, oldT1);
    }
}

void WaveTrack::PasteWaveTrack(double t0, const WaveTrack& other, bool merge)
{
    // Get a modifiable copy of `src` because it may come from another project
    // with different tempo, making boundary queries incorrect.
    const auto& tempo = GetProjectTempo(*this);
    if (!tempo.has_value()) {
        THROW_INCONSISTENCY_EXCEPTION;
    }
    const auto copyHolder = other.DuplicateWithOtherTempo(*tempo);
    PasteWaveTrackAtSameTempo(t0, *copyHolder, merge);
}

void WaveTrack::PasteWaveTrackAtSameTempo(
    double t0, const WaveTrack& other, bool merge)
{
    const auto otherNChannels = other.NChannels();
    assert(otherNChannels == NChannels());
    assert(
        GetProjectTempo(*this).has_value()
        && GetProjectTempo(*this) == GetProjectTempo(other));
    const auto startTime = other.GetStartTime();
    const auto endTime = other.GetEndTime();

    const auto insertDuration = endTime;
    auto& track = *this;
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

    if (other.GetNumClips() == 0) {
        return;
    }

    t0 = track.SnapToSample(t0);

    //wxPrintf("paste: we have at least one clip\n");

    const auto clipAtT0 = track.GetIntervalAtTime(t0);
    const auto otherFirstClip = other.GetLeftmostClip();
    const auto otherLastClip = other.GetRightmostClip();
    const auto pitchAndSpeedMatch
        =!clipAtT0 || (clipAtT0->HasEqualPitchAndSpeed(*otherFirstClip)
                       && clipAtT0->HasEqualPitchAndSpeed(*otherLastClip));

    // `singleClipMode` will try to merge. Only allow this if clips on both ends
    // of the selection have equal stretch ratio.
    const bool singleClipMode
        =other.GetNumClips() == 1
          && std::abs(startTime) < track.LongSamplesToTime(1) * 0.5
          && pitchAndSpeedMatch && merge;

    const auto rate = track.GetRate();
    if (insertDuration != 0 && insertDuration < 1.0 / rate) {
        // PRL:  I added this check to avoid violations of preconditions in other WaveClip and Sequence
        // methods, but allow the value 0 so I don't subvert the purpose of commit
        // 739422ba70ceb4be0bb1829b6feb0c5401de641e which causes append-recording always to make
        // a new clip.
        return;
    }

    //wxPrintf("Check if we need to make room for the pasted data\n");

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
        for (const auto& clip : track.Intervals()) {
            if (clip->GetPlayStartTime() > t0 - (1.0 / rate)) {
                clip->ShiftBy(insertDuration);
            }
        }
    } else {
        if (!merge) {
            track.SplitAt(t0);
        }
        const auto clipAtT0 = track.GetClipAtTime(t0);
        const auto t = clipAtT0 ? clipAtT0->GetPlayEndTime() : t0;
        if (!track.IsEmpty(t, t + insertDuration)) {
            throw notEnoughSpaceException;
        }
    }

    // See if the clipboard data is one clip only and if it should be merged. If
    // edit-clip-can-move mode is checked, merging happens only if the pasting
    // point splits a clip. If it isn't, merging also happens when the pasting
    // point is at the exact beginning of a clip.
    if (singleClipMode && merge) {
        // Single clip mode
        // wxPrintf("paste: checking for single clip mode!\n");

        IntervalHolder insideClip{};
        for (const auto& clip : track.Intervals()) {
            if (editClipCanMove) {
                if (clip->SplitsPlayRegion(t0)) {
                    //wxPrintf("t0=%.6f: inside clip is %.6f ... %.6f\n",
                    //       t0, clip->GetStartTime(), clip->GetEndTime());
                    insideClip = clip;
                    break;
                }
            } else {
                // If clips are immovable we also allow prepending to clips
                if (clip->WithinPlayRegion(t0)) {
                    insideClip = clip;
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
                for (const auto& clip : track.Intervals()) {
                    if (clip->GetPlayStartTime() > insideClip->GetPlayStartTime()
                        && insideClip->GetPlayEndTime() + insertDuration
                        > clip->GetPlayStartTime()) {
                        // Strong-guarantee in case of this path
                        // not that it matters.
                        throw notEnoughSpaceException;
                    }
                }
            }
            if (auto pClip = other.GetClip(0)) {
                // This branch only gets executed in `singleClipMode` - we've
                // already made sure that stretch ratios are equal, satisfying
                // `WaveClip::Paste`'s precondition.
                assert(insideClip->GetStretchRatio() == pClip->GetStretchRatio());
                // This too should follow from the assertion of the same number
                // of channels in the tracks, near the top
                assert(insideClip->NChannels() == pClip->NChannels());
                bool success = insideClip->Paste(t0, *pClip);
                assert(success);
            }
            return;
        }
        // Just fall through and exhibit NEW behaviour
    }

    // Insert NEW clips
    //wxPrintf("paste: multi clip mode!\n");

    if (!editClipCanMove
        && !track.IsEmpty(t0, t0 + insertDuration - 1.0 / rate)) {
        // Strong-guarantee in case of this path
        // not that it matters.
        throw notEnoughSpaceException;
    }

    for (const auto& clip : other.Intervals()) {
        // AWD Oct. 2009: Don't actually paste in placeholder clips
        if (!clip->GetIsPlaceholder()) {
            // If clip has no name (i.e. generated), assigning a new name
            const auto name = clip->GetName().IsEmpty()
                              ? track.MakeNewClipName()
                              : clip->GetName();
            const auto oldPlayStart = clip->GetPlayStartTime();
            const auto newSequenceStart
                =(oldPlayStart + t0) - clip->GetTrimLeft();
            const auto newClip = CreateClip(newSequenceStart, name, clip.get());
            newClip->Resample(rate);
            track.InsertInterval(move(newClip), false);
        }
    }
}

bool WaveTrack::RateConsistencyCheck() const
{
    // The channels and all clips in them should have the same sample rate.
    std::optional<double> oRate;
    auto channels = TrackList::Channels(this);
    return std::all_of(channels.begin(), channels.end(),
                       [&](const WaveTrack* pTrack){
        if (!pTrack) {
            return false;
        }

        const auto rate = pTrack->mLegacyRate;
        if (!oRate) {
            oRate = rate;
        } else if (*oRate != rate) {
            return false;
        }
        return true;
    });
}

bool WaveTrack::FormatConsistencyCheck() const
{
    const auto channels = TrackList::Channels(this);
    return std::all_of(channels.begin(), channels.end(),
                       [&](const WaveTrack* pTrack){
        return pTrack && pTrack->mLegacyFormat == mLegacyFormat;
    });
}

bool WaveTrack::InsertClip(WaveClipHolders& clips, WaveClipHolder clip,
                           bool newClip, bool backup, bool allowEmpty)
{
    if (!backup && !clip->GetIsPlaceholder() && !allowEmpty && clip->IsEmpty()) {
        return false;
    }

    const auto& tempo = GetProjectTempo(*this);
    if (tempo.has_value()) {
        clip->OnProjectTempoChange(std::nullopt, *tempo);
    }
    clips.push_back(std::move(clip));
    Publish({ clips.back(),
              newClip ? WaveTrackMessage::New : WaveTrackMessage::Inserted });

    return true;
}

void WaveTrack::ApplyPitchAndSpeed(
    std::optional<TimeInterval> interval, ProgressReporter reportProgress)
{
    // Assert that the interval is reasonable, but this function will be no-op
    // anyway if not
    assert(!interval.has_value()
           || interval->first <= interval->second);
    if (GetNumClips() == 0) {
        return;
    }

    if (interval) {
        interval.emplace(
            SnapToSample(interval->first), SnapToSample(interval->second));
    }

    // If there is no specified interval, we look at all clips in the track.
    const auto examinedClips
        =interval ? GetSortedClipsIntersecting(interval->first, interval->second)
          : SortedClipArray();
    if (examinedClips.empty()) {
        return;
    }

    const auto startTime
        =interval
          ? std::max(examinedClips.front()->GetPlayStartTime(), interval->first)
          : examinedClips.front()->GetPlayStartTime();
    const auto endTime
        =interval
          ? std::min(examinedClips.back()->GetPlayEndTime(), interval->second)
          : examinedClips.back()->GetPlayEndTime();
    if (startTime >= endTime) {
        assert(false);
        return;
    }

    if (auto clipAtT0 = GetClipAtTime(startTime);
        clipAtT0 && clipAtT0->SplitsPlayRegion(startTime)
        && clipAtT0->HasPitchOrSpeed()) {
        Split(startTime, startTime);
    }
    if (auto clipAtT1 = GetClipAtTime(endTime);
        clipAtT1 && clipAtT1->SplitsPlayRegion(endTime)
        && clipAtT1->HasPitchOrSpeed()) {
        Split(endTime, endTime);
    }

    IntervalHolders srcIntervals;
    auto clip = GetIntervalAtTime(startTime);
    while (clip && clip->GetPlayStartTime() < endTime)
    {
        if (clip->HasPitchOrSpeed()) {
            srcIntervals.push_back(clip);
        }
        clip = GetNextInterval(*clip, PlaybackDirection::forward);
    }

    ApplyPitchAndSpeedOnIntervals(srcIntervals, reportProgress);
}

/*! @excsafety{Weak} */
void WaveTrack::Paste(double t0, const Track& src)
{
    if (const auto other = dynamic_cast<const WaveTrack*>(&src)) {
        // Currently `Paste` isn't used by code that wants the newer "don't merge
        // when copy/pasting" behaviour ...
        constexpr auto merge = true;
        PasteWaveTrack(t0, *other, merge);
    } else {
        // THROW_INCONSISTENCY_EXCEPTION; // ?
        (void)0;// Empty if intentional.
    }
}

void WaveTrack::Silence(double t0, double t1, ProgressReporter reportProgress)
{
    if (t1 < t0) {
        THROW_INCONSISTENCY_EXCEPTION;
    }

    ApplyPitchAndSpeed({ { t0, t1 } }, std::move(reportProgress));

    auto start = TimeToLongSamples(t0);
    auto end = TimeToLongSamples(t1);

    for (const auto& pClip : Intervals()) {
        auto clipStart = pClip->GetPlayStartSample();
        auto clipEnd = pClip->GetPlayEndSample();
        if (clipEnd > start && clipStart < end) {
            auto offset = std::max(start - clipStart, sampleCount(0));
            // Clip sample region and Get/Put sample region overlap
            auto length = std::min(end, clipEnd) - (clipStart + offset);
            pClip->SetSilence(offset, length);
        }
    }
}

/*! @excsafety{Strong} */
void WaveTrack::InsertSilence(double t, double len)
{
    // Nothing to do, if length is zero.
    // Fixes Bug 1626
    if (len == 0) {
        return;
    }
    if (len <= 0) {
        THROW_INCONSISTENCY_EXCEPTION;
    }

    auto&& clips = Intervals();
    if (clips.empty()) {
        // Special case if there is no clip yet
        auto clip = CreateClip(0);
        clip->InsertSilence(0, len);
        // use No-fail-guarantee
        clip->SetPlayStartTime(t);
        InsertInterval(move(clip), true);
    } else {
        // Assume at most one clip contains t
        const auto end = clips.end();
        const auto it = std::find_if(clips.begin(), end,
                                     [&](const IntervalHolder& clip) { return clip->SplitsPlayRegion(t); });

        // use Strong-guarantee
        if (it != end) {
            (*it)->InsertSilence(t, len);
        }

        // use No-fail-guarantee
        for (const auto&& clip : clips) {
            if (clip->BeforePlayRegion(t)) {
                clip->ShiftBy(len);
            }
        }
    }
}

//Performs the opposite of Join
//Analyses selected region for possible Joined clips and disjoins them
/*! @excsafety{Weak} */
void WaveTrack::Disjoin(double t0, double t1)
{
    auto minSamples = TimeToLongSamples(WAVETRACK_MERGE_POINT_TOLERANCE);
    const size_t maxAtOnce = 1048576;
    std::vector<float> buffer;
    std::vector<samplePtr> buffers;
    Regions regions;

    const size_t width = NChannels();

    for (const auto& interval : Intervals()) {
        double startTime = interval->Start();
        double endTime = interval->End();

        if (endTime < t0 || startTime > t1) {
            continue;
        }

        // Assume all clips will have the same width
        if (buffer.empty()) {
            buffer.resize(maxAtOnce * width);
            buffers.resize(width);
            auto pBuffer = buffer.data();
            for (size_t ii = 0; ii < width; ++ii, pBuffer += maxAtOnce) {
                buffers[ii] = reinterpret_cast<samplePtr>(pBuffer);
            }
        }

        const auto allZeroesAt = [&](size_t i) {
            auto pData = buffer.data() + i;
            for (size_t ii = 0; ii < width; ++ii, pData += maxAtOnce) {
                if (*pData != 0.0) {
                    return false;
                }
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

            for (auto channel : interval->Channels()) {
                channel->GetSamples(
                    *bufferIt++, floatSample, start + done, numSamples);
            }

            for (decltype(numSamples) i = 0; i < numSamples; ++i) {
                auto curSamplePos = start + done + i;

                //start a NEW sequence
                if (seqStart == -1 && allZeroesAt(i)) {
                    seqStart = curSamplePos;
                } else if (curSamplePos == end - 1 || !allZeroesAt(i)) {
                    if (seqStart != -1) {
                        decltype(end) seqEnd;

                        //consider the end case, where selection ends in zeroes
                        if (curSamplePos == end - 1 && allZeroesAt(i)) {
                            seqEnd = end;
                        } else {
                            seqEnd = curSamplePos;
                        }
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

    for (const auto& region : regions) {
        SplitDelete(region.start, region.end);
    }
}

/*! @excsafety{Weak} */
void WaveTrack::Join(
    double t0, double t1, const ProgressReporter& reportProgress)
{
    // Merge all WaveClips overlapping selection into one
    const auto& intervals = Intervals();

    {
        IntervalHolders intervalsToJoin;
        for (const auto& interval : intervals) {
            if (interval->IntersectsPlayRegion(t0, t1)) {
                intervalsToJoin.push_back(interval);
            }
        }
        if (intervalsToJoin.size() < 2u) {
            return;
        }
        if (std::any_of(
                intervalsToJoin.begin() + 1, intervalsToJoin.end(),
                [first = intervalsToJoin[0]](const auto& interval) {
            return !first->HasEqualPitchAndSpeed(*interval);
        })) {
            ApplyPitchAndSpeedOnIntervals(intervalsToJoin, reportProgress);
        }
    }

    IntervalHolders clipsToDelete;
    IntervalHolder newClip{};

    const auto rate = GetRate();
    for (const auto& clip: intervals) {
        if (clip->IntersectsPlayRegion(t0, t1)) {
            // Put in sorted order
            auto it = clipsToDelete.begin(), end = clipsToDelete.end();
            for (; it != end; ++it) {
                if ((*it)->GetPlayStartTime() > clip->GetPlayStartTime()) {
                    break;
                }
            }
            //wxPrintf("Insert clip %.6f at position %d\n", clip->GetStartTime(), i);
            clipsToDelete.insert(it, clip);
        }
    }

    //if there are no clips to delete, nothing to do
    if (clipsToDelete.empty()) {
        return;
    }

    const auto firstToDelete = clipsToDelete[0].get();
    auto t = firstToDelete->GetPlayStartTime();
    //preserve left trim data if any
    newClip = CreateClip(
        firstToDelete->GetSequenceStartTime(),
        firstToDelete->GetName());

    for (const auto& clip : clipsToDelete) {
        // wxPrintf("t=%.6f adding clip (offset %.6f, %.6f ... %.6f)\n",
        //       t, clip->GetOffset(), clip->GetStartTime(),
        //       clip->GetEndTime());

        if (clip->GetPlayStartTime() - t > (1.0 / rate)) {
            double addedSilence = (clip->GetPlayStartTime() - t);
            // wxPrintf("Adding %.6f seconds of silence\n");
            auto offset = clip->GetPlayStartTime();
            auto value = clip->GetEnvelope().GetValue(offset);
            newClip->AppendSilence(addedSilence, value);
            t += addedSilence;
        }

        // wxPrintf("Pasting at %.6f\n", t);
        bool success = newClip->Paste(t, *clip);
        assert(success); // promise of DoCreateClip

        t = newClip->GetPlayEndTime();

        RemoveClip(FindClip(*clip));
    }

    InsertInterval(move(newClip), false);
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveChannel::AppendBuffer(constSamplePtr buffer, sampleFormat format,
                               size_t len, unsigned stride, sampleFormat effectiveFormat)
{
    const size_t iChannel = GetChannelIndex();
    return GetTrack()
           .Append(iChannel, buffer, format, len, stride, effectiveFormat);
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveChannel::Append(constSamplePtr buffer, sampleFormat format,
                         size_t len)
{
    const size_t iChannel = GetChannelIndex();
    return GetTrack()
           .Append(iChannel, buffer, format, len, 1, widestSampleFormat);
}

/*! @excsafety{Partial}
-- Some prefix (maybe none) of the buffer is appended,
and no content already flushed to disk is lost. */
bool WaveTrack::Append(size_t iChannel,
                       constSamplePtr buffer, sampleFormat format,
                       size_t len, unsigned int stride, sampleFormat effectiveFormat)
{
    assert(iChannel < NChannels());
    auto pTrack = this;
    constSamplePtr buffers[]{ buffer };
    auto pClip = RightmostOrNewClip();
    return pClip->Append(iChannel, 1,
                         buffers, format, len, stride, effectiveFormat);
}

size_t WaveTrack::GetBestBlockSize(sampleCount s) const
{
    auto bestBlockSize = GetMaxBlockSize();

    for (const auto& clip : Intervals()) {
        auto startSample = clip->GetPlayStartSample();
        auto endSample = clip->GetPlayEndSample();
        if (s >= startSample && s < endSample) {
            // ignore extra channels (this function will soon be removed)
            bestBlockSize
                =clip->GetBestBlockSize(s - clip->GetSequenceStartSample());
            break;
        }
    }

    return bestBlockSize;
}

size_t WaveTrack::GetMaxBlockSize() const
{
    const auto clips = Intervals();
    auto maxblocksize = std::accumulate(clips.begin(), clips.end(), size_t {},
                                        [](size_t acc, auto pClip){
        return std::max(acc, pClip->GetMaxBlockSize());
    });

    if (maxblocksize == 0) {
        // We really need the maximum block size, so create a
        // temporary sequence to get it.
        maxblocksize
            =Sequence{ mpFactory, SampleFormats{ GetSampleFormat(), GetSampleFormat() } }
        .GetMaxBlockSize();
    }

    wxASSERT(maxblocksize > 0);

    return maxblocksize;
}

size_t WaveTrack::GetIdealBlockSize()
{
    // ignore extra channels (this function will soon be removed)
    return (*NewestOrNewClip()->Channels().begin())->GetClip()
           .GetSequence(0)->GetIdealBlockSize();
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
    if (NIntervals() == 0) {
        return;
    }
    // After appending, presumably.  Do this to the clip that gets appended.
    GetRightmostClip()->Flush();
}

void WaveTrack::RepairChannels()
{
    for (auto pInterval : Intervals()) {
        pInterval->RepairChannels();
    }
}

void WaveTrack::SetLegacyFormat(sampleFormat format)
{
    mLegacyFormat = format;
}

const ChannelGroup* WaveTrack::FindChannelGroup() const
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

const char* WaveTrack::WaveTrack_tag = "wavetrack";

static constexpr auto Offset_attr = "offset";
static constexpr auto Rate_attr = "rate";
static constexpr auto Volume_attr
    ="gain"; // https://github.com/audacity/audacity/issues/7097: keep
             // backward-compatibility with older projects.
static constexpr auto Pan_attr = "pan";
static constexpr auto Linked_attr = "linked";
static constexpr auto SampleFormat_attr = "sampleformat";
static constexpr auto Channel_attr = "channel"; // write-only!

bool WaveTrack::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == WaveTrack_tag) {
        double dblValue;
        long nValue;

        for (const auto& pair : attrs) {
            const auto& attr = pair.first;
            const auto& value = pair.second;

            if (attr == Rate_attr) {
                // mRate is an int, but "rate" in the project file is a float.
                if (!value.TryGet(dblValue)
                    || (dblValue < 1.0) || (dblValue > 1000000.0)) { // allow a large range to be read
                    return false;
                }

                // Defer the setting of rate until LinkConsistencyFix
                mLegacyRate = lrint(dblValue);
            } else if (attr == Offset_attr && value.TryGet(dblValue)) {
                // Offset is only relevant for legacy project files. The value
                // is cached until the actual WaveClip containing the legacy
                // track is created.
                mLegacyProjectFileOffset = dblValue;
            } else if (this->WritableSampleTrack::HandleXMLAttribute(attr, value)) {
            } else if (this->Track::HandleCommonXMLAttribute(attr, value)) {
            } else if (attr == Volume_attr && value.TryGet(dblValue)) {
                DoSetVolume(dblValue);
            } else if (attr == Pan_attr && value.TryGet(dblValue)
                       && (dblValue >= -1.0) && (dblValue <= 1.0)) {
                DoSetPan(dblValue);
            } else if (attr == Linked_attr && value.TryGet(nValue)) {
                SetLinkType(ToLinkType(nValue), false);
            } else if (attr == SampleFormat_attr && value.TryGet(nValue)
                       && Sequence::IsValidSampleFormat(nValue)) {
                //Remember sample format until consistency check is performed.
                SetLegacyFormat(static_cast<sampleFormat>(nValue));
            }
        } // while
        return true;
    }

    return false;
}

void WaveTrack::HandleXMLEndTag(const std::string_view& WXUNUSED(tag))
{
#if 0
    // In case we opened a pre-multiclip project, we need to
    // simulate closing the waveclip tag.
    NewestOrNewClip()->HandleXMLEndTag(WaveClip::WaveClip_tag);
#else
    // File compatibility breaks have intervened long since, and the line above
    // would now have undesirable side effects
#endif
}

XMLTagHandler* WaveTrack::HandleXMLChild(const std::string_view& tag)
{
    if (auto pChild = WaveTrackIORegistry::Get().CallObjectAccessor(tag, *this)) {
        // Deserialize any extra attached structures
        return pChild;
    }

    const auto getClip = [this]() -> WaveClip& {
        return (*NewestOrNewClip()->Channels().begin())->GetClip();
    };

    //
    // This is legacy code (1.2 and previous) and is not called for new projects!
    if (tag == Sequence::Sequence_tag || tag == "envelope") {
        // This is a legacy project, so set the cached offset
        NewestOrNewClip()->SetSequenceStartTime(mLegacyProjectFileOffset);

        // Legacy project file tracks are imported as one single wave clip
        if (tag == Sequence::Sequence_tag) {
            return getClip().GetSequence(0);
        } else if (tag == "envelope") {
            return &getClip().GetEnvelope();
        }
    }

    // JKC... for 1.1.0, one step better than what we had, but still badly broken.
    // If we see a waveblock at this level, we'd better generate a sequence.
    if (tag == Sequence::WaveBlock_tag) {
        // This is a legacy project, so set the cached offset
        NewestOrNewClip()->SetSequenceStartTime(mLegacyProjectFileOffset);
        auto pSeq = getClip().GetSequence(0);
        return pSeq;
    }

    // This is for the new file format (post-1.2)
    if (tag == WaveClip::WaveClip_tag) {
        // Make clips (which don't serialize the rate) consistent with channel rate,
        // though the consistency check of channels with each other remains to do.
        // Not all `WaveTrackData` fields are properly initialized by now,
        // use deserialization helpers.
        auto clip = WaveClip::NewShared(1, mpFactory, mLegacyFormat, mLegacyRate);
        const auto xmlHandler = clip.get();
        auto& clips = NarrowClips();
        clips.push_back(std::move(clip));
        Publish({ clips.back(), WaveTrackMessage::Deserialized });
        return xmlHandler;
    }

    return nullptr;
}

void WaveTrack::WriteXML(XMLWriter& xmlFile) const
// may throw
{
    const auto channels = Channels();
    size_t iChannel = 0,
           nChannels = channels.size();
    for (const auto pChannel : channels) {
        WriteOneXML(*pChannel, xmlFile, iChannel++, nChannels);
    }
}

void WaveTrack::WriteOneXML(const WaveChannel& channel, XMLWriter& xmlFile,
                            size_t iChannel, size_t nChannels)
// may throw
{
    // Track data has always been written using channel-major iteration.
    // Do it still this way for compatibility.

    // Some values don't vary independently in channels but have been written
    // redundantly for each channel.  Keep doing this in 3.4 and later in case
    // a project is opened in an earlier version.

    xmlFile.StartTag(WaveTrack_tag);
    auto& track = channel.GetTrack();

    // Name, selectedness, etc. are channel group properties
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
        xmlFile.WriteAttr(Channel_attr, channelType);
    }

    // The "linked" flag is used to define the beginning of a channel group
    // that isn't mono
    const auto linkType = static_cast<int>(
        (iChannel == 0) && (nChannels == 2)
        ? LinkType::Aligned
        : LinkType::None);
    xmlFile.WriteAttr(Linked_attr, linkType);

    // VS: trying to save tracks that didn't pass all necessary
    // initializations on project read from the disk.
    const auto useLegacy = track.mLegacyRate != 0;

    // More channel group properties written redundantly
    track.WritableSampleTrack::WriteXMLAttributes(xmlFile);
    xmlFile.WriteAttr(Rate_attr, useLegacy ? track.mLegacyRate : track.GetRate());
    xmlFile.WriteAttr(Volume_attr, static_cast<double>(track.GetVolume()));
    xmlFile.WriteAttr(Pan_attr, static_cast<double>(track.GetPan()));
    xmlFile.WriteAttr(SampleFormat_attr, static_cast<long>(useLegacy ? track.mLegacyFormat : track.GetSampleFormat()));

    // Other persistent data specified elsewhere;
    // NOT written redundantly any more
    if (iChannel == 0) {
        WaveTrackIORegistry::Get().CallWriters(track, xmlFile);
    }

    for (const auto& clip : channel.Intervals()) {
        clip->WriteXML(xmlFile);
    }

    xmlFile.EndTag(WaveTrack_tag);
}

std::optional<TranslatableString> WaveTrack::GetErrorOpening() const
{
    for (const auto& pClip : Intervals()) {
        const auto width = pClip->NChannels();
        for (size_t ii = 0; ii < width; ++ii) {
            if (pClip->GetSequence(ii)->GetErrorOpening()) {
                return XO("A track has a corrupted sample sequence.");
            }
        }
    }

    return {};
}

auto WaveTrack::GetLeftmostClip() -> IntervalHolder
{
    auto clips = Intervals();
    if (clips.empty()) {
        return nullptr;
    }
    const auto begin = clips.begin(),
               iter = std::min_element(begin, clips.end(),
                                       [](const auto& a, const auto b) {
        return a->GetPlayStartTime() < b->GetPlayStartTime();
    });
    return GetClip(std::distance(begin, iter));
}

auto WaveTrack::GetLeftmostClip() const -> IntervalConstHolder
{
    return const_cast<WaveTrack&>(*this).GetLeftmostClip();
}

auto WaveTrack::GetRightmostClip() -> IntervalHolder
{
    auto clips = Intervals();
    if (clips.empty()) {
        return nullptr;
    }
    const auto begin = clips.begin(),
               iter = std::max_element(begin, clips.end(),
                                       [](const auto& a, const auto b) {
        return a->GetPlayEndTime() < b->GetPlayEndTime();
    });
    return GetClip(std::distance(begin, iter));
}

auto WaveTrack::GetRightmostClip() const -> IntervalConstHolder
{
    return const_cast<WaveTrack&>(*this).GetRightmostClip();
}

ClipConstHolders WaveTrack::GetClipInterfaces() const
{
    auto clips = Intervals();
    return { clips.begin(), clips.end() };
}

double WaveChannel::GetStartTime() const
{
    return GetTrack().GetStartTime();
}

double WaveTrack::GetStartTime() const
{
    return ChannelGroup::GetStartTime();
}

double WaveChannel::GetEndTime() const
{
    return GetTrack().GetEndTime();
}

double WaveTrack::GetEndTime() const
{
    return ChannelGroup::GetEndTime();
}

bool WaveChannel::DoGet(size_t iChannel, size_t nBuffers,
                        const samplePtr buffers[], sampleFormat format,
                        sampleCount start, size_t len, bool backwards, fillFormat fill,
                        bool mayThrow, sampleCount* pNumWithinClips) const
{
    // These two assertions still remain after the great wide wave track and clip
    // refactoring!
    assert(iChannel == 0);
    assert(nBuffers <= 1);
    return GetTrack().DoGet(GetChannelIndex(), std::min<size_t>(nBuffers, 1),
                            buffers, format, start, len, backwards, fill, mayThrow, pNumWithinClips);
}

//
// Getting/setting samples.  The sample counts here are
// expressed relative to t=0.0 at the track's sample rate.
//

bool WaveTrack::DoGet(size_t iChannel, size_t nBuffers,
                      const samplePtr buffers[], sampleFormat format,
                      sampleCount start, size_t len, bool backwards, fillFormat fill,
                      bool mayThrow, sampleCount* pNumWithinClips) const
{
    const auto nChannels = NChannels();
    assert(iChannel + nBuffers <= nChannels); // precondition
    return std::all_of(buffers, buffers + nBuffers, [&](samplePtr buffer) {
        const auto result = GetOne(mClips, iChannel++,
                                   buffer, format, start, len, backwards, fill, mayThrow,
                                   pNumWithinClips);
        return result;
    });
}

bool WaveTrack::GetOne(const WaveClipHolders& clips, size_t iChannel,
                       samplePtr buffer, sampleFormat format, sampleCount start, size_t len,
                       bool backwards, fillFormat fill, bool mayThrow,
                       sampleCount* pNumWithinClips) const
{
    if (backwards) {
        start -= len;
    }
    // Simple optimization: When this buffer is completely contained within one clip,
    // don't clear anything (because we won't have to). Otherwise, just clear
    // everything to be on the safe side.
    bool doClear = true;
    bool result = true;
    sampleCount samplesCopied = 0;
    for (const auto& clip: clips) {
        if (start >= clip->GetPlayStartSample() && start + len <= clip->GetPlayEndSample()) {
            doClear = false;
            break;
        }
    }
    if (doClear) {
        // Usually we fill in empty space with zero
        if (fill == FillFormat::fillZero) {
            ClearSamples(buffer, format, 0, len);
        }
        // but we don't have to.
        else if (fill == FillFormat::fillTwo) {
            wxASSERT(format == floatSample);
            float* pBuffer = (float*)buffer;
            for (size_t i=0; i < len; i++) {
                pBuffer[i]=2.0f;
            }
        } else {
            wxFAIL_MSG(wxT("Invalid fill format"));
        }
    }

    // Iterate the clips.  They are not necessarily sorted by time.
    for (const auto& clip: clips) {
        auto clipStart = clip->GetPlayStartSample();
        auto clipEnd = clip->GetPlayEndSample();

        if (clipEnd > start && clipStart < start + len) {
            if (clip->HasPitchOrSpeed()) {
                return false;
            }

            // Clip sample region and Get/Put sample region overlap
            auto samplesToCopy
                =std::min(start + len - clipStart, clip->GetVisibleSampleCount());
            auto startDelta = clipStart - start;
            decltype(startDelta) inclipDelta = 0;
            if (startDelta < 0) {
                inclipDelta = -startDelta; // make positive value
                samplesToCopy -= inclipDelta;
                // samplesToCopy is now either len or
                //    (clipEnd - clipStart) - (start - clipStart)
                //    == clipEnd - start > 0
                // samplesToCopy is not more than len
                //
                startDelta = 0;
                // startDelta is zero
            } else {
                // startDelta is nonnegative and less than len
                // samplesToCopy is positive and not more than len
            }

            if (!clip->GetSamples(iChannel,
                                  (samplePtr)(((char*)buffer)
                                              + startDelta.as_size_t()
                                              * SAMPLE_SIZE(format)),
                                  format, inclipDelta, samplesToCopy.as_size_t(), mayThrow)) {
                result = false;
            } else {
                samplesCopied += samplesToCopy;
            }
        }
    }
    if (pNumWithinClips) {
        *pNumWithinClips = samplesCopied;
    }
    if (result == true && backwards) {
        ReverseSamples(buffer, format, 0, len);
    }
    return result;
}

ChannelGroupSampleView
WaveTrack::GetSampleView(double t0, double t1, bool mayThrow) const
{
    ChannelGroupSampleView result;
    for (const auto& channel : Channels()) {
        result.push_back(channel->GetSampleView(t0, t1, mayThrow));
    }
    return result;
}

ChannelSampleView
WaveChannel::GetSampleView(double t0, double t1, bool mayThrow) const
{
    std::vector<std::shared_ptr<const WaveClipChannel> >
    intersectingIntervals;
    for (const auto& interval : Intervals()) {
        if (interval->Intersects(t0, t1)) {
            intersectingIntervals.push_back(interval);
        }
    }
    if (intersectingIntervals.empty()) {
        return { AudioSegmentSampleView { (TimeToLongSamples(t1) - TimeToLongSamples(t0)).as_size_t() } };
    }
    std::sort(
        intersectingIntervals.begin(), intersectingIntervals.end(),
        [](const auto& a, const auto& b) { return a->Start() < b->Start(); });
    std::vector<AudioSegmentSampleView> segments;
    segments.reserve(2 * intersectingIntervals.size() + 1);
    for (auto i = 0u; i < intersectingIntervals.size(); ++i) {
        const auto& interval = intersectingIntervals[i];
        const auto intervalStartTime = interval->Start();
        if (t0 < intervalStartTime) {
            const auto numSamples = TimeToLongSamples(intervalStartTime - t0);
            segments.push_back(AudioSegmentSampleView { numSamples.as_size_t() });
            t0 = intervalStartTime;
        }
        const auto intervalT0 = t0 - intervalStartTime;
        const auto intervalT1 = std::min(t1, interval->End()) - intervalStartTime;
        if (intervalT1 > intervalT0) {
            auto newSegment
                =interval->GetSampleView(intervalT0, intervalT1, mayThrow);
            t0 += intervalT1 - intervalT0;
            segments.push_back(std::move(newSegment));
        }
        if (t0 == t1) {
            break;
        }
    }
    if (t0 < t1) {
        segments.push_back(AudioSegmentSampleView {
            (TimeToLongSamples(t1) - TimeToLongSamples(t0)).as_size_t() });
    }
    return segments;
}

/*! @excsafety{Weak} */
bool WaveChannel::Set(constSamplePtr buffer, sampleFormat format,
                      sampleCount start, size_t len, sampleFormat effectiveFormat)
{
    for (const auto& clip: Intervals()) {
        auto clipStart = clip->GetPlayStartSample();
        auto clipEnd = clip->GetPlayEndSample();

        if (clipEnd > start && clipStart < start + len) {
            // Test as also in WaveTrack::GetOne()
            if (clip->HasPitchOrSpeed()) {
                return false;
            }

            // Clip sample region and Get/Put sample region overlap
            auto samplesToCopy
                =std::min(start + len - clipStart, clip->GetVisibleSampleCount());
            auto startDelta = clipStart - start;
            decltype(startDelta) inclipDelta = 0;
            if (startDelta < 0) {
                inclipDelta = -startDelta; // make positive value
                samplesToCopy -= inclipDelta;
                // samplesToCopy is now either len or
                //    (clipEnd - clipStart) - (start - clipStart)
                //    == clipEnd - start > 0
                // samplesToCopy is not more than len
                //
                startDelta = 0;
                // startDelta is zero
            } else {
                // startDelta is nonnegative and less than len
                // samplesToCopy is positive and not more than len
            }

            clip->SetSamples(
                buffer + startDelta.as_size_t() * SAMPLE_SIZE(format),
                format, inclipDelta, samplesToCopy.as_size_t(), effectiveFormat);
        }
    }
    return true;
}

sampleFormat WaveChannel::WidestEffectiveFormat() const
{
    return GetTrack().WidestEffectiveFormat();
}

sampleFormat WaveTrack::WidestEffectiveFormat() const
{
    auto result = narrowestSampleFormat;
    for (const auto& pClip : Intervals()) {
        result = std::max(result, pClip->GetSampleFormats().Effective());
    }
    return result;
}

bool WaveChannel::HasTrivialEnvelope() const
{
    return GetTrack().HasTrivialEnvelope();
}

bool WaveTrack::HasTrivialEnvelope() const
{
    auto pTrack = this;
    if (!pTrack) {
        return false;
    }
    auto clips = pTrack->Intervals();
    return std::all_of(clips.begin(), clips.end(),
                       [](const auto& pClip){ return pClip->GetEnvelope().IsTrivial(); });
}

void WaveChannel::GetEnvelopeValues(
    double* buffer, size_t bufferLen, double t0, bool backwards) const
{
    return GetTrack().GetEnvelopeValues(buffer, bufferLen, t0, backwards);
}

void WaveTrack::GetEnvelopeValues(
    double* buffer, size_t bufferLen, double t0, bool backwards) const
{
    auto pTrack = this;
    if (!pTrack) {
        return;
    }

    if (backwards) {
        t0 -= bufferLen / pTrack->GetRate();
    }
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
    for (decltype(bufferLen) i = 0; i < bufferLen; i++) {
        buffer[i] = 1.0;
    }

    double startTime = t0;
    const auto rate = pTrack->GetRate();
    auto tstep = 1.0 / rate;
    double endTime = t0 + tstep * bufferLen;
    for (const auto& clip: pTrack->Intervals()) {
        // IF clip intersects startTime..endTime THEN...
        auto dClipStartTime = clip->GetPlayStartTime();
        auto dClipEndTime = clip->GetPlayEndTime();
        if ((dClipStartTime < endTime) && (dClipEndTime > startTime)) {
            auto rbuf = buffer;
            auto rlen = bufferLen;
            auto rt0 = t0;

            if (rt0 < dClipStartTime) {
                // This is not more than the number of samples in
                // (endTime - startTime) which is bufferLen:
                auto nDiff = (sampleCount)floor((dClipStartTime - rt0) * rate + 0.5);
                auto snDiff = nDiff.as_size_t();
                rbuf += snDiff;
                wxASSERT(snDiff <= rlen);
                rlen -= snDiff;
                rt0 = dClipStartTime;
            }

            if (rt0 + rlen * tstep > dClipEndTime) {
                auto nClipLen = clip->GetPlayEndSample() - clip->GetPlayStartSample();

                if (nClipLen <= 0) { // Testing for bug 641, this problem is consistently '== 0', but doesn't hurt to check <.
                    return;
                }

                // This check prevents problem cited in http://bugzilla.audacityteam.org/show_bug.cgi?id=528#c11,
                // Gale's cross_fade_out project, which was already corrupted by bug 528.
                // This conditional prevents the previous write past the buffer end, in clip->GetEnvelope() call.
                // Never increase rlen here.
                // PRL bug 827:  rewrote it again
                rlen = limitSampleBufferSize(rlen, nClipLen);
                rlen = std::min(rlen, size_t(floor(0.5 + (dClipEndTime - rt0) / tstep)));
            }
            // Samples are obtained for the purpose of rendering a wave track,
            // so quantize time
            clip->GetEnvelope().GetValues(rbuf, rlen, rt0, tstep);
        }
    }
    if (backwards) {
        std::reverse(buffer, buffer + bufferLen);
    }
}

// When the time is both the end of a clip and the start of the next clip, the
// latter clip is returned.
auto WaveTrack::GetClipAtTime(double time) const -> IntervalConstHolder
{
    const auto clips = SortedClipArray();
    auto p = std::find_if(
        clips.rbegin(), clips.rend(), [&](const auto& pClip) {
        return pClip->WithinPlayRegion(time);
    });
    return p != clips.rend() ? *p : nullptr;
}

WaveTrack::IntervalConstHolders
WaveTrack::GetSortedClipsIntersecting(double t0, double t1) const
{
    assert(t0 <= t1);
    WaveTrack::IntervalConstHolders result;
    const auto clips = SortedClipArray();
    std::copy_if(
        clips.begin(), clips.end(), back_inserter(result),
        [&](const auto& pClip) { return pClip->IntersectsPlayRegion(t0, t1); });
    return result;
}

auto WaveTrack::CreateClip(double offset, const wxString& name,
                           const Interval* pToCopy, bool copyCutlines) -> IntervalHolder
{
    if (pToCopy) {
        constexpr auto backup = false;
        auto pNewClip
            =WaveClip::NewSharedFrom(*pToCopy, mpFactory, copyCutlines, backup);
        pNewClip->SetName(name);
        pNewClip->SetSequenceStartTime(offset);
        return pNewClip;
    } else {
        return DoCreateClip(offset, name);
    }
}

auto WaveTrack::CopyClip(const Interval& toCopy, bool copyCutlines)
-> IntervalHolder
{
    return CreateClip(toCopy.GetSequenceStartTime(),
                      toCopy.GetName(), &toCopy, copyCutlines);
}

void WaveTrack::CreateRight()
{
    mRightChannel.emplace(*this);
}

auto WaveTrack::DoCreateClip(double offset, const wxString& name) const
-> WaveClipHolder
{
    auto clip = WaveClip::NewShared(NChannels(), mpFactory, GetSampleFormat(), GetRate());
    clip->SetName(name);
    clip->SetSequenceStartTime(offset);

    const auto& tempo = GetProjectTempo(*this);
    if (tempo.has_value()) {
        clip->OnProjectTempoChange(std::nullopt, *tempo);
    }
    assert(clip->NChannels() == NChannels());
    return clip;
}

auto WaveTrack::NewestOrNewClip() -> IntervalHolder
{
    const auto& intervals = Intervals();
    if (intervals.empty()) {
        const auto origin = WaveTrackData::Get(*this).GetOrigin();
        const auto name = MakeNewClipName();
        auto pInterval = CreateClip(origin, name);
        InsertInterval(pInterval, true, true);
        return pInterval;
    } else {
        return mClips.back();
    }
}

/*! @excsafety{No-fail} */
auto WaveTrack::RightmostOrNewClip() -> IntervalHolder
{
    if (mClips.empty()) {
        auto pInterval = CreateClip(
            WaveTrackData::Get(*this).GetOrigin());
        InsertInterval(pInterval, true, true);
        return pInterval;
    } else {
        auto end = mClips.end(),
             it = max_element(mClips.begin(), end,
                              [](const auto& pClip1, const auto& pClip2){
            return pClip1->GetPlayStartTime() < pClip2->GetPlayStartTime();
        });
        assert(it != end);
        return *it;
    }
}

// For internal purposes only
int WaveTrack::GetClipIndex(const Interval& clip) const
{
    int result = 0;
    const auto& clips = Intervals();
    const auto test
        =[&](const auto& pOtherClip){ return &clip == pOtherClip.get(); };
    auto begin = clips.begin(),
         end = clips.end(),
         iter = std::find_if(begin, end, test);
    return std::distance(begin, iter);
}

int WaveTrack::GetNumClips() const
{
    return NarrowClips().size();
}

bool WaveTrack::CanOffsetClips(
    const std::vector<Interval*>& movingClips,
    double amount,
    double* allowedAmount /* = NULL */)
{
    if (allowedAmount) {
        *allowedAmount = amount;
    }

    const auto& moving = [&](Interval* clip){
        // linear search might be improved, but expecting few moving clips
        // compared with the fixed clips
        return movingClips.end()
               != std::find(movingClips.begin(), movingClips.end(), clip);
    };

    for (const auto& c: Intervals()) {
        if (moving(c.get())) {
            continue;
        }
        for (const auto clip : movingClips) {
            if (c->GetPlayStartTime() < clip->GetPlayEndTime() + amount
                && c->GetPlayEndTime() > clip->GetPlayStartTime() + amount) {
                if (!allowedAmount) {
                    return false; // clips overlap
                }
                if (amount > 0) {
                    if (c->GetPlayStartTime() - clip->GetPlayEndTime() < *allowedAmount) {
                        *allowedAmount = c->GetPlayStartTime() - clip->GetPlayEndTime();
                    }
                    if (*allowedAmount < 0) {
                        *allowedAmount = 0;
                    }
                } else {
                    if (c->GetPlayEndTime() - clip->GetPlayStartTime() > *allowedAmount) {
                        *allowedAmount = c->GetPlayEndTime() - clip->GetPlayStartTime();
                    }
                    if (*allowedAmount > 0) {
                        *allowedAmount = 0;
                    }
                }
            }
        }
    }

    if (allowedAmount) {
        if (*allowedAmount == amount) {
            return true;
        }

        // Check if the NEW calculated amount would not violate
        // any other constraint
        if (!CanOffsetClips(movingClips, *allowedAmount, nullptr)) {
            *allowedAmount = 0; // play safe and don't allow anything
            return false;
        } else {
            return true;
        }
    } else {
        return true;
    }
}

bool WaveTrack::CanInsertClip(
    const Interval& candidateClip, double& slideBy, double tolerance) const
{
    const auto& clips = Intervals();
    if (clips.empty()) {
        return true;
    }
    // Find clip in this that overlaps most with `clip`:
    const auto candidateClipStartTime = candidateClip.GetPlayStartTime();
    const auto candidateClipEndTime = candidateClip.GetPlayEndTime();
    const auto t0 = SnapToSample(candidateClipStartTime + slideBy);
    const auto t1 = SnapToSample(candidateClipEndTime + slideBy);
    std::vector<double> overlaps;
    std::transform(
        clips.begin(), clips.end(), std::back_inserter(overlaps),
        [&](const auto& pClip) {
        return pClip->IntersectsPlayRegion(t0, t1)
               ? std::min(pClip->GetPlayEndTime(), t1)
               - std::max(pClip->GetPlayStartTime(), t0)
               : 0.0;
    });
    const auto maxOverlap = std::max_element(overlaps.begin(), overlaps.end());
    if (*maxOverlap > tolerance) {
        return false;
    }
    auto iter = clips.begin();
    std::advance(iter, std::distance(overlaps.begin(), maxOverlap));
    const auto overlappedClip = *iter;
    const auto requiredOffset =  slideBy
                                + *maxOverlap * (overlappedClip->GetPlayStartTime() < t0 ? 1 : -1);
    // Brute-force check to see if there's another clip that'd be in the way.
    if (std::any_of(
            clips.begin(), clips.end(),
            [&](const auto& pClip)
    {
        const auto result = pClip->IntersectsPlayRegion(
            SnapToSample(candidateClipStartTime + requiredOffset),
            SnapToSample(candidateClipEndTime + requiredOffset));
        return result;
    })) {
        return false;
    }
    slideBy = requiredOffset;
    return true;
}

/*! @excsafety{Weak} */
void WaveTrack::Split(double t0, double t1)
{
    SplitAt(t0);
    if (t0 != t1) {
        SplitAt(t1);
    }
}

/*! @excsafety{Weak} */
auto WaveTrack::SplitAt(double t) -> std::pair<IntervalHolder, IntervalHolder>
{
    for (const auto&& c : Intervals()) {
        if (c->SplitsPlayRegion(t)) {
            t = SnapToSample(t);
            auto newClip = CopyClip(*c, true);
            c->TrimRightTo(t);// put t on a sample
            newClip->TrimLeftTo(t);
            auto result = std::pair{ c, newClip };

            // This could invalidate the iterators for the loop!  But we return
            // at once so it's okay
            InsertInterval(move(newClip), false); // transfer ownership
            return result;
        }
    }
    return {};
}

// Can't promise strong exception safety for a pair of tracks together
bool WaveTrack::MergeClips(int clipidx1, int clipidx2)
{
    const auto clip1 = GetClip(clipidx1);
    const auto clip2 = GetClip(clipidx2);

    if (!clip1 || !clip2) {
        return false; // Don't throw, just do nothing.
    }
    if (!clip1->HasEqualPitchAndSpeed(*clip2)) {
        return false;
    }

    // Append data from second clip to first clip
    // use Strong-guarantee
    bool success = clip1->Paste(clip1->GetPlayEndTime(), *clip2);
    assert(success); // assuming clips of the same track must have same width

    // use No-fail-guarantee for the rest
    // Delete second clip
    RemoveInterval(clip2);
    return true;
}

void WaveTrack::ApplyPitchAndSpeedOnIntervals(
    const IntervalHolders& srcIntervals,
    const ProgressReporter& reportProgress)
{
    IntervalHolders dstIntervals;
    dstIntervals.reserve(srcIntervals.size());
    std::transform(
        srcIntervals.begin(), srcIntervals.end(),
        std::back_inserter(dstIntervals), [&](const IntervalHolder& interval) {
        return GetRenderedCopy(interval,
                               reportProgress, mpFactory, GetSampleFormat());
    });

    // If we reach this point it means that no error was thrown - we can replace
    // the source with the destination intervals.
    for (auto i = 0; i < srcIntervals.size(); ++i) {
        ReplaceInterval(srcIntervals[i], dstIntervals[i]);
    }
}

namespace {
bool ClipsAreUnique(const WaveClipHolders& clips)
{
    // This is used only in assertions
    using Set = std::unordered_set<WaveClipHolder>;
    return clips.size() == Set{ clips.begin(), clips.end() }.size();
}
}

void WaveTrack::InsertInterval(const IntervalHolder& clip,
                               bool newClip, bool allowEmpty)
{
    if (clip) {
        constexpr bool backup = false;
        InsertClip(mClips, clip, newClip, backup, allowEmpty);
        // Detect errors resulting in duplicate shared pointers to clips
        assert(ClipsAreUnique(mClips));
    }
}

void WaveTrack::RemoveInterval(const IntervalHolder& interval)
{
    const auto end = mClips.end(),
               iter = find(mClips.begin(), end, interval);
    if (iter != end) {
        mClips.erase(iter);
    }
}

void WaveTrack::ReplaceInterval(
    const IntervalHolder& oldOne, const IntervalHolder& newOne)
{
    assert(newOne == oldOne || FindClip(*newOne) == Intervals().size());
    assert(oldOne->NChannels() == newOne->NChannels());
    RemoveInterval(oldOne);
    InsertInterval(newOne, false);
    newOne->SetName(oldOne->GetName());
}

/*! @excsafety{Weak} -- Partial completion may leave clips at differing sample rates!
*/
void WaveTrack::Resample(int rate, BasicUI::ProgressDialog* progress)
{
    for (const auto& pClip : Intervals()) {
        pClip->Resample(rate, progress);
    }
    DoSetRate(rate);
}

bool WaveTrack::SetFloats(const float* const* buffers,
                          sampleCount start, size_t len, sampleFormat effectiveFormat)
{
    bool result = true;
    size_t ii = 0;
    for (const auto& pChannel : Channels()) {
        const auto buffer = buffers[ii++];
        assert(buffer); // precondition
        result = pChannel->SetFloats(buffer, start, len, effectiveFormat)
                 && result;
    }
    return result;
}

auto WaveTrack::SortedClipArray() const -> IntervalConstHolders
{
    const auto& intervals = Intervals();
    IntervalConstHolders clips{ intervals.begin(), intervals.end() };
    const auto comp = [](const auto& a, const auto& b) {
        return a->GetPlayStartTime() < b->GetPlayStartTime();
    };
    std::sort(clips.begin(), clips.end(), comp);
    return clips;
}

auto WaveTrack::SortedIntervalArray() -> IntervalHolders
{
    const auto& intervals = Intervals();
    IntervalHolders result;
    copy(intervals.begin(), intervals.end(), back_inserter(result));
    sort(result.begin(), result.end(), [](const auto& pA, const auto& pB){
        return pA->GetPlayStartTime() < pB->GetPlayStartTime();
    });
    return result;
}

auto WaveTrack::SortedIntervalArray() const -> IntervalConstHolders
{
    const auto& intervals = Intervals();
    IntervalConstHolders result;
    copy(intervals.begin(), intervals.end(), back_inserter(result));
    sort(result.begin(), result.end(), [](const auto& pA, const auto& pB){
        return pA->GetPlayStartTime() < pB->GetPlayStartTime();
    });
    return result;
}

void WaveTrack::ZipClips(bool mustAlign)
{
    const auto pOwner = GetOwner();
    assert(GetOwner()); // pre
    assert(NChannels() == 1); // pre

    // If deserializing, first un-link the track, so iterator finds the partner.
    SetLinkType(LinkType::None);

    auto iter = pOwner->Find(this);
    assert(this == *iter);
    ++iter;
    assert(iter != pOwner->end()); // pre
    auto pRight = dynamic_cast<WaveTrack*>(*iter);
    assert(pRight && pRight->NChannels() == 1); // pre

    //! Refuse if clips are not well aligned.
    if (mustAlign
        && !AreAligned(this->SortedClipArray(), pRight->SortedClipArray())) {
        return;
    }

    CreateRight();

    // Now steal right side sample data info.  When not requiring alignment,
    // because this is a track that just keeps the sample counts of blocks
    // above 0 for later purposes -- then there is laxity about consistent
    // width of the clips.
    auto iterMe = mClips.begin(),
         endMe = mClips.end();
    auto iterRight = pRight->mClips.begin(),
         endRight = pRight->mClips.end();
    while (iterMe != endMe && iterRight != endRight) {
        (*iterMe)->MakeStereo(std::move(**iterRight), mustAlign);
        ++iterMe;
        ++iterRight;
    }
    assert(!mustAlign || (iterMe == endMe && iterRight == endRight));

    while (iterRight != endRight) {
        // Leftover misaligned mono clips
        mClips.emplace_back(move(*iterRight));
        ++iterRight;
    }

    this->MergeChannelAttachments(std::move(*pRight));

    pOwner->Remove(*pRight);
}

static auto TrackFactoryFactory = []( AudacityProject& project ) {
    return std::make_shared< WaveTrackFactory >(
        ProjectRate::Get(project),
        SampleBlockFactory::New(project));
};

static const AudacityProject::AttachedObjects::RegisteredFactory key2{
    TrackFactoryFactory
};

WaveTrackFactory& WaveTrackFactory::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< WaveTrackFactory >(key2);
}

const WaveTrackFactory& WaveTrackFactory::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

WaveTrackFactory& WaveTrackFactory::Reset(AudacityProject& project)
{
    auto result = TrackFactoryFactory(project);
    project.AttachedObjects::Assign(key2, result);
    return *result;
}

void WaveTrackFactory::Destroy(AudacityProject& project)
{
    project.AttachedObjects::Assign(key2, nullptr);
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
    if (mIsSyncLocked) {
        return true;
    }
    bool editClipsCanMove;
    return EditClipsCanMove.Read();
}

BoolSetting EditClipsCanMove{
    L"/GUI/EditClipCanMove",         false };

DEFINE_XML_METHOD_REGISTRY(WaveTrackIORegistry);
