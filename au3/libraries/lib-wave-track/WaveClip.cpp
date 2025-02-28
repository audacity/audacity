/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveClip.cpp

  ?? Dominic Mazzoni
  ?? Markus Meyer

*******************************************************************//**

\class WaveClip
\brief This allows multiple clips to be a part of one WaveTrack.

*//*******************************************************************/
#include "WaveClip.h"

#include <math.h>
#include <numeric>
#include <optional>
#include <vector>
#include <wx/log.h>

#include "BasicUI.h"
#include "Envelope.h"
#include "InconsistencyException.h"
#include "Resample.h"
#include "Sequence.h"
#include "TimeAndPitchInterface.h"
#include "UserException.h"

#include "global/realfn.h"

#ifdef _OPENMP
#include <omp.h>
#endif

const char* WaveClip::WaveClip_tag = "waveclip";

WaveClipListener::~WaveClipListener() = default;

void WaveClipListener::WriteXMLAttributes(XMLWriter&) const
{
}

bool WaveClipListener::HandleXMLAttribute(
    const std::string_view&, const XMLAttributeValueView&)
{
    return false;
}

void WaveClipListener::MakeStereo(WaveClipListener&&, bool)
{
}

void WaveClipListener::SwapChannels()
{
}

void WaveClipListener::Erase(size_t)
{
}

WaveClipChannel::~WaveClipChannel() = default;

Envelope& WaveClipChannel::GetEnvelope()
{
    return GetClip().GetEnvelope();
}

const Envelope& WaveClipChannel::GetEnvelope() const
{
    return GetClip().GetEnvelope();
}

bool WaveClipChannel::Intersects(double t0, double t1) const
{
    return GetClip().IntersectsPlayRegion(t0, t1);
}

double WaveClipChannel::Start() const
{
    return GetClip().GetPlayStartTime();
}

double WaveClipChannel::End() const
{
    return GetClip().GetPlayEndTime();
}

AudioSegmentSampleView
WaveClipChannel::GetSampleView(double t0, double t1, bool mayThrow) const
{
    return GetClip().GetSampleView(miChannel, t0, t1, mayThrow);
}

bool WaveClipChannel::WithinPlayRegion(double t) const
{
    return GetClip().WithinPlayRegion(t);
}

double WaveClipChannel::SamplesToTime(sampleCount s) const noexcept
{
    return GetClip().SamplesToTime(s);
}

bool WaveClipChannel::HasPitchOrSpeed() const
{
    return GetClip().HasPitchOrSpeed();
}

double WaveClipChannel::GetTrimLeft() const
{
    return GetClip().GetTrimLeft();
}

bool WaveClipChannel::GetSamples(samplePtr buffer, sampleFormat format,
                                 sampleCount start, size_t len, bool mayThrow) const
{
    return GetClip().GetSamples(miChannel, buffer, format, start, len, mayThrow);
}

AudioSegmentSampleView WaveClipChannel::GetSampleView(
    sampleCount start, size_t length, bool mayThrow) const
{
    return GetClip().GetSampleView(miChannel, start, length, mayThrow);
}

const Sequence& WaveClipChannel::GetSequence() const
{
    const auto pSequence = GetClip().GetSequence(miChannel);
    // Assume sufficiently wide clip
    assert(pSequence);
    return *pSequence;
}

constSamplePtr WaveClipChannel::GetAppendBuffer() const
{
    return GetClip().GetAppendBuffer(miChannel);
}

size_t WaveClipChannel::GetAppendBufferLen() const
{
    return GetClip().GetAppendBufferLen(miChannel);
}

const BlockArray* WaveClipChannel::GetSequenceBlockArray() const
{
    return GetClip().GetSequenceBlockArray(miChannel);
}

std::pair<float, float>
WaveClipChannel::GetMinMax(double t0, double t1, bool mayThrow) const
{
    return GetClip().GetMinMax(miChannel, t0, t1, mayThrow);
}

float WaveClipChannel::GetRMS(double t0, double t1, bool mayThrow) const
{
    return GetClip().GetRMS(miChannel, t0, t1, mayThrow);
}

sampleCount WaveClipChannel::GetPlayStartSample() const
{
    return GetClip().GetPlayStartSample();
}

sampleCount WaveClipChannel::GetPlayEndSample() const
{
    return GetClip().GetPlayEndSample();
}

void WaveClipChannel::SetSamples(constSamplePtr buffer, sampleFormat format,
                                 sampleCount start, size_t len, sampleFormat effectiveFormat)
{
    return GetClip().SetSamples(miChannel,
                                buffer, format, start, len, effectiveFormat);
}

void WaveClipChannel::WriteXML(XMLWriter& xmlFile) const
{
    GetClip().WriteXML(miChannel, xmlFile);
}

double WaveClipChannel::GetTrimRight() const
{
    return GetClip().GetTrimRight();
}

sampleCount WaveClipChannel::GetVisibleSampleCount() const
{
    return GetClip().GetVisibleSampleCount();
}

int WaveClipChannel::GetRate() const
{
    return GetClip().GetRate();
}

double WaveClipChannel::GetPlayStartTime() const
{
    return GetClip().GetPlayStartTime();
}

double WaveClipChannel::GetPlayEndTime() const
{
    return GetClip().GetPlayEndTime();
}

double WaveClipChannel::GetPlayDuration() const
{
    return GetPlayEndTime() - GetPlayStartTime();
}

sampleCount WaveClipChannel::TimeToSamples(double time) const
{
    return GetClip().TimeToSamples(time);
}

double WaveClipChannel::GetStretchRatio() const
{
    return GetClip().GetStretchRatio();
}

static int64_t s_lastClipId = 0;
int64_t WaveClip::NewID()
{
    return ++s_lastClipId;
}

WaveClip::WaveClip(size_t width,
                   const SampleBlockFactoryPtr& factory,
                   sampleFormat format, int rate)
{
    mId = ++s_lastClipId;
    assert(width > 0);
    mRate = rate;
    mSequences.resize(width);
    for (auto& pSequence : mSequences) {
        pSequence = std::make_unique<Sequence>(factory,
                                               SampleFormats { narrowestSampleFormat, format });
    }

    mEnvelope = std::make_unique<Envelope>(true, 1e-7, 2.0, 1.0);
    assert(CheckInvariants());
}

WaveClip::WaveClip(
    const WaveClip& orig, const SampleBlockFactoryPtr& factory,
    bool copyCutlines, CreateToken token)
    : mCentShift{orig.mCentShift}
    , mPitchAndSpeedPreset{orig.mPitchAndSpeedPreset}
    , mClipStretchRatio{orig.mClipStretchRatio}
    , mRawAudioTempo{orig.mRawAudioTempo}
    , mClipTempo{orig.mClipTempo}
{
    // essentially a copy constructor - but you must pass in the
    // current sample block factory, because we might be copying
    // from one project to another

    mId = orig.mId;
    mSequenceOffset = orig.mSequenceOffset;
    mTrimLeft = orig.mTrimLeft;
    mTrimRight = orig.mTrimRight;
    mRate = orig.mRate;
    mStretchToMatchProjectTempo = orig.mStretchToMatchProjectTempo;

    // Deep copy of attachments
    Attachments& attachments = *this;
    attachments = orig;

    mSequences.reserve(orig.NChannels());
    if (!token.emptyCopy) {
        for (auto& pSequence : orig.mSequences) {
            mSequences.push_back(std::make_unique<Sequence>(*pSequence, factory));
        }
    }

    mEnvelope = std::make_unique<Envelope>(*orig.mEnvelope);

    mName = orig.mName;

    if (copyCutlines) {
        mGroupId = orig.mGroupId;
    }
    for (const auto& clip: orig.mCutLines) {
        mCutLines.push_back(
            std::make_shared<WaveClip>(*clip, factory, true, token));
    }

    mIsPlaceholder = orig.GetIsPlaceholder();

    assert(NChannels() == (token.emptyCopy ? 0 : orig.NChannels()));
    assert(token.emptyCopy || CheckInvariants());
    assert(!copyCutlines || NumCutLines() == orig.NumCutLines());
}

WaveClip::WaveClip(
    const WaveClip& orig, const SampleBlockFactoryPtr& factory,
    bool copyCutlines, double t0, double t1)
    : mCentShift{orig.mCentShift}
    , mClipStretchRatio{orig.mClipStretchRatio}
    , mRawAudioTempo{orig.mRawAudioTempo}
    , mClipTempo{orig.mClipTempo}
{
    assert(orig.CountSamples(t0, t1) > 0);

    mId = orig.mId;
    mSequenceOffset = orig.mSequenceOffset;

    //Adjust trim values to sample-boundary
    if (t0 > orig.GetPlayStartTime()) {
        const auto s0 = orig.TimeToSamples(t0 - orig.GetSequenceStartTime());
        mTrimLeft = orig.SamplesToTime(s0);
    } else {
        mTrimLeft = orig.mTrimLeft;
    }

    if (t1 < orig.GetPlayEndTime()) {
        const auto s1 = orig.TimeToSamples(orig.GetSequenceEndTime() - t1);
        mTrimRight = orig.SamplesToTime(s1);
    } else {
        mTrimRight = orig.mTrimRight;
    }

    mRate = orig.mRate;
    mStretchToMatchProjectTempo = orig.mStretchToMatchProjectTempo;

    // Deep copy of attachments
    Attachments& attachments = *this;
    attachments = orig;

    mIsPlaceholder = orig.GetIsPlaceholder();

    mSequences.reserve(orig.NChannels());
    for (auto& pSequence : orig.mSequences) {
        mSequences.push_back(
            std::make_unique<Sequence>(*pSequence, factory));
    }

    mEnvelope = std::make_unique<Envelope>(*orig.mEnvelope);

    if (copyCutlines) {
        mGroupId = orig.mGroupId;
    }

    for (const auto& cutline : orig.mCutLines) {
        // IDs of cutline placeholder clips are unimportant - let's not
        // increment the ID counter for that.
        constexpr auto backup = true;
        mCutLines.push_back(
            WaveClip::NewSharedFrom(*cutline, factory, true, backup));
    }

    assert(NChannels() == orig.NChannels());
    assert(CheckInvariants());
}

WaveClip::~WaveClip()
{
    Observer::Publisher<WaveClipDtorCalled>::Publish(WaveClipDtorCalled {});
}

int64_t WaveClip::GetId() const
{
    return mId;
}

void WaveClip::SetId(int64_t id)
{
    mId = id;
}

double WaveClip::Start() const
{
    return GetPlayStartTime();
}

double WaveClip::End() const
{
    return GetPlayEndTime();
}

std::shared_ptr<ChannelInterval> WaveClip::DoGetChannel(size_t iChannel)
{
    return std::make_shared<Channel>(*this, iChannel);
}

AudioSegmentSampleView WaveClip::GetSampleView(
    size_t ii, sampleCount start, size_t length, bool mayThrow) const
{
    assert(ii < NChannels());
    return mSequences[ii]->GetFloatSampleView(
        start + TimeToSamples(mTrimLeft), length, mayThrow);
}

AudioSegmentSampleView WaveClip::GetSampleView(
    size_t iChannel, double t0, double t1, bool mayThrow) const
{
    assert(iChannel < NChannels());
    const auto start = TimeToSamples(std::max(0., t0));
    const auto length
        =(std::min(GetNumSamples(), TimeToSamples(t1)) - start).as_size_t();
    return GetSampleView(iChannel, start, length, mayThrow);
}

size_t WaveClip::NChannels() const
{
    return mSequences.size();
}

bool WaveClip::GetSamples(size_t ii,
                          samplePtr buffer, sampleFormat format,
                          sampleCount start, size_t len, bool mayThrow) const
{
    assert(ii < NChannels());
    return mSequences[ii]
           ->Get(buffer, format, start + TimeToSamples(mTrimLeft), len, mayThrow);
}

bool WaveClip::GetSamples(samplePtr buffers[], sampleFormat format,
                          sampleCount start, size_t len, bool mayThrow) const
{
    bool result = true;
    for (size_t ii = 0, width = NChannels(); result && ii < width; ++ii) {
        result = GetSamples(ii, buffers[ii], format, start, len, mayThrow);
    }
    return result;
}

/*! @excsafety{Strong} */
void WaveClip::SetSamples(size_t ii,
                          constSamplePtr buffer, sampleFormat format,
                          sampleCount start, size_t len, sampleFormat effectiveFormat)
{
    StrongInvariantScope scope{ *this };
    assert(ii < NChannels());
    // use Strong-guarantee
    mSequences[ii]->SetSamples(buffer, format,
                               start + TimeToSamples(mTrimLeft), len, effectiveFormat);

    // use No-fail-guarantee
    MarkChanged();
}

void WaveClip::SetEnvelope(std::unique_ptr<Envelope> p)
{
    assert(p);
    mEnvelope = move(p);
}

const BlockArray* WaveClip::GetSequenceBlockArray(size_t ii) const
{
    assert(ii < NChannels());
    return &mSequences[ii]->GetBlockArray();
}

size_t WaveClip::GetAppendBufferLen(size_t iChannel) const
{
    assert(iChannel < NChannels());
    return mSequences[iChannel]->GetAppendBufferLen();
}

void WaveClip::DiscardRightChannel()
{
    mSequences.resize(1);
    this->Attachments::ForEach([](WaveClipListener& attachment){
        attachment.Erase(1);
    });
    for (auto& pCutline : mCutLines) {
        pCutline->DiscardRightChannel();
    }
    assert(NChannels() == 1);
    assert(CheckInvariants());
}

void WaveClip::SwapChannels()
{
    assert(NChannels() == 2);
    this->Attachments::ForEach([](WaveClipListener& attachment){
        attachment.SwapChannels();
    });
    std::swap(mSequences[0], mSequences[1]);
    for (auto& pCutline : mCutLines) {
        pCutline->SwapChannels();
    }
    assert(CheckInvariants());
}

void WaveClip::TransferSequence(WaveClip& origClip, WaveClip& newClip)
{
    // Move right channel into result
    newClip.mSequences.resize(1);
    newClip.mSequences[0] = move(origClip.mSequences[1]);
    // Delayed satisfaction of the class invariants after the empty construction
    newClip.CheckInvariants();
}

void WaveClip::FixSplitCutlines(
    WaveClipHolders& myCutlines, WaveClipHolders& newCutlines)
{
    auto beginMe = myCutlines.begin(),
         endMe = myCutlines.end();
    auto iterNew = newCutlines.begin(),
         endNew = newCutlines.end();
    for_each(beginMe, endMe, [&](const auto& myCutline){
        assert(iterNew != endNew);
        const auto pNew = *iterNew;
        TransferSequence(*myCutline, *pNew);
        // Recursion!
        FixSplitCutlines(myCutline->mCutLines, pNew->mCutLines);
        ++iterNew;
    });
    assert(iterNew == endNew);
}

std::shared_ptr<WaveClip> WaveClip::SplitChannels()
{
    assert(NChannels() == 2);

    // Make empty copies of this and all cutlines
    CreateToken token{ true };
    auto result = std::make_shared<WaveClip>(*this, GetFactory(), true, token);

    // Move one Sequence
    TransferSequence(*this, *result);

    // Must also do that for cutlines, which must be in correspondence, because
    // of the post of the constructor.
    // And possibly too for cutlines inside of cutlines!
    FixSplitCutlines(mCutLines, result->mCutLines);

    // Fix attachments in the new clip and assert consistency conditions between
    // the clip and its cutlines
    result->Attachments::ForEach([](WaveClipListener& attachment){
        attachment.Erase(0);
    });
    assert(result->CheckInvariants());

    // This call asserts invariants for this clip
    DiscardRightChannel();

    // Assert postconditions
    assert(NChannels() == 1);
    assert(result->NChannels() == 1);
    return result;
}

void WaveClip::MakeStereo(WaveClip&& other, bool mustAlign)
{
    assert(NChannels() == 1);
    assert(other.NChannels() == 1);
    assert(GetSampleFormats() == other.GetSampleFormats());
    assert(GetFactory() == other.GetFactory());
    assert(!mustAlign || GetNumSamples() == other.GetNumSamples());

    mCutLines.clear();
    mSequences.resize(2);
    mSequences[1] = move(other.mSequences[0]);

    this->Attachments::ForCorresponding(other,
                                        [mustAlign](WaveClipListener* pLeft, WaveClipListener* pRight){
        // Precondition of callback from ClientData::Site
        assert(pLeft && pRight);
        pLeft->MakeStereo(std::move(*pRight), mustAlign);
    });

    if (mustAlign) {
        assert(StrongInvariant());
    } else {
        assert(CheckInvariants());
    }
}

size_t WaveClip::GreatestAppendBufferLen() const
{
    size_t result = 0;
    for (size_t iChannel = 0; iChannel < NChannels(); ++iChannel) {
        result = std::max(result, mSequences[iChannel]->GetAppendBufferLen());
    }
    return result;
}

void WaveClip::OnProjectTempoChange(
    const std::optional<double>& oldTempo, double newTempo)
{
    if (!mRawAudioTempo.has_value()) {
        // When we have tempo detection ready (either by header-file
        // read-up or signal analysis) we can use something smarter than that. In
        // the meantime, use the tempo of the project when the clip is created as
        // source tempo.
        mRawAudioTempo = oldTempo.value_or(newTempo);
    }

    if (oldTempo.has_value()) {
        const auto ratioChange = oldTempo.value() / newTempo;
        mSequenceOffset *= ratioChange;
        if (!mStretchToMatchProjectTempo) {
            mSequenceOffset += ((mTrimLeft * ratioChange) - mTrimLeft);
            return;
        }
        mTrimLeft *= ratioChange;
        mTrimRight *= ratioChange;
        StretchCutLines(ratioChange);
        mEnvelope->RescaleTimesBy(ratioChange);
    }

    if (mStretchToMatchProjectTempo) {
        mClipTempo = newTempo;
    }

    Observer::Publisher<StretchRatioChange>::Publish(
        StretchRatioChange { GetStretchRatio() });
}

void WaveClip::StretchLeftTo(double to)
{
    const auto pet = GetPlayEndTime();
    if (to >= pet) {
        return;
    }
    const auto oldPlayDuration = pet - GetPlayStartTime();
    const auto newPlayDuration = pet - to;
    const auto ratioChange = newPlayDuration / oldPlayDuration;
    mSequenceOffset = pet - (pet - mSequenceOffset) * ratioChange;
    mTrimLeft *= ratioChange;
    mTrimRight *= ratioChange;
    mClipStretchRatio *= ratioChange;
    mEnvelope->SetOffset(mSequenceOffset);
    mEnvelope->RescaleTimesBy(ratioChange);
    StretchCutLines(ratioChange);
    Observer::Publisher<StretchRatioChange>::Publish(
        StretchRatioChange { GetStretchRatio() });
}

void WaveClip::StretchRightTo(double to)
{
    const auto pst = GetPlayStartTime();
    if (to <= pst) {
        return;
    }
    const auto oldPlayDuration = GetPlayEndTime() - pst;
    const auto newPlayDuration = to - pst;
    const auto ratioChange = newPlayDuration / oldPlayDuration;
    StretchBy(ratioChange);
}

void WaveClip::StretchBy(double ratio)
{
    const auto pst = GetPlayStartTime();
    mSequenceOffset = pst - mTrimLeft * ratio;
    mTrimLeft *= ratio;
    mTrimRight *= ratio;
    mClipStretchRatio *= ratio;
    mEnvelope->SetOffset(mSequenceOffset);
    mEnvelope->RescaleTimesBy(ratio);
    StretchCutLines(ratio);
    Observer::Publisher<StretchRatioChange>::Publish(
        StretchRatioChange { GetStretchRatio() });
}

void WaveClip::StretchCutLines(double ratioChange)
{
    for (const auto& cutline : mCutLines) {
        cutline->mSequenceOffset *= ratioChange;
        cutline->mTrimLeft *= ratioChange;
        cutline->mTrimRight *= ratioChange;
        cutline->mClipStretchRatio *= ratioChange;
        cutline->mEnvelope->RescaleTimesBy(ratioChange);
    }
}

double WaveClip::GetStretchRatio() const
{
    const auto dstSrcRatio
        =mClipTempo.has_value() && mRawAudioTempo.has_value()
          ? *mRawAudioTempo / *mClipTempo
          : 1.0;
    return mClipStretchRatio * dstSrcRatio;
}

int WaveClip::GetCentShift() const
{
    return mCentShift;
}

Observer::Subscription
WaveClip::SubscribeToCentShiftChange(std::function<void(int)> cb) const
{
    // Consider the list of subcribers as a mutable member that doesn't change
    // real state
    return const_cast<WaveClip*>(this)->
           Observer::Publisher<CentShiftChange>::Subscribe(
        [cb](const CentShiftChange& cents) { cb(cents.newValue); });
}

Observer::Subscription WaveClip::SubscribeToPitchAndSpeedPresetChange(
    std::function<void(PitchAndSpeedPreset)> cb) const
{
    // Consider the list of subcribers as a mutable member that doesn't change
    // real state
    return const_cast<WaveClip*>(this)->
           Observer::Publisher<PitchAndSpeedPresetChange>::Subscribe(
        [cb](const PitchAndSpeedPresetChange& formant) {
        cb(formant.newValue);
    });
}

bool WaveClip::HasEqualPitchAndSpeed(const WaveClip& other) const
{
    return StretchRatioEquals(other.GetStretchRatio())
           && GetCentShift() == other.GetCentShift();
}

bool WaveClip::HasPitchOrSpeed() const
{
    return !StretchRatioEquals(1.0) || GetCentShift() != 0;
}

bool WaveClip::GetStretchToMatchProjectTempo() const
{
    return mStretchToMatchProjectTempo;
}

void WaveClip::SetStretchToMatchProjectTempo(bool enabled)
{
    if (mStretchToMatchProjectTempo == enabled) {
        return;
    }
    mStretchToMatchProjectTempo = enabled;
}

bool WaveClip::StretchRatioEquals(double value) const
{
    return TimeAndPitchInterface::IsPassThroughMode(
        1 + GetStretchRatio() - value);
}

sampleCount WaveClip::GetNumSamples() const
{
    // Assume only the weak invariant
    sampleCount result = 0;
    for (auto& pSequence: mSequences) {
        result = std::max(result, pSequence->GetNumSamples());
    }
    return result;
}

SampleFormats WaveClip::GetSampleFormats() const
{
    // All sequences have the same formats by class invariant
    return mSequences[0]->GetSampleFormats();
}

size_t WaveClip::CountBlocks() const
{
    return std::accumulate(mSequences.begin(), mSequences.end(), size_t {},
                           [](size_t acc, auto& pSequence){
        return acc + pSequence->GetBlockArray().size();
    });
}

//! A hint for sizing of well aligned fetches
size_t WaveClip::GetBestBlockSize(sampleCount t) const
{
    return mSequences[0]->GetBestBlockSize(t);
}

size_t WaveClip::GetMaxBlockSize() const
{
    return std::accumulate(mSequences.begin(), mSequences.end(), size_t {},
                           [](size_t acc, auto& pSequence){
        return std::max(acc, pSequence->GetMaxBlockSize());
    });
}

const SampleBlockFactoryPtr& WaveClip::GetFactory() const
{
    // All sequences have the same factory by class invariant
    return mSequences[0]->GetFactory();
}

std::vector<std::unique_ptr<Sequence> > WaveClip::GetEmptySequenceCopies() const
{
    decltype(mSequences) newSequences;
    newSequences.reserve(mSequences.size());
    for (auto& pSequence : mSequences) {
        newSequences.push_back(std::make_unique<Sequence>(
                                   pSequence->GetFactory(), pSequence->GetSampleFormats()));
    }
    return newSequences;
}

constSamplePtr WaveClip::GetAppendBuffer(size_t ii) const
{
    assert(ii < NChannels());
    return mSequences[ii]->GetAppendBuffer();
}

void WaveClip::MarkChanged() noexcept // NOFAIL-GUARANTEE
{
    Attachments::ForEach(std::mem_fn(&WaveClipListener::MarkChanged));
}

std::pair<float, float> WaveClip::GetMinMax(size_t ii,
                                            double t0, double t1, bool mayThrow) const
{
    assert(ii < NChannels());
    t0 = std::max(t0, GetPlayStartTime());
    t1 = std::min(t1, GetPlayEndTime());
    if (t0 > t1) {
        if (mayThrow) {
            THROW_INCONSISTENCY_EXCEPTION;
        }
        return {
            0.f, // harmless, but unused since Sequence::GetMinMax does not use these values
            0.f // harmless, but unused since Sequence::GetMinMax does not use these values
        };
    }

    if (t0 == t1) {
        return{ 0.f, 0.f };
    }

    auto s0 = TimeToSequenceSamples(t0);
    auto s1 = TimeToSequenceSamples(t1);

    return mSequences[ii]->GetMinMax(s0, s1 - s0, mayThrow);
}

float WaveClip::GetRMS(size_t ii, double t0, double t1, bool mayThrow) const
{
    assert(ii < NChannels());
    if (t0 > t1) {
        if (mayThrow) {
            THROW_INCONSISTENCY_EXCEPTION;
        }
        return 0.f;
    }

    if (t0 == t1) {
        return 0.f;
    }

    auto s0 = TimeToSequenceSamples(t0);
    auto s1 = TimeToSequenceSamples(t1);

    return mSequences[ii]->GetRMS(s0, s1 - s0, mayThrow);
}

void WaveClip::ConvertToSampleFormat(sampleFormat format,
                                     const std::function<void(size_t)>& progressReport)
{
    // This mutator does not require the strong invariant.  It leaves sample
    // counts unchanged in each sequence.

    // Note:  it is not necessary to do this recursively to cutlines.
    // They get converted as needed when they are expanded.

    Transaction transaction{ *this };

    auto bChanged = mSequences[0]->ConvertToSampleFormat(format, progressReport);
    for (size_t ii = 1, width = NChannels(); ii < width; ++ii) {
        bool alsoChanged
            =mSequences[ii]->ConvertToSampleFormat(format, progressReport);
        // Class invariant implies:
        assert(bChanged == alsoChanged);
    }
    if (bChanged) {
        MarkChanged();
    }

    transaction.Commit();
}

/*! @excsafety{No-fail} */
void WaveClip::UpdateEnvelopeTrackLen()
{
    // The envelope time points account for stretching.
    const auto len = GetNumSamples().as_double() * GetStretchRatio() / mRate;
    if (len != mEnvelope->GetTrackLen()) {
        mEnvelope->SetTrackLen(len, 1.0 / GetRate());
    }
}

/*! @excsafety{Strong} */
std::shared_ptr<SampleBlock>
WaveClip::AppendToChannel(size_t iChannel,
                          constSamplePtr buffer, sampleFormat format, size_t len)
{
    assert(iChannel < NChannels());
    return mSequences[iChannel]->AppendNewBlock(buffer, format, len);
}

/*! @excsafety{Strong} */
std::shared_ptr<SampleBlock>
WaveClip::AppendLegacyNewBlock(constSamplePtr buffer, sampleFormat format, size_t len)
{
    // This is a special use function for legacy files only and this assertion
    // does not need to be relaxed.  The clip is in a still unzipped track.
    assert(NChannels() == 1);
    return AppendToChannel(0, buffer, format, len);
}

/*! @excsafety{Strong} */
void WaveClip::AppendLegacySharedBlock(
    const std::shared_ptr<SampleBlock>& pBlock)
{
    // This is a special use function for legacy files only and this assertion
    // does not need to be relaxed.  The clip is in a still unzipped track.
    assert(NChannels() == 1);
    mSequences[0]->AppendSharedBlock(pBlock);
}

bool WaveClip::Append(size_t iChannel, const size_t nChannels,
                      constSamplePtr buffers[], sampleFormat format,
                      size_t len, unsigned int stride, sampleFormat effectiveFormat)
{
    assert(iChannel < NChannels());
    assert(iChannel + nChannels <= NChannels());

    // No requirement or promise of the strong invariant, and therefore no
    // need for Transaction

    //wxLogDebug(wxT("Append: len=%lli"), (long long) len);

    bool appended = false;
    for (size_t ii = 0; ii < nChannels; ++ii) {
        appended = mSequences[iChannel + ii]->Append(
            buffers[ii], format, len, stride, effectiveFormat)
                   || appended;
    }

    // use No-fail-guarantee
    UpdateEnvelopeTrackLen();
    MarkChanged();

    return appended;
}

bool WaveClip::Append(constSamplePtr buffers[], sampleFormat format,
                      size_t len, unsigned int stride, sampleFormat effectiveFormat)
{
    StrongInvariantScope scope{ *this };

    Transaction transaction{ *this };

    //wxLogDebug(wxT("Append: len=%lli"), (long long) len);

    size_t ii = 0;
    bool appended = false;
    for (auto& pSequence : mSequences) {
        appended
            =pSequence->Append(buffers[ii++], format, len, stride, effectiveFormat)
              || appended;
    }

    transaction.Commit();
    // use No-fail-guarantee
    UpdateEnvelopeTrackLen();
    MarkChanged();

    return appended;
}

void WaveClip::Flush()
{
    // Does not require or guarantee the strong invariant

    //wxLogDebug(wxT("WaveClip::Flush"));
    //wxLogDebug(wxT("   mAppendBufferLen=%lli"), (long long) mAppendBufferLen);
    //wxLogDebug(wxT("   previous sample count %lli"), (long long) mSequence->GetNumSamples());

    if (GreatestAppendBufferLen() > 0) {
        Transaction transaction{ *this };

        for (auto& pSequence : mSequences) {
            pSequence->Flush();
        }

        transaction.Commit();

        // No-fail operations
        UpdateEnvelopeTrackLen();
        MarkChanged();
    }

    //wxLogDebug(wxT("now sample count %lli"), (long long) mSequence->GetNumSamples());
}

void WaveClip::RepairChannels()
{
    if (NChannels() < 2) {
        return;
    }
    // Be sure of consistency of sample counts
    // We may be here because the drive can't hold another megabyte, but
    // note that InsertSilence makes silent blocks that don't occupy
    // space in the database table of blocks.
    // (However autosave may want to rewrite the document blob, so this solution
    // may yet not be perfect.)
    Transaction transaction{ *this };
    const auto maxSamples = GetNumSamples();
    for (const auto& pSequence: mSequences) {
        const auto numSamples = pSequence->GetNumSamples();
        if (pSequence->GetNumSamples() != maxSamples) {
            pSequence->InsertSilence(numSamples, maxSamples - numSamples);
        }
    }
    transaction.Commit();
}

static constexpr auto Offset_attr = "offset";
static constexpr auto TrimLeft_attr = "trimLeft";
static constexpr auto TrimRight_attr = "trimRight";
static constexpr auto CentShiftAttr = "centShift";
static constexpr auto PitchAndSpeedPreset_attr = "pitchAndSpeedPreset";
static constexpr auto RawAudioTempo_attr = "rawAudioTempo";
static constexpr auto ClipStretchRatio_attr = "clipStretchRatio";
static constexpr auto ClipStretchToMatchTempo_attr = "clipStretchToMatchTempo";
static constexpr auto ClipTempo_attr = "clipTempo";
static constexpr auto Name_attr = "name";
static constexpr auto GroupId_attr = "groupId";
static constexpr auto Color_attr = "color";

bool WaveClip::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == WaveClip_tag) {
        double dblValue;
        long longValue;
        bool boolValue;
        for (auto pair : attrs) {
            auto attr = pair.first;
            auto value = pair.second;

            if (attr == Offset_attr) {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                SetSequenceStartTime(dblValue);
            } else if (attr == TrimLeft_attr) {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                SetTrimLeft(dblValue);
            } else if (attr == TrimRight_attr) {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                SetTrimRight(dblValue);
            } else if (attr == CentShiftAttr) {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                mCentShift = dblValue;
            } else if (attr == PitchAndSpeedPreset_attr) {
                if (!value.TryGet(longValue)) {
                    return false;
                }
                mPitchAndSpeedPreset = static_cast<PitchAndSpeedPreset>(longValue);
            } else if (attr == RawAudioTempo_attr) {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                if (dblValue == 0) {
                    mRawAudioTempo.reset();
                } else {
                    mRawAudioTempo = dblValue;
                }
            } else if (attr == ClipStretchRatio_attr) {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                mClipStretchRatio = dblValue;
            } else if (attr == ClipStretchToMatchTempo_attr) {
                if (!value.TryGet(boolValue)) {
                    return false;
                }
                mStretchToMatchProjectTempo = boolValue;
            } else if (attr == ClipTempo_attr) {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                mClipTempo = dblValue;
            } else if (attr == Name_attr) {
                if (value.IsStringView()) {
                    SetName(value.ToWString());
                }
            } else if (attr == GroupId_attr) {
                if (!value.TryGet(longValue)) {
                    return false;
                }
                mGroupId = longValue;
            } else if (attr == Color_attr) {
                if (value.IsStringView()) {
                    SetColor(value.ToWString());
                }
            } else if (Attachments::FindIf(
                           [&](WaveClipListener& listener){
                return listener.HandleXMLAttribute(attr, value);
            }
                           )) {
            }
        }
        return true;
    }

    return false;
}

void WaveClip::HandleXMLEndTag(const std::string_view& tag)
{
    // All blocks were deserialized into new sequences; remove the one made
    // by the constructor which remains empty.
    mSequences.erase(mSequences.begin());
    mSequences.shrink_to_fit();
    if (tag == WaveClip_tag) {
        UpdateEnvelopeTrackLen();
    }
    // A proof of this assertion assumes that nothing has happened since
    // construction of this, besides calls to the other deserialization
    // functions
    assert(CheckInvariants());
}

XMLTagHandler* WaveClip::HandleXMLChild(const std::string_view& tag)
{
    auto& pFirst = mSequences[0];
    if (tag == Sequence::Sequence_tag) {
        // Push back a new sequence prototyped from the empty sequence made
        // by the constructor.  See also HandleXMLEndTag above.
        // Assume sequences were serialized in channel iteration order.
        mSequences.push_back(std::make_unique<Sequence>(
                                 pFirst->GetFactory(), pFirst->GetSampleFormats()));
        return mSequences.back().get();
    } else if (tag == "envelope") {
        return mEnvelope.get();
    } else if (tag == WaveClip_tag) {
        // Nested wave clips are cut lines
        auto format = pFirst->GetSampleFormats().Stored();
        // The format is not stored in WaveClip itself but passed to
        // Sequence::Sequence; but then the Sequence will deserialize format
        // again

        // Make only one channel now, but recursive deserialization
        // increases the width later
        mCutLines.push_back(WaveClip::NewShared(1, pFirst->GetFactory(), format, mRate));
        return mCutLines.back().get();
    } else {
        return nullptr;
    }
}

void WaveClip::WriteXML(size_t ii, XMLWriter& xmlFile) const
// may throw
{
    assert(ii < NChannels());

    if (GetSequenceSamplesCount() <= 0) {
        // Oops, I'm empty? How did that happen? Anyway, I do nothing but causing
        // problems, don't save me.
        return;
    }

    xmlFile.StartTag(WaveClip_tag);
    xmlFile.WriteAttr(Offset_attr, mSequenceOffset, 8);
    xmlFile.WriteAttr(TrimLeft_attr, mTrimLeft, 8);
    xmlFile.WriteAttr(TrimRight_attr, mTrimRight, 8);
    xmlFile.WriteAttr(CentShiftAttr, mCentShift);
    xmlFile.WriteAttr(PitchAndSpeedPreset_attr,
                      static_cast<long>(mPitchAndSpeedPreset));
    xmlFile.WriteAttr(RawAudioTempo_attr, mRawAudioTempo.value_or(0.), 8);
    xmlFile.WriteAttr(ClipStretchRatio_attr, mClipStretchRatio, 8);
    xmlFile.WriteAttr(ClipStretchToMatchTempo_attr, mStretchToMatchProjectTempo);
    xmlFile.WriteAttr(ClipTempo_attr, mClipTempo.value_or(0.), 8);
    xmlFile.WriteAttr(Name_attr, mName);
    xmlFile.WriteAttr(GroupId_attr, static_cast<long>(mGroupId));
    xmlFile.WriteAttr(Color_attr, mColor);
    Attachments::ForEach([&](const WaveClipListener& listener){
        listener.WriteXMLAttributes(xmlFile);
    });

    mSequences[ii]->WriteXML(xmlFile);
    mEnvelope->WriteXML(xmlFile);

    for (const auto& clip: mCutLines) {
        clip->WriteXML(ii, xmlFile);
    }

    xmlFile.EndTag(WaveClip_tag);
}

/*! @excsafety{Strong} */
bool WaveClip::Paste(double t0, const WaveClip& o)
{
    const WaveClip* pOther = &o;
    WaveClipHolder dup;

    // None of the clips created here will survive, they are just temporary
    // placeholders. Whether we set backup to true or false doesn't matter much,
    // but setting it to `false` would increase the ID counter unnecessarily.
    constexpr auto backup = true;

    if (!o.StrongInvariant()) {
        assert(false); // precondition not honored
        // But try to repair it and continue in release
        dup = WaveClip::NewSharedFrom(o, o.GetFactory(), true, backup);
        dup->RepairChannels();
        pOther = dup.get();
    }
    auto& other = *pOther;

    if (NChannels() != other.NChannels()) {
        // post is satisfied
        return false;
    }

    if (GetSequenceSamplesCount() == 0) {
        // Empty clip: we're flexible and adopt the other's stretching.
        mRawAudioTempo = other.mRawAudioTempo;
        mClipStretchRatio = other.mClipStretchRatio;
        mClipTempo = other.mClipTempo;
    } else if (GetStretchRatio() != other.GetStretchRatio()) {
        // post is satisfied
        return false;
    }

    StrongInvariantScope scope{ *this };

    Transaction transaction{ *this };

    const bool clipNeedsResampling = other.mRate != mRate;
    const bool clipNeedsNewFormat
        =other.GetSampleFormats().Stored() != GetSampleFormats().Stored();
    std::shared_ptr<WaveClip> newClip;

    t0 = std::clamp(t0, GetPlayStartTime(), GetPlayEndTime());
    // Delay the finish of the clearing of this clip
    ClearSequenceFinisher finisher;

    //seems like edge cases cannot happen, see WaveTrack::PasteWaveTrack
    auto& factory = GetFactory();
    if (t0 == GetPlayStartTime()) {
        finisher = ClearSequence(GetSequenceStartTime(), t0);
        SetTrimLeft(other.GetTrimLeft());

        auto copy = WaveClip::NewSharedFrom(other, factory, true, backup);
        copy->ClearSequence(copy->GetPlayEndTime(), copy->GetSequenceEndTime())
        .Commit();
        newClip = std::move(copy);
    } else if (t0 == GetPlayEndTime()) {
        finisher = ClearSequence(GetPlayEndTime(), GetSequenceEndTime());
        SetTrimRight(other.GetTrimRight());

        auto copy = WaveClip::NewSharedFrom(other, factory, true, backup);
        copy->ClearSequence(copy->GetSequenceStartTime(), copy->GetPlayStartTime())
        .Commit();
        newClip = std::move(copy);
    } else {
        newClip = WaveClip::NewSharedFrom(other, factory, true, backup);
        newClip->ClearSequence(newClip->GetPlayEndTime(), newClip->GetSequenceEndTime())
        .Commit();
        newClip->ClearSequence(newClip->GetSequenceStartTime(), newClip->GetPlayStartTime())
        .Commit();
        newClip->SetTrimLeft(0);
        newClip->SetTrimRight(0);
    }

    if (clipNeedsResampling || clipNeedsNewFormat) {
        auto copy = WaveClip::NewSharedFrom(*newClip.get(), factory, true, backup);

        if (clipNeedsResampling) {
            // The other clip's rate is different from ours, so resample
            copy->Resample(mRate);
        }

        if (clipNeedsNewFormat) {
            // Force sample formats to match.
            copy->ConvertToSampleFormat(GetSampleFormats().Stored());
        }
        newClip = std::move(copy);
    }

    // Paste cut lines contained in pasted clip
    WaveClipHolders newCutlines;
    for (const auto& cutline: newClip->mCutLines) {
        auto cutlineCopy = WaveClip::NewSharedFrom(*cutline, factory,
                                                   // Recursively copy cutlines of cutlines.  They don't need
                                                   // their offsets adjusted.
                                                   true, backup);
        cutlineCopy->ShiftBy(t0 - GetSequenceStartTime());
        newCutlines.push_back(std::move(cutlineCopy));
    }

    sampleCount s0 = TimeToSequenceSamples(t0);

    // Because newClip was made above as a copy of (a copy of) other
    assert(other.NChannels() == newClip->NChannels());
    // And other has the same width as this, so this loop is safe
    // Assume Strong-guarantee from Sequence::Paste
    for (size_t ii = 0, width = NChannels(); ii < width; ++ii) {
        mSequences[ii]->Paste(s0, newClip->mSequences[ii].get());
    }

    // Assume No-fail-guarantee in the remaining

    finisher.Commit();
    transaction.Commit();
    MarkChanged();

    const auto sampleTime = 1.0 / GetRate();
    const auto timeOffsetInEnvelope
        =s0.as_double() * GetStretchRatio() / mRate + GetSequenceStartTime();
    mEnvelope->PasteEnvelope(
        timeOffsetInEnvelope, newClip->mEnvelope.get(), sampleTime);
    OffsetCutLines(t0, newClip->GetPlayEndTime() - newClip->GetPlayStartTime());

    for (auto& holder : newCutlines) {
        mCutLines.push_back(std::move(holder));
    }

    return true;
}

/*! @excsafety{Strong} */
void WaveClip::InsertSilence(double t, double len, double* pEnvelopeValue)
{
    StrongInvariantScope scope{ *this };
    Transaction transaction{ *this };
    ClearSequenceFinisher finisher;

    if (t == GetPlayStartTime() && t > GetSequenceStartTime()) {
        finisher = ClearSequence(GetSequenceStartTime(), t);
    } else if (t == GetPlayEndTime() && t < GetSequenceEndTime()) {
        finisher = ClearSequence(t, GetSequenceEndTime());
        SetTrimRight(.0);
    }

    const auto s0 = TimeToSequenceSamples(t);
    const auto slen = TimeToSamples(len);

    // use Strong-guarantee
    for (auto& pSequence : mSequences) {
        pSequence->InsertSilence(s0, slen);
    }

    // use No-fail-guarantee in the rest
    finisher.Commit();
    transaction.Commit();

    OffsetCutLines(t, len);

    const auto sampleTime = 1.0 / GetRate();
    auto& envelope = GetEnvelope();
    if (pEnvelopeValue) {
        // Preserve limit value at the end
        auto oldLen = envelope.GetTrackLen();
        auto newLen = oldLen + len;
        envelope.Cap(sampleTime);

        // Ramp across the silence to the given value
        envelope.SetTrackLen(newLen, sampleTime);
        envelope.InsertOrReplace
            (envelope.GetOffset() + newLen, *pEnvelopeValue);
    } else {
        envelope.InsertSpace(t, len);
    }

    MarkChanged();
}

/*! @excsafety{Strong} */
void WaveClip::AppendSilence(double len, double envelopeValue)
{
    auto t = GetPlayEndTime();
    InsertSilence(t, len, &envelopeValue);
}

/*! @excsafety{Strong} */
void WaveClip::Clear(double t0, double t1)
{
    auto st0 = t0;
    auto st1 = t1;
    auto offset = .0;
    if (st0 <= GetPlayStartTime()) {
        offset = (t0 - GetPlayStartTime()) + GetTrimLeft();
        st0 = GetSequenceStartTime();

        SetTrimLeft(.0);
    }
    if (st1 >= GetPlayEndTime()) {
        st1 = GetSequenceEndTime();
        SetTrimRight(.0);
    }
    Transaction transaction{ *this };
    ClearSequence(st0, st1)
    .Commit();
    transaction.Commit();
    MarkChanged();

    if (offset != .0) {
        ShiftBy(offset);
    }
}

void WaveClip::ClearLeft(double t)
{
    if (t > GetPlayStartTime() && t < GetPlayEndTime()) {
        Transaction transaction{ *this };
        ClearSequence(GetSequenceStartTime(), t)
        .Commit();
        transaction.Commit();
        SetTrimLeft(.0);
        SetSequenceStartTime(t);
        MarkChanged();
    }
}

void WaveClip::ClearRight(double t)
{
    if (t > GetPlayStartTime() && t < GetPlayEndTime()) {
        Transaction transaction{ *this };
        ClearSequence(t, GetSequenceEndTime())
        .Commit();
        transaction.Commit();
        SetTrimRight(.0);
        MarkChanged();
    }
}

auto WaveClip::ClearSequence(double t0, double t1) -> ClearSequenceFinisher
{
    StrongInvariantScope scope{ *this };

    auto clip_t0 = std::max(t0, GetSequenceStartTime());
    auto clip_t1 = std::min(t1, GetSequenceEndTime());

    auto s0 = TimeToSequenceSamples(clip_t0);
    auto s1 = TimeToSequenceSamples(clip_t1);

    if (s0 == s1) {
        return {};
    }

    // use Strong-guarantee
    for (auto& pSequence : mSequences) {
        pSequence->Delete(s0, s1 - s0);
    }

    return { this, t0, t1, clip_t0, clip_t1 };
}

WaveClip::ClearSequenceFinisher::~ClearSequenceFinisher() noexcept
{
    if (!pClip || !committed) {
        return;
    }

    // use No-fail-guarantee in the remaining

    // msmeyer
    //
    // Delete all cutlines that are within the given area, if any.
    //
    // Note that when cutlines are active, two functions are used:
    // Clear() and ClearAndAddCutLine(). ClearAndAddCutLine() is called
    // whenever the user directly calls a command that removes some audio, e.g.
    // "Cut" or "Clear" from the menu. This command takes care about recursive
    // preserving of cutlines within clips. Clear() is called when internal
    // operations want to remove audio. In the latter case, it is the right
    // thing to just remove all cutlines within the area.
    //

    // May DELETE as we iterate, so don't use range-for
    for (auto it = pClip->mCutLines.begin(); it != pClip->mCutLines.end();) {
        WaveClip* clip = it->get();
        double cutlinePosition
            =pClip->GetSequenceStartTime() + clip->GetSequenceStartTime();
        if (cutlinePosition >= t0 && cutlinePosition <= t1) {
            // This cutline is within the area, DELETE it
            it = pClip->mCutLines.erase(it);
        } else {
            if (cutlinePosition >= t1) {
                clip->ShiftBy(clip_t0 - clip_t1);
            }
            ++it;
        }
    }

    // Collapse envelope
    auto sampleTime = 1.0 / pClip->GetRate();
    pClip->GetEnvelope().CollapseRegion(t0, t1, sampleTime);
}

/*! @excsafety{Weak}
-- This WaveClip remains destructible in case of AudacityException.
But some cutlines may be deleted */
void WaveClip::ClearAndAddCutLine(double t0, double t1)
{
    StrongInvariantScope scope{ *this };
    if (t0 > GetPlayEndTime() || t1 < GetPlayStartTime() || CountSamples(t0, t1) == 0) {
        return; // no samples to remove
    }
    Transaction transaction{ *this };

    const double clip_t0 = std::max(t0, GetPlayStartTime());
    const double clip_t1 = std::min(t1, GetPlayEndTime());

    // We don't care about new IDs for cutline clips: when the cutline gets
    // expanded, the placeholder clip gets merged anyway.
    constexpr auto backup = true;
    auto newClip = WaveClip::NewSharedFromRange(
        *this, GetFactory(), true, backup, clip_t0, clip_t1);

    if (t1 < GetPlayEndTime()) {
        newClip->ClearSequence(t1, newClip->GetSequenceEndTime())
        .Commit();
        newClip->SetTrimRight(.0);
    }
    if (t0 > GetPlayStartTime()) {
        newClip->ClearSequence(newClip->GetSequenceStartTime(), t0)
        .Commit();
        newClip->SetTrimLeft(.0);
    }

    newClip->SetSequenceStartTime(clip_t0 - GetSequenceStartTime());

    // Remove cutlines from this clip that were in the selection, shift
    // left those that were after the selection
    // May DELETE as we iterate, so don't use range-for
    for (auto it = mCutLines.begin(); it != mCutLines.end();) {
        WaveClip* clip = it->get();
        double cutlinePosition = GetSequenceStartTime() + clip->GetSequenceStartTime();
        if (cutlinePosition >= t0 && cutlinePosition <= t1) {
            it = mCutLines.erase(it);
        } else {
            if (cutlinePosition >= t1) {
                clip->ShiftBy(clip_t0 - clip_t1);
            }
            ++it;
        }
    }

    // Clear actual audio data
    auto s0 = TimeToSequenceSamples(t0);
    auto s1 = TimeToSequenceSamples(t1);

    // use Weak-guarantee
    for (auto& pSequence : mSequences) {
        pSequence->Delete(s0, s1 - s0);
    }

    // Collapse envelope
    auto sampleTime = 1.0 / GetRate();
    GetEnvelope().CollapseRegion(t0, t1, sampleTime);

    transaction.Commit();
    MarkChanged();
    AddCutLine(move(newClip));
}

void WaveClip::AddCutLine(WaveClipHolder pClip)
{
    assert(NChannels() == pClip->NChannels());
    mCutLines.push_back(move(pClip));
    // New clip is assumed to have correct width
    assert(CheckInvariants());
}

bool WaveClip::FindCutLine(double cutLinePosition,
                           double* cutlineStart /* = NULL */,
                           double* cutlineEnd /* = NULL */) const
{
    for (const auto& cutline: mCutLines) {
        if (fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001) {
            auto startTime = GetSequenceStartTime() + cutline->GetSequenceStartTime();
            if (cutlineStart) {
                *cutlineStart = startTime;
            }
            if (cutlineEnd) {
                *cutlineEnd = startTime + cutline->SamplesToTime(cutline->GetVisibleSampleCount());
            }
            return true;
        }
    }

    return false;
}

/*! @excsafety{Strong} */
void WaveClip::ExpandCutLine(double cutLinePosition)
{
    auto end = mCutLines.end();
    auto it = std::find_if(mCutLines.begin(), end,
                           [&](const WaveClipHolder& cutline) {
        return fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001;
    });

    if (it != end) {
        auto* cutline = it->get();
        // assume Strong-guarantee from Paste

        // Envelope::Paste takes offset into account, WaveClip::Paste doesn't!
        // Do this to get the right result:
        cutline->mEnvelope->SetOffset(0);
        bool success = Paste(
            GetSequenceStartTime() + cutline->GetSequenceStartTime(), *cutline);
        assert(success); // class invariant promises cutlines have correct width

        // Now erase the cutline,
        // but be careful to find it again, because Paste above may
        // have modified the array of cutlines (if our cutline contained
        // another cutline!), invalidating the iterator we had.
        end = mCutLines.end();
        it = std::find_if(mCutLines.begin(), end,
                          [=](const WaveClipHolder& p) { return p.get() == cutline; });
        if (it != end) {
            mCutLines.erase(it); // deletes cutline!
        } else {
            wxASSERT(false);
        }
    }
}

bool WaveClip::RemoveCutLine(double cutLinePosition)
{
    for (auto it = mCutLines.begin(); it != mCutLines.end(); ++it) {
        const auto& cutline = *it;
        //std::numeric_limits<double>::epsilon() or (1.0 / static_cast<double>(mRate))?
        if (fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001) {
            mCutLines.erase(it); // deletes cutline!
            return true;
        }
    }

    return false;
}

/*! @excsafety{No-fail} */
void WaveClip::OffsetCutLines(double t0, double len)
{
    for (const auto& cutLine : mCutLines) {
        if (GetSequenceStartTime() + cutLine->GetSequenceStartTime() >= t0) {
            cutLine->ShiftBy(len);
        }
    }
}

void WaveClip::CloseLock() noexcept
{
    // Don't need a Transaction for noexcept operations
    for (auto& pSequence : mSequences) {
        pSequence->CloseLock();
    }
    for (const auto& cutline: mCutLines) {
        cutline->CloseLock();
    }
}

void WaveClip::SetRate(int rate)
{
    const auto trimLeftSampleNum = TimeToSamples(mTrimLeft);
    const auto trimRightSampleNum = TimeToSamples(mTrimRight);
    auto ratio = static_cast<double>(mRate) / rate;
    mRate = rate;
    mTrimLeft = SamplesToTime(trimLeftSampleNum);
    mTrimRight = SamplesToTime(trimRightSampleNum);
    const auto newLength
        =GetNumSamples().as_double() * GetStretchRatio() / mRate;
    mEnvelope->RescaleTimes(newLength);
    MarkChanged();
    SetSequenceStartTime(GetSequenceStartTime() * ratio);
}

void WaveClip::SetRawAudioTempo(double tempo)
{
    mRawAudioTempo = tempo;
}

void WaveClip::SetClipTempo(double tempo)
{
    mClipTempo = tempo;
}

bool WaveClip::SetCentShift(int cents)
{
    if (
        cents < TimeAndPitchInterface::MinCents
        || cents > TimeAndPitchInterface::MaxCents) {
        return false;
    }
    mCentShift = cents;
    Observer::Publisher<CentShiftChange>::Publish(CentShiftChange { cents });
    return true;
}

void WaveClip::SetPitchAndSpeedPreset(PitchAndSpeedPreset preset)
{
    mPitchAndSpeedPreset = preset;
    Observer::Publisher<PitchAndSpeedPresetChange>::Publish(
        PitchAndSpeedPresetChange { mPitchAndSpeedPreset });
}

PitchAndSpeedPreset WaveClip::GetPitchAndSpeedPreset() const
{
    return mPitchAndSpeedPreset;
}

/*! @excsafety{Strong} */
void WaveClip::Resample(int rate, BasicUI::ProgressDialog* progress)
{
    // This mutator does not require the strong invariant.

    // Note:  it is not necessary to do this recursively to cutlines.
    // They get resampled as needed when they are expanded.

    if (rate == mRate) {
        return; // Nothing to do
    }
    // This function does its own RAII without a Transaction

    double factor = (double)rate / (double)mRate;
    //Resample is always configured to have single channel.
    //Create Resample instance per each channel in the clip
    std::vector<::Resample> resample;
    for (unsigned n = 0; n < mSequences.size(); ++n) {
        resample.emplace_back(true, factor, factor);// constant rate resampling
    }
    const size_t bufsize = 65536;
    Floats inBuffer{ bufsize };
    Floats outBuffer{ bufsize };
    sampleCount pos = 0;
    bool error = false;
    int outGenerated = 0;
    const auto numSamples = GetNumSamples();

    // These sequences are appended to below
    auto newSequences = GetEmptySequenceCopies();

    /**
     * We want to keep going as long as we have something to feed the resampler
     * with OR as long as the resampler spews out samples (which could continue
     * for a few iterations after we stop feeding it)
     */
    while (pos < numSamples || outGenerated > 0) {
        const auto inLen = limitSampleBufferSize(bufsize, numSamples - pos);

        bool isLast = ((pos + inLen) == numSamples);

        auto ppNewSequence = newSequences.begin();
        std::optional<std::pair<size_t, size_t> > results{};
        size_t nSequence = 0;
        for (auto& pSequence : mSequences) {
            auto& pNewSequence = *ppNewSequence++;
            if (
                inLen > 0
                && !pSequence->Get(
                    (samplePtr)inBuffer.get(), floatSample, pos, inLen, true)) {
                error = true;
                break;
            }

            // Expect the same results for all channels, or else fail
            auto newResults = resample[nSequence].Process(factor, inBuffer.get(), inLen,
                                                          isLast, outBuffer.get(), bufsize);
            if (!results) {
                results.emplace(newResults);
            } else if (*results != newResults) {
                error = true;
                break;
            }

            outGenerated = results->second;
            if (outGenerated < 0) {
                error = true;
                break;
            }

            pNewSequence->Append((samplePtr)outBuffer.get(), floatSample,
                                 outGenerated, 1,
                                 widestSampleFormat /* computed samples need dither */
                                 );
            ++nSequence;
        }
        if (results) {
            pos += results->first;
        }

        if (progress) {
            auto updateResult = progress->Poll(
                pos.as_long_long(),
                numSamples.as_long_long()
                );
            error = (updateResult != BasicUI::ProgressResult::Success);
            if (error) {
                throw UserException{};
            }
        }
    }

    if (error) {
        throw SimpleMessageBoxException{
                  ExceptionType::Internal,
                  XO("Resampling failed."),
                  XO("Warning"),
                  "Error:_Resampling"
        };
    } else {
        // Use No-fail-guarantee in these steps
        mSequences = move(newSequences);
        mRate = rate;
        Flush();
        Attachments::ForEach(std::mem_fn(&WaveClipListener::Invalidate));
        MarkChanged();
    }
}

void WaveClip::SetName(const wxString& name)
{
    mName = name;
}

const wxString& WaveClip::GetName() const
{
    return mName;
}

int64_t WaveClip::GetGroupId() const
{
    return mGroupId;
}

void WaveClip::SetGroupId(int64_t id)
{
    mGroupId = id;
}

void WaveClip::SetColor(const wxString& color)
{
    mColor = color;
}

const wxString& WaveClip::GetColor() const
{
    return mColor;
}

sampleCount WaveClip::TimeToSamples(double time) const
{
    return sampleCount(floor(time * mRate / GetStretchRatio() + 0.5));
}

double WaveClip::SamplesToTime(sampleCount s) const noexcept
{
    return s.as_double() * GetStretchRatio() / mRate;
}

double WaveClip::SnapToTrackSample(double t) const noexcept
{
    return std::round(t * mRate) / mRate;
}

void WaveClip::SetSilence(sampleCount offset, sampleCount length)
{
    StrongInvariantScope scope{ *this };
    const auto start = TimeToSamples(mTrimLeft) + offset;
    Transaction transaction{ *this };
    for (auto& pSequence : mSequences) {
        pSequence->SetSilence(start, length);
    }
    transaction.Commit();
    MarkChanged();
}

sampleCount WaveClip::GetSequenceSamplesCount() const
{
    return GetNumSamples() * NChannels();
}

double WaveClip::GetPlayStartTime() const noexcept
{
    return SnapToTrackSample(mSequenceOffset + mTrimLeft);
}

void WaveClip::SetPlayStartTime(double time)
{
    SetSequenceStartTime(time - mTrimLeft);
}

double WaveClip::GetPlayEndTime() const
{
    const auto numSamples = GetNumSamples();
    double maxLen = mSequenceOffset
                    + ((numSamples + GreatestAppendBufferLen()).as_double())
                    * GetStretchRatio() / mRate
                    - mTrimRight;
    // JS: calculated value is not the length;
    // it is a maximum value and can be negative; no clipping to 0
    return SnapToTrackSample(maxLen);
}

double WaveClip::GetPlayDuration() const
{
    return GetPlayEndTime() - GetPlayStartTime();
}

bool WaveClip::IsEmpty() const
{
    return std::floor(GetPlayDuration() * mRate + 0.5) < 2.0;
}

sampleCount WaveClip::GetPlayStartSample() const
{
    return sampleCount { GetPlayStartTime() * mRate + 0.5 };
}

sampleCount WaveClip::GetPlayEndSample() const
{
    return sampleCount { GetPlayEndTime() * mRate + 0.5 };
}

sampleCount WaveClip::GetVisibleSampleCount() const
{
    return GetNumSamples()
           - TimeToSamples(mTrimRight) - TimeToSamples(mTrimLeft);
}

void WaveClip::SetTrimLeft(double trim)
{
    mTrimLeft = std::max(.0, trim);
}

double WaveClip::GetTrimLeft() const noexcept
{
    return mTrimLeft;
}

void WaveClip::SetTrimRight(double trim)
{
    mTrimRight = std::max(.0, trim);
}

double WaveClip::GetTrimRight() const noexcept
{
    return mTrimRight;
}

void WaveClip::TrimLeft(double deltaTime)
{
    SetTrimLeft(mTrimLeft + deltaTime);
}

void WaveClip::TrimRight(double deltaTime)
{
    SetTrimRight(mTrimRight + deltaTime);
}

void WaveClip::TrimQuarternotesFromRight(double quarters)
{
    assert(mRawAudioTempo.has_value());
    if (!mRawAudioTempo.has_value()) {
        return;
    }
    const auto secondsPerQuarter = 60 * GetStretchRatio() / *mRawAudioTempo;
    // MH https://github.com/audacity/audacity/issues/5878: Clip boundaries are
    // quantized to the sample period. Music durations aren't, though.
    // `quarters` was probably chosen such that the clip ends exactly at some
    // musical grid snapping point. However, if we right-trim by `quarters`,
    // the clip's play end time might be rounded up to the next sample period,
    // overlapping the next snapping point on the musical grid. We don't want
    // this, or it would disturb music producers who want to horizontally
    // duplicate loops.
    const auto quantizedTrim
        =std::ceil(quarters * secondsPerQuarter * GetRate()) / GetRate();
    TrimRight(quantizedTrim);
}

void WaveClip::TrimLeftTo(double to)
{
    mTrimLeft
        =std::clamp(to, SnapToTrackSample(mSequenceOffset), GetPlayEndTime())
          - mSequenceOffset;
}

void WaveClip::TrimRightTo(double to)
{
    const auto endTime = SnapToTrackSample(GetSequenceEndTime());
    mTrimRight = endTime - std::clamp(to, GetPlayStartTime(), endTime);
}

double WaveClip::GetSequenceStartTime() const noexcept
{
    // JS: mSequenceOffset is the minimum value and it is returned; no clipping to 0
    // Do we need to `SnapToTrackSample` before returning?
    return mSequenceOffset;
}

void WaveClip::SetSequenceStartTime(double startTime)
{
    mSequenceOffset = startTime;
    mEnvelope->SetOffset(startTime);
}

double WaveClip::GetSequenceEndTime() const
{
    const auto numSamples = GetNumSamples();
    double maxLen = GetSequenceStartTime()
                    + numSamples.as_double() * GetStretchRatio() / mRate;
    return maxLen;
}

sampleCount WaveClip::GetSequenceStartSample() const
{
    return TimeToSamples(mSequenceOffset);
}

void WaveClip::ShiftBy(double delta) noexcept
{
    SetSequenceStartTime(GetSequenceStartTime() + delta);
    MarkChanged();
}

bool WaveClip::SplitsPlayRegion(double t) const
{
    return GetPlayStartTime() < t && t < GetPlayEndTime();
}

bool WaveClip::WithinPlayRegion(double t) const
{
    return GetPlayStartTime() <= t && t < GetPlayEndTime();
}

bool WaveClip::EntirelyWithinPlayRegion(double t0, double t1) const
{
    assert(t0 <= t1);
    // t1 is the open end of the interval, hence it's ok if it's equal to the
    // open end of the play region.
    return !BeforePlayRegion(t0) && t1 <= GetPlayEndTime();
}

bool WaveClip::PartlyWithinPlayRegion(double t0, double t1) const
{
    assert(t0 <= t1);
    return WithinPlayRegion(t0) != WithinPlayRegion(t1);
}

bool WaveClip::IntersectsPlayRegion(double t0, double t1) const
{
    assert(t0 <= t1);
    // t1 is the open end of the interval, so it must be excluded from the closed
    // begin of the play region.
    return !muse::RealIsEqualOrMore(t0, GetPlayEndTime()) && !muse::RealIsEqualOrMore(GetPlayStartTime(), t1);
}

bool WaveClip::CoversEntirePlayRegion(double t0, double t1) const
{
    assert(t0 <= t1);
    return t0 <= GetPlayStartTime() && GetPlayEndTime() <= t1;
}

bool WaveClip::BeforePlayRegion(double t) const
{
    return t < GetPlayStartTime();
}

bool WaveClip::AtOrBeforePlayRegion(double t) const
{
    return t <= GetPlayStartTime();
}

bool WaveClip::AfterPlayRegion(double t) const
{
    return GetPlayEndTime() <= t;
}

sampleCount WaveClip::CountSamples(double t0, double t1) const
{
    if (t0 < t1) {
        t0 = std::max(t0, GetPlayStartTime());
        t1 = std::min(t1, GetPlayEndTime());
        const auto s0 = TimeToSamples(t0 - GetPlayStartTime());
        const auto s1 = TimeToSamples(t1 - GetPlayStartTime());
        return s1 - s0;
    }
    return { 0 };
}

sampleCount WaveClip::TimeToSequenceSamples(double t) const
{
    if (t < GetSequenceStartTime()) {
        return 0;
    } else if (t > GetSequenceEndTime()) {
        return GetNumSamples();
    }
    return TimeToSamples(t - GetSequenceStartTime());
}

bool WaveClip::CheckInvariants() const
{
    const auto width = NChannels();
    auto iter = mSequences.begin(),
         end = mSequences.end();
    // There must be at least one pointer
    if (iter != end) {
        // All pointers mut be non-null
        auto& pFirst = *iter++;
        if (pFirst) {
            // All sequences must have the same sample formats, and sample block
            // factory
            return
                std::all_of(iter, end, [&](decltype(pFirst) pSequence) {
                return pSequence
                       && pSequence->GetSampleFormats() == pFirst->GetSampleFormats()
                       && pSequence->GetFactory() == pFirst->GetFactory();
            })
                &&// All cut lines are non-null, satisfy the invariants, and match width
                std::all_of(mCutLines.begin(), mCutLines.end(),
                            [width](const WaveClipHolder& pCutLine) {
                if (!(pCutLine && pCutLine->NChannels() == width)) {
                    return false;
                }
                if (!pCutLine->StrongInvariant()) {
                    pCutLine->AssertOrRepairStrongInvariant();
                    return false;
                }
                return true;
            });
        }
    }
    return false;
}

bool WaveClip::StrongInvariant() const
{
    if (!CheckInvariants()) {
        return false;
    }
    const auto width = NChannels();
    auto iter = mSequences.begin(),
         end = mSequences.end();
    assert(iter != end); // because CheckInvariants is true
    auto& pFirst = *iter++;
    assert(pFirst); // likewise
    // All sequences must have the same lengths
    return all_of(iter, end, [&](decltype(pFirst) pSequence) {
        assert(pSequence); // likewise
        return pSequence->GetNumSamples() == pFirst->GetNumSamples();
    });
}

void WaveClip::AssertOrRepairStrongInvariant()
{
    if (!StrongInvariant()) {
        assert(false);
        RepairChannels();
        assert(StrongInvariant());
    }
}

WaveClip::StrongInvariantScope::StrongInvariantScope(WaveClip& clip)
    : mClip{clip}
{
    mClip.AssertOrRepairStrongInvariant();
}

WaveClip::StrongInvariantScope::~StrongInvariantScope()
{
    assert(mClip.StrongInvariant());
}

WaveClip::Transaction::Transaction(WaveClip& clip)
    : clip{clip}
    , mTrimLeft{clip.mTrimLeft}
    , mTrimRight{clip.mTrimRight}
{
    sequences.reserve(clip.mSequences.size());
    auto& factory = clip.GetFactory();
    for (auto& pSequence : clip.mSequences) {
        sequences.push_back(
            //! Does not copy un-flushed append buffer data
            std::make_unique<Sequence>(*pSequence, factory));
    }
}

WaveClip::Transaction::~Transaction()
{
    if (!committed) {
        clip.mSequences.swap(sequences);
        clip.mTrimLeft = mTrimLeft;
        clip.mTrimRight = mTrimRight;
    }
}
